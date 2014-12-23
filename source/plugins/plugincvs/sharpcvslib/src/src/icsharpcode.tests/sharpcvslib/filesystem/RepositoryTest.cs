#region "Copyright"
// Copyright (C) 2003 Clayton Harbour
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// As a special exception, the copyright holders of this library give you
// permission to link this library with independent modules to produce an
// executable, regardless of the license terms of these independent
// modules, and to copy and distribute the resulting executable under
// terms of your choice, provided that you also meet, for each linked
// independent module, the terms and conditions of the license of that
// module.  An independent module is a module which is not derived from
// or based on this library.  If you modify this library, you may extend
// this exception to your version of the library, but you are not
// obligated to do so.  If you do not wish to do so, delete this
// exception statement from your version.
//    Author: Clayton Harbour
//     claytonharbour@sporadicism.com
#endregion

using System;
using System.Collections;
using System.IO;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Misc;

using ICSharpCode.SharpCvsLib.Tests;
using ICSharpCode.SharpCvsLib.Tests.Config;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.FileSystem {
    /// <summary>
    ///     Test the repository file parses the input string correctly
    ///         and assigns the correct values to the properties.
    /// </summary>
    [TestFixture]
    public class RepositoryTest {
        private SharpCvsLibTestsConfig settings = 
            SharpCvsLibTestsConfig.GetInstance();

        /// <summary>
        /// The module name is the top level directory in the repository  folder,
        ///     in our examples it is sharpcvslib.
        /// </summary>
        private readonly String MODULE_NAME = "sharpcvslib";
        private readonly String RELATIVE_PATH = "src";
        private readonly String REPOSITORY_ENTRY1 = "sharpcvslib/src";
        private readonly String REPOSITORY_ENTRY2 = "sharpcvslib/doc";
        private readonly String REPOSITORY_FILE_NAME = "Repository";
        /// <summary>
        ///     Constructor for test case.
        /// </summary>
        public RepositoryTest () {

        }

        /// <summary>
        ///     Ensure that the values the repository is initialized with
        ///         can be determined.
        /// </summary>
        [Test]
        public void CreateRepositoryTest () {
            String fullPath =
                Path.Combine (this.settings.Config.LocalPath, RELATIVE_PATH);
            Repository repos = new Repository (fullPath,
                                            this.REPOSITORY_ENTRY1);

            String cvsPath = Path.Combine (fullPath, "CVS");
            Assert.AreEqual (fullPath, repos.ParentDir.FullName);
            Assert.AreEqual (this.REPOSITORY_ENTRY1, repos.FileContents);
            Assert.AreEqual (this.REPOSITORY_FILE_NAME, repos.Filename);
            Assert.AreEqual (Factory.FileType.Repository, repos.Type);
            Assert.AreEqual (false, repos.IsMultiLined);
            Assert.AreEqual(MODULE_NAME, repos.ModuleName);
        }

        /// <summary>
        ///     Test that the equals method correctly identifies two repository objects
        ///         as equal.
        /// </summary>
        [Test]
        public void EqualsTest () {
            String cvsPath = Path.Combine (this.settings.Config.LocalPath,
                                        MODULE_NAME);
            Repository reposSame1 = new Repository (cvsPath, this.REPOSITORY_ENTRY1);
            Repository reposSame2 = new Repository (cvsPath, this.REPOSITORY_ENTRY1);
            Repository reposDiff1 = new Repository (cvsPath, this.REPOSITORY_ENTRY2);

            Assert.AreEqual (reposSame1, reposSame1);
            Assert.AreEqual (reposSame2, reposSame1);
            Assert.AreEqual (reposSame1, reposSame2);

            Assert.IsTrue (!reposDiff1.Equals (reposSame1));
            Assert.IsTrue (!reposDiff1.Equals (reposSame2));
            Assert.IsTrue (!reposSame1.Equals (reposDiff1));
            Assert.IsTrue (!reposSame2.Equals (reposDiff1));

            Assert.AreEqual(MODULE_NAME, reposSame1.ModuleName);
            Assert.AreEqual(MODULE_NAME, reposSame2.ModuleName);
            Assert.AreEqual(MODULE_NAME, reposDiff1.ModuleName);
        }

        /// <summary>
        /// The slashes in a cvs repository file are stripped off.  This
        ///     means that the repository + the entry from the entries file
        ///     equal the relative server path for the file in the repository
        ///     when the two are concatentated.
        /// </summary>
        [Test]
        public void NoSlashAtEnd () {
            String fullPath =
                Path.Combine (this.settings.Config.LocalPath, this.settings.Config.Module);

            String repositoryEntryWithSlash = this.REPOSITORY_ENTRY1 + "/";

            Assert.IsTrue (repositoryEntryWithSlash.EndsWith ("/"));
            Repository repos = new Repository (fullPath,
                                            repositoryEntryWithSlash);

            Assert.IsTrue (!repos.FileContents.EndsWith ("/"));
            Assert.AreEqual ("sharpcvslib/src", repos.FileContents);

            Assert.AreEqual(MODULE_NAME, repos.ModuleName);
        }

        /// <summary>
        ///     Clean up any test directories, etc.
        /// </summary>
        [TearDown]
        public void TearDown () {
            if (Directory.Exists (this.settings.Config.LocalPath)) {
                Directory.Delete (this.settings.Config.LocalPath, true);
            }
        }
    }
}
