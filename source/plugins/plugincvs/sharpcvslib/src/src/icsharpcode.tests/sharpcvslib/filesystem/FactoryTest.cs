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
//
//    Author: Gerald Evans
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
    ///     Test the FileSystem Factory.
    /// </summary>
    [TestFixture]
    public class FactoryTest : AbstractTest {
        private SharpCvsLibTestsConfig settings = 
            SharpCvsLibTestsConfig.GetInstance();

        private const String ENTRY_FILE_NAME = "Entries";
        private const String REPOSITORY_FILE_NAME = "Repository";
        private const String ROOT_FILE_NAME = "Root";
        private const String TAG_FILE_NAME = "Tag";

        private const String ENTRY_NAME_OF_FILE = "CvsFileManagerTest.cs";
        private const String ENTRY_LINE =
            "/CvsFileManagerTest.cs/1.1/Tue May 13 05:10:17 2003//";
        private const String REPOSITORY_LINE = "sharpcvslib/src";
        private const String ROOT_LINE =
            ":pserver:anonymous@cvs.sourceforge.net:/cvsroot/sharpcvslib";
        private const String TAG_LINE =
            "TVer1.1";

        // TODO: Find out if there is any reason why the Factory class
        // does not have static methods
        private Factory factory = new Factory();

        /// <summary>
        ///     Constructor for test case.
        /// </summary>
        public FactoryTest () {

        }

        /// <summary>
        ///     Check factory creation of an Entry.
        /// </summary>
        [Test]
        public void CreateEntryTest () {
            String path = this.settings.Config.LocalPath;

            ICvsFile cvsFile = 
                factory.CreateCvsObject (path, Factory.FileType.Entries.ToString(), ENTRY_LINE);
            Assert.IsTrue (cvsFile is Entry);
            Assert.AreEqual (path, cvsFile.Path);
            Assert.AreEqual (Path.Combine(path, ENTRY_NAME_OF_FILE), cvsFile.FullPath);
            Assert.AreEqual (ENTRY_LINE, cvsFile.FileContents);
        }

        /// <summary>
        ///     Check factory creation of a Repository.
        /// </summary>
        [Test]
        public void CreateRepositoryTest () {
            String fullPath = this.settings.Config.LocalPath;

            ICvsFile cvsFile = factory.CreateCvsObject (fullPath, Repository.FILE_NAME, REPOSITORY_LINE);
            Assert.IsTrue (cvsFile is Repository);
            Assert.AreEqual (fullPath, cvsFile.Path);
            Assert.AreEqual (REPOSITORY_LINE, cvsFile.FileContents);
        }

        /// <summary>
        ///     Check factory creation of a Root.
        /// </summary>
        [Test]
        public void CreateRootTest () {
            String fullPath = this.settings.Config.LocalPath;

            ICvsFile cvsFile = factory.CreateCvsObject (fullPath, Factory.FileType.Root.ToString(), ROOT_LINE);
            Assert.IsTrue (cvsFile is Root);
            Assert.AreEqual(Path.Combine(fullPath, "CVS"), cvsFile.ParentDir.FullName);
            Assert.AreEqual (fullPath, cvsFile.Path);
            Assert.AreEqual (ROOT_LINE, cvsFile.FileContents);
        }

        /// <summary>
        ///     Check factory creation of a Tag.
        /// </summary>
        [Test]
        public void CreateTagTest () {
            String fullPath = this.settings.Config.LocalPath;

            ICvsFile cvsFile = factory.CreateCvsObject (fullPath, Factory.FileType.Tag.ToString(), TAG_LINE);
            Assert.IsTrue (cvsFile is Tag);
            Assert.AreEqual(Path.Combine(fullPath, "CVS"), cvsFile.ParentDir.FullName);
            Assert.AreEqual (fullPath, cvsFile.Path);
            Assert.AreEqual ("N" + TAG_LINE.Substring (1),
                                    cvsFile.FileContents);
        }

        /// <summary>
        ///     Check file type to filename mapping.
        /// </summary>
        [Test]
        public void CheckFilenamesTest () {
            Assert.AreEqual (ENTRY_FILE_NAME, factory.GetFilename (Factory.FileType.Entries));
            Assert.AreEqual (REPOSITORY_FILE_NAME, factory.GetFilename (Factory.FileType.Repository));
            Assert.AreEqual (ROOT_FILE_NAME, factory.GetFilename (Factory.FileType.Root));
            Assert.AreEqual (TAG_FILE_NAME, factory.GetFilename (Factory.FileType.Tag));
        }

        /// <summary>
        ///     Check file type to filename mapping.
        /// </summary>
        [Test]
        public void CheckFileTypesTest () {
            Assert.AreEqual (Factory.FileType.Entries, factory.GetFileType(ENTRY_FILE_NAME));
            Assert.AreEqual (Factory.FileType.Repository, factory.GetFileType(REPOSITORY_FILE_NAME));
            Assert.AreEqual (Factory.FileType.Root, factory.GetFileType(ROOT_FILE_NAME));
            Assert.AreEqual (Factory.FileType.Tag, factory.GetFileType(TAG_FILE_NAME));
        }
    }
}
