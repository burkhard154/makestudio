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

using ICSharpCode.SharpCvsLib.Tests.Config;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.FileSystem {
    /// <summary>
    ///     Test the tag file and its properties.
    /// </summary>
    [TestFixture]
    public class TagTest {
        private SharpCvsLibTestsConfig settings = 
            SharpCvsLibTestsConfig.GetInstance();

        private readonly String RELATIVE_PATH = "src";
        private readonly String TAG_ENTRY1 =
            "TVer1.1";
        private readonly String TAG_ENTRY2 =
            "D15 sep 2003 10:05:17";
        private readonly String TAG_FILE_NAME = "Tag";
        /// <summary>
        ///     Constructor for test case.
        /// </summary>
        public TagTest () {

        }

        /// <summary>
        ///     Ensure that the values the tag is initialized with
        ///         can be determined.
        /// </summary>
        [Test]
        public void CreateTagTest () {
            String fullPath =
                Path.Combine (this.settings.Config.LocalPath, RELATIVE_PATH);
            Tag tag = new Tag(fullPath,
                this.TAG_ENTRY1);

            String cvsPath = Path.Combine (fullPath, "CVS");
            Assert.IsNotNull (tag.ParentDir.FullName);
            Assert.AreEqual (fullPath, tag.ParentDir.FullName);
            Assert.IsNotNull (tag.FileContents);
            Assert.AreEqual ("N" + this.TAG_ENTRY1.Substring (1), tag.FileContents);
            Assert.AreEqual (this.TAG_FILE_NAME, tag.Filename);
            Assert.AreEqual (Factory.FileType.Tag, tag.Type);
            Assert.AreEqual (false, tag.IsMultiLined);
        }

        /// <summary>
        ///     Test that the equals method correctly identifies two tag objects
        ///         as equal.
        /// </summary>
        [Test]
        public void EqualsTest () {
            String cvsPath = Path.Combine (this.settings.Config.LocalPath,
                                        this.settings.Config.Module);
            Tag tagSame1 = new Tag (cvsPath, this.TAG_ENTRY1);
            Tag tagSame2 = new Tag (cvsPath, this.TAG_ENTRY1);
            Tag tagDiff1 = new Tag (cvsPath, this.TAG_ENTRY2);

            Assert.AreEqual (tagSame1, tagSame1);
            Assert.AreEqual (tagSame2, tagSame1);
            Assert.AreEqual (tagSame1, tagSame2);

            Assert.IsTrue (!tagDiff1.Equals (tagSame1));
            Assert.IsTrue (!tagDiff1.Equals (tagSame2));
            Assert.IsTrue (!tagSame1.Equals (tagDiff1));
            Assert.IsTrue (!tagSame2.Equals (tagDiff1));
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
