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
#endregion

using System;
using System.Collections;
using System.IO;

using ICSharpCode.SharpCvsLib.Tests;
using ICSharpCode.SharpCvsLib.Tests.Config;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.FileSystem {
    /// <summary>
    /// A cvs entry can contain the following items:
    ///     / name / version / conflict / options / tag_or_date
    ///
    /// Sharpcvslib converts each cvs entry into an object so the data
    ///     can be accessed easier.  This class tests the parsing of the
    ///     cvs string and other behavoir related to this entry.
    ///
    /// </summary>
    [TestFixture]
    public class EntryTest : AbstractTest {
        private ILog LOGGER =
            LogManager.GetLogger (typeof(EntryTest));

        private SharpCvsLibTestsConfig settings = 
            SharpCvsLibTestsConfig.GetInstance();

        /// <summary>
        ///     Test entry 1: Standard checkout file.
        /// </summary>
        public const String CHECKOUT_ENTRY =
            "/CvsFileManagerTest.cs/1.1/Tue May 13 05:10:17 2003//";
        /// <summary>
        /// Test entry filename 1.
        /// </summary>
        public const String CHECKOUT_ENTRY_FILENAME = "CvsFileManagerTest.cs";
        /// <summary>
        ///     Test entry 2: Date in RFC1123 format.
        /// </summary>
        public const String CHECKOUT_ENTRY_2 =
            "/EntryTest.cs/1.1/03 Jan 2003 04:07:36 -0000//";
        /// <summary>
        /// Test entry filename 2.
        /// </summary>
        public const String CHECKOUT_ENTRY_2_FILENAME = "EntryTest.cs";
//        private const String NORMALISED_ENTRY_2 =
//            "/EntryTest.cs/1.1/Fri Jan 3 04:07:36 2003//";
        // added another space between month and day because tortoisecvs was indicating
        //  files that were not modified were in fact modified.
        private const String NORMALISED_ENTRY_2 =
            "/EntryTest.cs/1.1/Fri Jan  3 04:07:36 2003//";

        private const String NORMALISED_ENTRY_2_FILENAME = "EntryTest.cs";
        /// <summary>
        ///     Test entry 3: Checkout file with conflict, binary flag and tag.
        /// </summary>
        public const String CHECKOUT_ENTRY_3 =
            "/ICSharpCode.SharpZipLib.dll/1.2/Sat Jun 21 03:22:02 2003+Sat Jun 21 03:22:03 2003/-kb/TV1.0";
        /// <summary>
        /// Test entry 3 filename.
        /// </summary>
        public const String CHECKOUT_ENTRY_3_FILENAME = "ICSharpCode.SharpZipLib.dll";
        /// <summary>
        ///     Test entry 4: Subdirectory.
        /// </summary>
        public const String DIR_ENTRY =
            "D/ICSharpCode.Tests////";
        /// <summary>
        ///     Test entry 4 directory name.
        /// </summary>
        public const String DIR_ENTRY_DIRNAME = "ICSharpCode.Tests";
        /// <summary>
        ///     Test entry 5: Too many arguments in entry.
        /// </summary>
        public const String INVALID_ENTRY_1 =
            "/CvsFileManagerTest.cs/1.1/Tue May 13 05:10:17 2003///";
        /// <summary>
        ///     Test entry 6: Not enough arguments in entry.
        /// </summary>
        public const String INVALID_ENTRY_2 =
            "/CvsFileManagerTest.cs/1.1/Tue May 13 05:10:17 2003/";
        /// <summary>
        ///     Test entry 7: Invalid date in entry.
        /// </summary>
        public const String INVALID_ENTRY_3 =
            "/CvsFileManagerTest.cs/1.1/Result of merge//";

        private readonly String ENTRY_FILE_NAME = "Entries";

        private Manager manager;
        /// <summary>
        /// Constructor for customer db test.
        /// </summary>
        public EntryTest () {
            this.manager = new Manager (Path.Combine(this.settings.LocalPath, this.settings.Module));
        }

        /// <summary>
        ///
        /// The items that should be parsed out of the cvs string are:
        ///         <ol>
        ///             <li>name</li>
        ///             <li>version</li>
        ///             <li>conflict</li>
        ///             <li>options</li>
        ///             <li>tag or date</li>
        ///         </ol>
        /// </summary>
        [Test]
        public void TestParseCheckoutEntry () {
            Entry entry = new Entry (this.settings.Config.LocalPath, CHECKOUT_ENTRY);

            Assert.AreEqual (this.settings.Config.LocalPath, entry.Path);
            Assert.AreEqual (Path.Combine (this.settings.Config.LocalPath, CHECKOUT_ENTRY_FILENAME), 
                entry.FullPath);
            Assert.IsTrue (entry.Filename.Equals (ENTRY_FILE_NAME));

            Assert.AreEqual ("CvsFileManagerTest.cs", entry.Name);
            Assert.AreEqual ("1.1", entry.Revision);
            Assert.AreEqual ("Tue May 13 05:10:17 2003", entry.Date);
            Assert.IsTrue (entry.Conflict == null);
            Assert.IsTrue (entry.Options.Length == 0);
            Assert.IsTrue (entry.Tag.Length == 0);

            Assert.IsTrue (entry.TimeStamp.Day == 13);
            Assert.IsTrue (entry.TimeStamp.Month == 5);
            Assert.IsTrue (entry.TimeStamp.Year == 2003);
            Assert.IsTrue (entry.TimeStamp.Hour == 5);
            Assert.IsTrue (entry.TimeStamp.Minute == 10);
            Assert.IsTrue (entry.TimeStamp.Second == 17);

            Assert.IsTrue (entry.IsBinaryFile == false);
            Assert.IsTrue (entry.IsDirectory == false);

            Assert.IsTrue (entry.FileContents.Equals (CHECKOUT_ENTRY));
        }

        /// <summary>
        /// Test parsing of the RFC1123 date format.
        /// </summary>
        [Test]
        public void TestParseRfc1123Entry () {
            Entry entry = new Entry (this.settings.Config.LocalPath, CHECKOUT_ENTRY_2);

            Assert.AreEqual (this.settings.Config.LocalPath, entry.Path);
            Assert.AreEqual (Path.Combine(this.settings.Config.LocalPath, CHECKOUT_ENTRY_2_FILENAME), 
                entry.FullPath);
            Assert.AreEqual (Path.Combine(this.settings.Config.LocalPath, entry.Name),
                entry.FullPath);
            Assert.AreEqual (ENTRY_FILE_NAME, entry.Filename);

            Assert.AreEqual ("EntryTest.cs", entry.Name);
            Assert.AreEqual ("1.1", entry.Revision);
            // TODO: check what format the date should come back in
            //Assert.IsTrue (entry.Date, entry.Date.Equals ("Fri Jan 03 04:07:36"));
            Assert.AreEqual ("03 Jan 2003 04:07:36 -0000", entry.Date);
            Assert.AreEqual (null, entry.Conflict);
            Assert.AreEqual (0, entry.Options.Length);
            Assert.AreEqual (0, entry.Tag.Length);

            Assert.AreEqual (3, entry.TimeStamp.Day);
            Assert.AreEqual (1, entry.TimeStamp.Month);
            Assert.AreEqual (2003, entry.TimeStamp.Year);
            Assert.AreEqual (4, entry.TimeStamp.Hour);
            Assert.AreEqual (7, entry.TimeStamp.Minute);
            Assert.AreEqual (36, entry.TimeStamp.Second);

            Assert.AreEqual (entry.IsBinaryFile, false);
            Assert.AreEqual (entry.IsDirectory, false);

            Assert.AreEqual (NORMALISED_ENTRY_2, entry.FileContents);
        }

        /// <summary>
        /// Test parsing of an entry with conflict, options and the tag field populated.
        /// </summary>
        [Test]
        public void TestParseFullCheckoutEntry () {
            Entry entry = new Entry (this.settings.Config.LocalPath, CHECKOUT_ENTRY_3);

            Assert.IsTrue (entry.Name.Equals ("ICSharpCode.SharpZipLib.dll"));
            Assert.IsTrue (entry.FullPath.Equals (
                Path.Combine(this.settings.Config.LocalPath, CHECKOUT_ENTRY_3_FILENAME)));
            Assert.IsTrue (entry.Revision.Equals ("1.2"));
            Assert.IsTrue (entry.Date.Equals ("Sat Jun 21 03:22:02 2003"));
            Assert.IsTrue (entry.Conflict.Equals ("Sat Jun 21 03:22:03 2003"));
            Assert.IsTrue (entry.Options.Equals ("-kb"));
            Assert.IsTrue (entry.Tag.Equals ("TV1.0"));

            Assert.IsTrue (entry.IsBinaryFile == true);
            Assert.IsTrue (entry.IsDirectory == false);

            Assert.IsTrue (entry.FileContents.Equals (CHECKOUT_ENTRY_3));
        }

        /// <summary>
        /// Test parsing of a sub-directory entry.
        /// The items that should be parsed out of the cvs string are:
        ///         <ol>
        ///             <li>name</li>
        ///         </ol>
        /// </summary>
        [Test]
        public void TestParseDirEntry () {
            Entry entry = new Entry (this.settings.Config.LocalPath, DIR_ENTRY);
            Assert.AreEqual(Path.Combine(this.settings.Config.LocalPath, DIR_ENTRY_DIRNAME),
                entry.FullPath);
            Assert.IsTrue (entry.IsDirectory == true);

        }

        /// <summary>
        /// Test too many args.
        /// </summary>
        [Test]
        [ExpectedException(typeof(EntryParseException))]
        public void TestTooManyArgsEntry () {
            Entry entry = new Entry (this.settings.Config.LocalPath, INVALID_ENTRY_1);
        }

        /// <summary>
        /// Test that valid cvs root paths pass.  Note that a root can contain cvs directories.
        /// </summary>
        [Test]
        public void TestValidRootPaths () {
            Entry entry = new Entry("c:/cvs/nant", CHECKOUT_ENTRY);

        }
    }
}

