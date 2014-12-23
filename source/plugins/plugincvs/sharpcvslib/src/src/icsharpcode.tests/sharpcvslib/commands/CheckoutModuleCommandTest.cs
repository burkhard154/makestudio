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
//    Author:    Clayton Harbour
#endregion

using System;
using System.Collections;
using System.IO;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Exceptions;

using ICSharpCode.SharpCvsLib.Tests;
using ICSharpCode.SharpCvsLib.Tests.Config;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Commands {
    /// <summary>
    ///     Test that checkout module command fetches files from the
    ///         remote repository.  All files are not verified, only
    ///         a select few are checked.  After the files are checked
    ///         the entries are verified in the <code>CVS/Entries</code>
    ///         folder.
    /// </summary>
    [TestFixture]
    public class CheckoutModuleCommandTest : AbstractTest  {
        string rootDir;
        string checkFile;

        private SharpCvsLibTestsConfig settings = 
            SharpCvsLibTestsConfig.GetInstance();

        Manager manager;

        private ILog LOGGER =
            LogManager.GetLogger (typeof(CheckoutModuleCommandTest));

        /// <summary>
        /// Constructor for customer db test.
        /// </summary>
        public CheckoutModuleCommandTest () {
            this.rootDir =
                Path.Combine (this.settings.Config.LocalPath,
                this.settings.Config.Module);
            this.checkFile =
                Path.Combine (rootDir, this.settings.Config.TargetFile);
            this.manager = new Manager (rootDir);
        }

        /// <summary>
        ///     Test that a checkout with all parameters is successful.
        /// </summary>
        [Test]
        public void CheckoutTest () {
            this.Checkout ();

            Assert.IsTrue (File.Exists (checkFile), "Should have found the check file.  file=[" +
                            checkFile + "]");

            Entries entries =
                manager.FetchEntries (rootDir);

            String moduleDir = Path.Combine(this.settings.LocalPath, this.settings.Module);
            Assert.IsTrue (entries.Contains(Path.Combine(moduleDir, checkFile)), "Build file should have a cvs entry.");
            Assert.IsTrue (entries.Contains(Path.Combine(moduleDir, "src" + Path.DirectorySeparatorChar.ToString())),
                this.settings.Config.TargetDirectory + " directory should have a cvs entry.");
            foreach (DictionaryEntry dicEntry in entries) {
                Entry entry = (Entry)dicEntry.Value;
                if (!entry.IsDirectory) {
                    Assert.IsNotNull(entry.Date, "Should have date information.");
                }
            }
            Assert.IsTrue (!Directory.Exists (Path.Combine (this.settings.Config.LocalPath, manager.CVS)), 
                "Should not have a cvs directory above module path.");
            Assert.IsTrue (!Directory.Exists (Path.Combine (this.settings.Config.Module, manager.CVS)), 
                "Should not have a cvs directory in the current execution path.  ");


        }

        /// <summary>
        ///     Test that specifying a revision produces a checkout of the specific
        ///     revision tag and creates a tag file in the cvs folder.
        /// </summary>
        [Test]
        public void CheckoutRevisionTest_Revision_1 () {
            this.CheckoutRevisionTest (this.settings.Config.Tag1,
                                    this.settings.Config.Content1);
        }

        /// <summary>
        ///     Test that specifying a revision produces a checkout of the specific
        ///     revision tag and creates a tag file in the cvs folder.
        /// </summary>
        [Test]
        public void CheckoutRevisionTest_Revision_2 () {
            this.CheckoutRevisionTest (this.settings.Config.Tag2,
                                    this.settings.Config.Content2);
        }

        /// <summary>
        ///     Test that specifying a revision produces a checkout of the specific
        ///     revision tag and creates a tag file in the cvs folder.
        /// </summary>
        /// <param name="revision">The revision tag to checkout.</param>
        /// <param name="expectedContent">The file contents that are expected.</param>
        private void CheckoutRevisionTest (String revision, String expectedContent) {
            this.Checkout (revision, null);
            Assert.IsTrue (File.Exists (checkFile), "Should have found the check file.  file=[" +
                            checkFile + "]");

            ICvsFile[] entries =
                manager.Fetch (rootDir, Factory.FileType.Entries);
            int foundFileEntry = 0;
            int foundDirectoryEntry = 0;

            foreach (ICvsFile cvsEntry in entries) {
                Entry entry = (Entry)cvsEntry;
                System.Console.WriteLine ("entry=[" + entry + "]");
                if (entry.Name.Equals (this.settings.Config.TargetFile)) {
                    foundFileEntry++;
                }

                if (entry.Name.Equals (this.settings.Config.TargetDirectory)) {
                    foundDirectoryEntry++;
                }
            }

            Assert.IsTrue(foundFileEntry == 1, "Build file should have a cvs entry.");
            Assert.IsTrue (foundDirectoryEntry == 1, this.settings.Config.TargetDirectory + " directory should have a cvs entry.");
            Assert.IsTrue (!Directory.Exists (Path.Combine (this.settings.Config.LocalPath, manager.CVS)),
                            "Should not have a cvs directory above module path.");
            Assert.IsTrue (!Directory.Exists (Path.Combine (this.settings.Config.Module, manager.CVS)),
                            "Should not have a cvs directory in the current execution path.  ");

            String tagFile =
                Path.Combine (Path.Combine (this.settings.Config.Module, manager.CVS), Tag.FILE_NAME);
            Assert.IsTrue (!Directory.Exists (tagFile), 
                            "Should not have a cvs directory in the current execution path.  ");

            AssertFileContentsEqualString (checkFile, expectedContent);
        }

        /// <summary>
        ///     Assert that the expected file contents match the contents actually
        ///         in the given file.
        /// </summary>
        public static void AssertFileContentsEqualString (String filename, String expectedContent) {
            StreamReader reader = new StreamReader (filename);
            String actualContent = reader.ReadToEnd ();

            // Note the read to end method appends a carriage return (^M)/ line feed (^F)
            //    to the string read so this is removed manually:
            actualContent = actualContent.Substring (0, actualContent.Length -2);
            reader.Close ();
            Assert.AreEqual(expectedContent, actualContent,
                "Files should be equal.");
        }


        /// <summary>
        ///     Test that specifying a revision produces a checkout of the specific
        ///     revision tag and creates a tag file in the cvs folder.
        /// </summary>
        [Test]
        public void CheckoutOverrideDirectoryTest () {
            this.rootDir =
                Path.Combine (this.settings.Config.LocalPath, this.settings.Config.OverrideDirectory);
            this.checkFile =
                Path.Combine (rootDir, this.settings.Config.TargetFile);

            this.Checkout (null, this.settings.Config.OverrideDirectory);
            Assert.IsTrue (File.Exists (checkFile), "Should have found the check file.  file=[" +
                            checkFile + "]");

            ICvsFile[] entries =
                manager.Fetch (rootDir, Factory.FileType.Entries);
            int foundFileEntry = 0;
            int foundDirectoryEntry = 0;

            foreach (ICvsFile cvsEntry in entries) {
                Entry entry = (Entry)cvsEntry;
                System.Console.WriteLine ("entry=[" + entry + "]");
                if (entry.Name.Equals (this.settings.Config.TargetFile)) {
                    foundFileEntry++;
                }

                if (entry.Name.Equals (this.settings.Config.TargetDirectory)) {
                    foundDirectoryEntry++;
                }
            }

            Assert.IsTrue (foundFileEntry == 1, "Build file should have a cvs entry.");
            Assert.IsTrue (foundDirectoryEntry == 1, this.settings.Config.TargetDirectory + " directory should have a cvs entry.");
            Assert.IsTrue (!Directory.Exists (Path.Combine (this.settings.Config.LocalPath, manager.CVS)),
                "Should not have a cvs directory above module path.");
            Assert.IsTrue (!Directory.Exists (Path.Combine (this.settings.Config.Module, manager.CVS)),
                "Should not have a cvs directory in the current execution path.  ");

            String tagFile =
                Path.Combine (Path.Combine (this.settings.Config.Module, manager.CVS), Tag.FILE_NAME);
            Assert.IsTrue (!Directory.Exists (tagFile), 
                "Should not have a cvs directory and tag file in the current execution path.  ");
        }

        /// <summary>
        ///     Perform a checkout command.
        /// </summary>
        public void Checkout () {
            this.Checkout (null);
        }

        /// <summary>
        ///     Perform a checkout command using the values in the
        ///         The revision tag
        ///         (if specified) is also used to select the code
        ///         to checkout.
        /// </summary>
        /// <param name="revision">The specific revision of the module
        ///     to checkout from the repository.  If <code>null</code>
        ///     is specified then the default revision, usually the
        ///     <code>HEAD</code> is checked out.</param>
        /// <param name="overrideDirectory">The override directory to
        ///     checkout the repository to.  If <code>null</code>
        ///     is specified then the directory is not overridden
        ///     and the module name is used.</param>
        public void Checkout (String revision, String overrideDirectory) {
            CvsRoot root = new CvsRoot (this.settings.Config.Cvsroot);
            WorkingDirectory working =
                new WorkingDirectory (root,
                                    this.settings.Config.LocalPath,
                                    this.settings.Config.Module);

            System.Console.WriteLine (this.settings.Config.LocalPath);

            working.Revision = revision;
            working.OverrideDirectory = overrideDirectory;

            CVSServerConnection connection = new CVSServerConnection ();
            Assert.IsNotNull (connection, "Should have a connection object.");

            ICommand command = new CheckoutModuleCommand (working);
            Assert.IsNotNull(command, "Should have a command object.");

            try {
                connection.Connect (working, this.settings.Config.ValidPassword);
            } catch (AuthenticationException) {
                Assert.IsTrue(true, "Failed to authenticate with server.");
            }

            command.Execute (connection);
            connection.Close ();
        }

        /// <summary>
        ///     Perform a checkout command.  The revision tag
        ///         (if specified) is also used to select the code
        ///         to checkout.
        /// </summary>
        /// <param name="revision">The specific revision of the module
        ///     to checkout from the repository.  If <code>null</code>
        ///     is specified then the default revision, usually the
        ///     <code>HEAD</code> is checked out.</param>
        public void Checkout (String revision) {
            this.Checkout (revision, null);
        }

    }
}
