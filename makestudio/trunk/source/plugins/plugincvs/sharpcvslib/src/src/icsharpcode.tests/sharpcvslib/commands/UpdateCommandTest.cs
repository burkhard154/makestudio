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

using ICSharpCode.SharpCvsLib.Tests;
using ICSharpCode.SharpCvsLib.Tests.Config;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Commands {
    /// <summary>
    ///     Tests that if a file is removed it will be pulled back down
    ///         with an update.
    /// </summary>
    [TestFixture]
    public class UpdateCommandTest : AbstractTest	{
        private ILog LOGGER =
            LogManager.GetLogger (typeof(CheckoutModuleCommandTest));

        private SharpCvsLibTestsConfig settings = 
            SharpCvsLibTestsConfig.GetInstance();

        String rootDir;
        String checkFile;
        String moduleDir;

        private Manager manager;
        /// <summary>
        /// Constructor for customer db test.
        /// </summary>
        public UpdateCommandTest () {
            this.moduleDir = this.settings.Config.Module;
            this.rootDir =
                Path.Combine (this.settings.Config.LocalPath, this.moduleDir);
            this.checkFile =
                Path.Combine (rootDir, this.settings.Config.TargetFile);
        }

        /// <summary>Wrapper for the checkout command test checkout method.</summary>
        private void Checkout () {
            this.Checkout (null);
        }

        /// <summary>Wrapper for the checkout command test checkout method.</summary>
        private void Checkout (String revision) {
            this.Checkout (revision, null);
        }

        private void Checkout (String revision, String overrideDirectory) {
            if (null != overrideDirectory) {
                this.manager = new Manager (Path.Combine(this.settings.LocalPath, overrideDirectory));
            } else {
                this.manager = new Manager (Path.Combine(this.settings.LocalPath, this.settings.Module));
            }
            CheckoutModuleCommandTest checkout =
                new CheckoutModuleCommandTest ();
            checkout.Checkout (revision, overrideDirectory);
        }

        /// <summary>
        ///     Test that the update command brings the check file back
        ///         down after it is deleted.
        /// </summary>
        [Test]
        public void UpdateTest () {
            this.Checkout ();
            File.Delete (checkFile);
            LOGGER.Debug("After deleted checkFile=[" + checkFile + "]");

            Assert.IsTrue (!File.Exists (checkFile));
            this.UpdateAllRecursive (rootDir);
            Assert.IsTrue (File.Exists (checkFile));

            String moduleDir = Path.Combine(rootDir, this.settings.Module);
            Entries entries =
                this.manager.FetchEntries(moduleDir + Path.DirectorySeparatorChar.ToString());

            String[] files =
                Directory.GetFiles (rootDir);
            String[] directories =
                Directory.GetDirectories (rootDir);
            // Minus the cvs directory
            int total = files.Length + directories.Length - 1;
            Assert.AreEqual (total, entries.Count);
            LOGGER.Debug("Before checking all file names.");

            Assert.IsTrue(entries.Contains(Path.Combine(moduleDir, this.settings.TargetFile)));

            // Had some problems with an extra module directory appearing under
            //    the main working folder.
            String doubleModuleDir = Path.Combine (rootDir, this.settings.Config.Module);
            Assert.IsTrue (!Directory.Exists (doubleModuleDir));
        }

        private void UpdateAllRecursive (String rootDir, String overrideDirectory) {
            CvsRoot root = new CvsRoot (this.settings.Config.Cvsroot);
            WorkingDirectory working =
                new WorkingDirectory (root,
                                    this.settings.Config.LocalPath,
                                    this.settings.Config.Module);

            working.OverrideDirectory = overrideDirectory;

            CVSServerConnection connection = new CVSServerConnection ();
            Assert.IsNotNull (connection);

            ICommand command = new UpdateCommand2 (working);
            Assert.IsNotNull (command);

            connection.Connect (working, this.settings.Config.ValidPassword);

            // Update all files...
            LOGGER.Debug ("Fetching all files from rootDir=[" + rootDir + "]");
            working.FoldersToUpdate =
                this.manager.FetchFilesToUpdate (rootDir);

            command.Execute (connection);
            connection.Close ();
        }

        private void UpdateAllRecursive (String rootDir) {
            this.UpdateAllRecursive (rootDir, null);
        }

        /// <summary>
        ///     Test that a directory checked out as a revision is updated
        ///         successfully.
        /// </summary>
        [Test]
        public void UpdateRevisionTest () {
            this.Checkout (this.settings.Config.Tag1);
            this.UpdateAllRecursive (this.rootDir);

            CheckoutModuleCommandTest.AssertFileContentsEqualString (this.checkFile, this.settings.Config.Content1);
        }

        /// <summary>
        ///     Test that a directory checked out with an override directory is
        ///         updated correctly.
        /// </summary>
        [Test]
        public void UpdateOverrideDirectoryTest () {
            this.moduleDir = this.settings.Config.OverrideDirectory;
            this.rootDir =
                Path.Combine (this.settings.Config.LocalPath, this.moduleDir);
            this.checkFile =
                Path.Combine (rootDir, this.settings.Config.TargetFile);

            this.Checkout (null, this.settings.Config.OverrideDirectory);
            File.Delete (checkFile);

            Assert.IsTrue (!File.Exists (checkFile));
            this.UpdateAllRecursive (rootDir, this.settings.Config.OverrideDirectory);
            Assert.IsTrue (File.Exists (checkFile));

        }
    }
}
