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
//    <author>Clayton Harbour</author>
#endregion

using System;
using System.IO;

using NUnit.Framework;

using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Config;
using ICSharpCode.SharpCvsLib.Exceptions;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Tests.Config;

using log4net;

// TODO: Change to internalize helpers (remove)
[assembly: log4net.Config.XmlConfigurator(
ConfigFileExtension="config", Watch=true)]

namespace ICSharpCode.SharpCvsLib.Tests {
    /// <summary>
    /// Abstract test is used to perform common setup and teardown routines for
    ///     tests.
    /// </summary>
    public class AbstractTest {
        private SharpCvsLibTestsConfig settings = 
            SharpCvsLibTestsConfig.GetInstance();
        private readonly ILog LOGGER = LogManager.GetLogger(typeof(AbstractTest));

        /// <summary>
        /// Settings that are used for the tests.
        /// </summary>
        public SharpCvsLibTestsConfig Settings {
            get {return this.settings;}
        }

        /// <summary>
        /// Create a new instance of the abstract test.
        /// </summary>
        public AbstractTest() {
        }

        /// <summary>
        /// Perform arbitrary setup routines, such as creating any temporary directories
        ///     that are needed for the tests.
        /// </summary>
        [SetUp]
        public virtual void SetUp () {
            LOGGER.Debug("Test settings: " + this.Settings);
            LOGGER.Debug("Application settings: " + SharpCvsLibConfig.GetInstance());
            this.GetTempPath();
        }

        /// <summary>
        /// Perform arbitrary teardown routines such as removing any temporary directories
        ///     that are needed for the test.
        /// </summary>
        [TearDown]
        public virtual void TearDown() {
//            this.CleanTempDirectory();            
        }

        /// <summary>
        ///     Check if the temporary directory exists.  If it does then
        ///         remove the directory.
        /// </summary>
        protected void CleanTempDirectory () {
            if (Directory.Exists(this.settings.Config.LocalPath)) {
                try {
                    Directory.Delete (this.settings.Config.LocalPath, true);
                } catch (IOException) {
                    Environment.CurrentDirectory = 
                        Path.Combine(this.settings.Config.LocalPath, ".." + Path.DirectorySeparatorChar);
                    Directory.Delete (this.settings.Config.LocalPath, true);
                }
            }
        }

        /// <summary>
        /// Get a temp path that does not care about isolating the path for the 
        ///     specific test case.
        /// </summary>
        /// <returns>A temp path that is not isolated.</returns>
        public String GetTempPath () {
            String tempPath = this.Settings.Config.LocalPath;
            this.CreateDirectory(tempPath);
            return tempPath;
        }

        /// <summary>
        /// Get a temporary path that appends the type of the class to the test path
        ///     so we can determine which tests has which output.
        /// </summary>
        /// <param name="typeOfTest">The type of the calling test.</param>
        /// <returns></returns>
        public String GetTempPath (Type typeOfTest) {
            String tempPath = this.Settings.Config.LocalPath;
            tempPath = Path.Combine (tempPath, typeOfTest.Name);

            this.CreateDirectory(tempPath);

            return tempPath;
        }

        private void CreateDirectory (String path) {
            if (!Directory.Exists(path)) {
                Directory.CreateDirectory(path);
            }
        }

        /// <summary>
        /// Checkout the test repository.  This repository is configured in the
        ///     ICSharpConfig.SharpCvsLib.Tests.dll.config file, or if this file
        ///     cannot be found or loaded then the settings are loaded from
        ///     TestConstants class.
        /// </summary>
        public void CheckoutTestModule () {
            CvsRoot root = new CvsRoot (this.settings.Config.Cvsroot);
            WorkingDirectory working =
                new WorkingDirectory (root,
                this.settings.Config.LocalPath,
                this.settings.Config.Module);

            System.Console.WriteLine (this.settings.Config.LocalPath);

            CVSServerConnection connection = new CVSServerConnection ();
            Assert.IsNotNull(connection, "Should have a connection object.");

            ICommand command = new CheckoutModuleCommand (working);
            Assert.IsNotNull (command, "Should have a command object.");

            try {
                connection.Connect (working, this.settings.Config.ValidPassword);
            } catch (AuthenticationException) {
                Assert.IsTrue (true, "Failed to authenticate with server.");
            }

            command.Execute (connection);
            connection.Close ();
        }

    }
}
