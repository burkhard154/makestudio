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

using ICSharpCode.SharpCvsLib.Tests.Config;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Commands {
    /// <summary>
    ///     Tests that if a file is removed it will be pulled back down
    ///         with an update.
    /// </summary>
    [TestFixture]
    public class UpdateCommandTestCvsnt	{
        private ILog LOGGER =
            LogManager.GetLogger (typeof(CheckoutModuleCommandTest));

        private SharpCvsLibTestsConfig settings = SharpCvsLibTestsConfig.GetInstance();
        private Manager manager;

        private const String CVSNT_CVSROOT =
            ":pserver:cvs@cvs.cvsnt.org:/usr/local/cvs";
        private const String CVSNT_MODULE =
            "cvsnt/cvsntcpl";

        /// <summary>
        /// Constructor for customer db test.
        /// </summary>
        public UpdateCommandTestCvsnt () {
        }


        /// <summary>
        ///     Checkout the sharpcvslib module so we have something
        ///         to test the update command with.
        /// </summary>
        [SetUp]
        public void SetUp () {
            this.manager = 
                new Manager (Path.Combine(this.settings.LocalPath, CVSNT_MODULE));
        }

        /// <summary>Perform an update of the repository on a directory
        ///     that has not been checked out.</summary>
        [Test]
        public void UpdateNoCheckoutTest () {
            CvsRoot root = new CvsRoot (this.settings.Config.Cvsroot);
            WorkingDirectory working =
                new WorkingDirectory (root,
                                    this.settings.Config.LocalPath,
                                    CVSNT_MODULE);

            CVSServerConnection connection = new CVSServerConnection ();
            Assert.IsNotNull(connection);

            ICommand command = new UpdateCommand2 (working);
            Assert.IsNotNull(command);

            connection.Connect (working, this.settings.Config.ValidPassword);
        }

        /// <summary>Update from a cvsnt repository.
        ///     NOTE: Assumption is made the the cvsnt project is "Eating
        ///     their own dogfood." (i.e. they are using cvsnt for the
        ///     project sources.</summary>
        [Test]
        public void UpdateFromCvsntTest_NoCheckout () {
            this.PerformUpdate ();
        }

        private void PerformUpdate () {
            CvsRoot root = new CvsRoot (CVSNT_CVSROOT);
            WorkingDirectory working =
                new WorkingDirectory (root,
                                    this.settings.Config.LocalPath,
                                    CVSNT_MODULE);

            CVSServerConnection connection = new CVSServerConnection ();
            Assert.IsNotNull(connection);

            ICommand command = new UpdateCommand2 (working);
            Assert.IsNotNull(command);

            connection.Connect (working, this.settings.Config.ValidPassword);
        }

        /// <summary>Checkout the cvsnt project and then update the project
        ///     that has just been checked out.</summary>
        [Test]
        public void UpdateFromCvsntTest_Checkout () {
            string cvsPath =
                Path.Combine (this.settings.Config.LocalPath, CVSNT_MODULE);
            Manager manager = new Manager (cvsPath);

            CvsRoot root = new CvsRoot (CVSNT_CVSROOT);
            WorkingDirectory working =
                new WorkingDirectory (root,
                                    this.settings.Config.LocalPath,
                                    CVSNT_MODULE);

            CVSServerConnection connection = new CVSServerConnection ();
            Assert.IsNotNull (connection);

            ICommand command = new CheckoutModuleCommand (working);
            Assert.IsNotNull (command);

            try {
                connection.Connect (working, this.settings.Config.ValidPassword);
            } catch (AuthenticationException) {
                Assert.IsTrue (true);
            }

            command.Execute (connection);
            connection.Close ();

            this.PerformUpdate ();
        }

    }
}
