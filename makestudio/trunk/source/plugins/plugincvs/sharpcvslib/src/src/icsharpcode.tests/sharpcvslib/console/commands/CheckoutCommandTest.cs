#region "Copyright"
//
// Copyright (C) 2003 Steve Kenzell
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
//    <author>Steve Kenzell</author>
//    <author>Clayton Harbour</author>
#endregion

using System;
using System.Collections;
using System.IO;
using System.Diagnostics;
using System.Text;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Misc;

using ICSharpCode.SharpCvsLib.Tests;
using ICSharpCode.SharpCvsLib.Tests.Config;
using ICSharpCode.SharpCvsLib.Console.Parser;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Console.Commands{
    /// <summary>
    ///     Test the checkout command object for valid ones
    ///         and test invalid ones.
    /// </summary>
    [TestFixture]
    public class CheckoutCommandTest : AbstractTest{

        private SharpCvsLibTestsConfig settings = SharpCvsLibTestsConfig.GetInstance();
        private readonly ILog LOGGER = LogManager.GetLogger(typeof(CheckoutCommandTest));

        /// <summary>
        ///     Constructory for test case.
        /// </summary>
        public CheckoutCommandTest (){
        }

        /// <summary>
        ///     Create a CheckoutCommand object.
        ///
        /// </summary>
        [Test]
        public void MakeCheckoutCommandTest (){
            Directory.CreateDirectory (settings.Config.LocalPath);
            Environment.CurrentDirectory = settings.Config.LocalPath;
            
            String commandLine = 
               "-d" + settings.Config.Cvsroot + " co " + settings.Config.Module;
            String [] commandLineArgs = commandLine.Split(' ');
            // Test Creating a ConsoleMain object
            ConsoleMain consoleMain = new ConsoleMain ();
            consoleMain.Execute (commandLineArgs);
            Assert.IsTrue (Directory.Exists(Path.Combine(settings.Config.LocalPath, settings.Config.Module)));
        }
        /// <summary>
        ///     Checkout files based on revision specified in -r option.
        ///
        /// </summary>
        [Test]
        public void MinusrOptionCheckoutFilesBasedOnRevision (){
            String commandLine = "-d" + settings.Config.Cvsroot + " co -r " + settings.Config.Tag1 +
                                 " " + settings.Config.Module;
            String [] commandLineArgs = commandLine.Split(' ');
            // Test Creating a ConsoleMain object
            ConsoleMain consoleMain = new ConsoleMain();
            consoleMain.Execute (commandLineArgs);
            // check for directory
            Assert.IsTrue (Directory.Exists(Path.Combine(settings.Config.LocalPath, settings.Config.Module)));
            // check for files with specified revision
        }
        /// <summary>
        ///     Checkout files to specified local location instead of current local location
        ///     with the -d option
        /// </summary>
        [Test]
        public void MinusdOptionCheckoutFileIntoDir (){
            String commandLine = "-d" + settings.Config.Cvsroot + " co -d newlocation " + settings.Config.Module;
            String [] commandLineArgs = commandLine.Split(' ');
            // Test Creating a ConsoleMain object
            ConsoleMain consoleMain = new ConsoleMain ();
            Assert.IsNotNull (consoleMain);
            consoleMain.Execute (commandLineArgs);
            Assert.IsTrue(Directory.Exists(Path.Combine(settings.Config.LocalPath, "newlocation")));
        }
        /// <summary>
        ///     Checkout files no earlier than the specified Date 
        ///     with the -D option
        /// </summary>
        [Test]
        public void MinusDOptionCheckoutByCertainDate (){
            String commandLine = "-d" + settings.Config.Cvsroot + " co -D 09.13.03 " + settings.Config.Module;
            String [] commandLineArgs = commandLine.Split(' ');
            // Test Creating a ConsoleMain object
            ConsoleMain consoleMain = new ConsoleMain();
            Assert.IsNotNull (consoleMain);
            consoleMain.Execute( commandLineArgs);
            Assert.IsTrue(Directory.Exists(Path.Combine(settings.Config.LocalPath, settings.Config.Module)));
            // Find a file that should exist 
            //Assertion.Assert ("Should have found the check file.  file=[" +
            //    checkFile + "]", File.Exists (checkFile));

            // Find a file that should not exist
            //Assertion.Assert ("Should have found the check file.  file=[" +
            //    checkFile + "]", File.Exists (checkFile));

        }

        /// <summary>
        /// Test for bug 884798.  This reveals a known issue with multiple options not
        ///     being recognized by the command line client.
        ///     
        /// <see cref="http://sourceforge.net/tracker/index.php?func=detail&aid=884798&group_id=78334&atid=552888"/>
        /// </summary>
        [Test]
        public void TestBug884798_TwoSpaces () {
            Environment.CurrentDirectory = this.GetTempPath();
            String commandLine = 
                "-d:pserver:anonymous@linux.sporadicism.com:/home/cvs/src co -d sharpcvslib-new -r HEAD  sharpcvslib-test-repository";
            // Create new process

            ConsoleMain main = new ConsoleMain();
            try {
                main.Execute(commandLine.Split(' '));
            } catch (Exception e) {
                LOGGER.Error(e);
                Assert.Fail(e.ToString());
            }

            Manager manager = new Manager(this.GetTempPath());
            String modDir = Path.Combine(this.GetTempPath(), this.settings.Config.Module);
            Tag tag = manager.FetchTag(Path.Combine(modDir, "src"));
            Assert.IsNotNull(tag);
            Assert.AreEqual("NHEAD", tag.FileContents);
 
        }

        /// <summary>
        /// Test for bug 884798.  This reveals a known issue with multiple options not
        ///     being recognized by the command line client.
        ///     
        /// <see cref="http://sourceforge.net/tracker/index.php?func=detail&aid=884798&group_id=78334&atid=552888"/>
        /// </summary>
        [Test]
        public void TestBug884798_OneSpace () {
            Environment.CurrentDirectory = this.GetTempPath();
            String commandLine = 
                "-d:pserver:anonymous@linux.sporadicism.com:/home/cvs/src co -d sharpcvslib-new -r HEAD sharpcvslib-test-repository";
            // Create new process

            ConsoleMain main = new ConsoleMain();
            try {
                main.Execute(commandLine.Split(' '));
            } catch (Exception e) {
                LOGGER.Error(e);
                Assert.Fail(e.ToString());
            }
            Manager manager = new Manager(this.GetTempPath());
            String modDir = Path.Combine(this.GetTempPath(), this.settings.Config.Module);
            Tag tag = manager.FetchTag(Path.Combine(modDir, "src"));
            Assert.IsNotNull(tag);
            Assert.AreEqual("NHEAD", tag.FileContents);
        }

    }
}
