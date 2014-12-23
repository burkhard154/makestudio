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
#endregion
using System;
using System.Collections;
using System.IO;
using System.Diagnostics;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Misc;

using ICSharpCode.SharpCvsLib.Tests.Config;
using ICSharpCode.SharpCvsLib.Console.Parser;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Console.Commands {
    
    /// <summary>
    ///     Test the Add command object for valid ones
    ///         and test invalid ones.
    /// </summary>
    [TestFixture]
    public class AddCommandTest {

        private SharpCvsLibTestsConfig settings = SharpCvsLibTestsConfig.GetInstance();
        private readonly ILog LOGGER = LogManager.GetLogger(typeof(AddCommandTest));
        /// <summary>
        ///     Constructory for test case.
        /// </summary>
        public AddCommandTest () {
        }

        /// <summary>
        /// Test the Add Command.
        ///
        /// </summary>
        [Test]
        public void MakeAddCommandTest () {
            String fullPath = null;
            Directory.CreateDirectory( settings.Config.LocalPath);
            Environment.CurrentDirectory = settings.Config.LocalPath;
            // add file TargetFile
            // commit file TargetFile
            // remove file TargetFile
            // commit remove TargetFile
            String[] files = Directory.GetFiles(Path.Combine(Environment.CurrentDirectory, 
                settings.Config.Module), settings.Config.TargetFile);
            Assert.IsTrue(files.Length > 0);

            foreach (String file in files) {
                LOGGER.Debug("file=[" + file + "]");
                // Remove the .txt when everything works, giving me bugs...
                String newFileName = Guid.NewGuid().ToString() + ".txt";
                fullPath = Path.Combine(Path.Combine(Environment.CurrentDirectory, settings.Config.Module),newFileName);
                File.Copy (file, fullPath);
            }
            String commandLine = "-d" + settings.Config.Cvsroot + " add " + fullPath;
            String [] commandLineArgs = commandLine.Split(' ');
            // Create consoleMain object to test the Add command
            ConsoleMain consoleMain = new ConsoleMain();
            Assert.IsNotNull (consoleMain);
            
            consoleMain.Execute(commandLineArgs);
            Assert.IsTrue(File.Exists(fullPath));
        }
    }
}
