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

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Console.Parser;

using ICSharpCode.SharpCvsLib.Tests.Config;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Console {
    /// <summary>
    ///     Test the command line args parameters for valid ones
    ///         and test invalid ones.
    /// </summary>
    [TestFixture]
    public class MainTest {
        private ILog LOGGER = LogManager.GetLogger (typeof (MainTest));
        private SharpCvsLibTestsConfig settings = 
            SharpCvsLibTestsConfig.GetInstance();

        private const int DEFAULT_WAIT_TIME = 100;
        private String buildDir;
        /// <summary>
        ///     Constructory for test case.
        /// </summary>
        public MainTest () {
            buildDir = System.AppDomain.CurrentDomain.BaseDirectory;
        }

        /// <summary>
        ///     Ensure valid help parameter processing.
        ///
        /// </summary>
        [Test]
        public void GenericHelpTest ()
        {
            // Get Executable name
            String filename = Path.Combine (buildDir, "cvs.exe");

            LOGGER.Debug ("buildDir=[" + buildDir + "]");
            LOGGER.Debug ("filename=[" + filename + "]");

            // Create new process
            ProcessStartInfo cvsProcessInfo = new ProcessStartInfo(filename, "--help");
            cvsProcessInfo.UseShellExecute = false;
            cvsProcessInfo.RedirectStandardOutput = true;
            cvsProcessInfo.CreateNoWindow = true;

            // Run the process
            Process cvsProcess = new Process();
            cvsProcess.StartInfo = cvsProcessInfo;
            cvsProcess.Start();
            string output = cvsProcess.StandardOutput.ReadToEnd();
            cvsProcess.WaitForExit(DEFAULT_WAIT_TIME);

            // Check the results of process output
            Assert.AreEqual (Usage.General + "\r\n", output);
        }
        /// <summary>
        ///     Ensure valid option help parameter processing.
        ///
        /// </summary>
        [Test]
        public void OptionHelpTest ()
        {
            // Get Executable name
            String filename = Path.Combine (buildDir, "cvs.exe");
            // Add Help option parameter to executable name
            // Create new process
            ProcessStartInfo cvsProcessInfo = new ProcessStartInfo(filename, "--help-options");
            cvsProcessInfo.UseShellExecute = false;
            cvsProcessInfo.RedirectStandardOutput = true;
            cvsProcessInfo.CreateNoWindow = true;

            // Run the process
            Process cvsProcess = new Process();
            cvsProcess.StartInfo = cvsProcessInfo;
            cvsProcess.Start();
            string output = cvsProcess.StandardOutput.ReadToEnd();
            cvsProcess.WaitForExit(DEFAULT_WAIT_TIME);

            // Check the results of process output
            Assert.AreEqual (Usage.Options + "\r\n", output);

        }		/// <summary>
        ///     Ensure valid commands help parameter processing.
        ///
        /// </summary>
        [Test]
        public void CommandsHelpTest ()
        {
            // Test Commands help parameter
            // Get Executable name
            String filename = Path.Combine (buildDir, "cvs.exe");

            // Create new process
            ProcessStartInfo cvsProcessInfo = new ProcessStartInfo(filename, "--help-commands");
            cvsProcessInfo.UseShellExecute = false;
            cvsProcessInfo.RedirectStandardOutput = true;
            cvsProcessInfo.CreateNoWindow = true;

            // Run the process
            Process cvsProcess = new Process();
            cvsProcess.StartInfo = cvsProcessInfo;
            cvsProcess.Start();
            string output = cvsProcess.StandardOutput.ReadToEnd();
            cvsProcess.WaitForExit(DEFAULT_WAIT_TIME);

            // Check the results of process output
            Assert.AreEqual (Usage.Commands + "\r\n", output);
        }
        /// <summary>
        ///     Ensure valid synonyms help parameter processing.
        ///
        /// </summary>
        [Test]
        public void SynonymsHelpTest ()
        {
            // Get Executable name
            String filename = Path.Combine (buildDir, "cvs.exe");

            // Create new process
            ProcessStartInfo cvsProcessInfo = new ProcessStartInfo(filename, "--help-synonyms");
            cvsProcessInfo.UseShellExecute = false;
            cvsProcessInfo.RedirectStandardOutput = true;
            cvsProcessInfo.CreateNoWindow = true;

            // Run the process
            Process cvsProcess = new Process();
            cvsProcess.StartInfo = cvsProcessInfo;
            cvsProcess.Start();
            string output = cvsProcess.StandardOutput.ReadToEnd();
            cvsProcess.WaitForExit(DEFAULT_WAIT_TIME);

            // Check the results of process output
            Assert.AreEqual (Usage.Synonyms + "\r\n", output);
        }
        /// <summary>
        ///     Ensure invalid help parameter processing.
        ///
        /// </summary>
        [Test]
        public void BadHelpTest ()
        {
            // Get Executable name
            String filename = Path.Combine (buildDir, "cvs.exe");

            // Create new process
            ProcessStartInfo cvsProcessInfo = new ProcessStartInfo(filename, "--help-bad");
            cvsProcessInfo.UseShellExecute = false;
            cvsProcessInfo.RedirectStandardOutput = true;
            cvsProcessInfo.CreateNoWindow = true;

            // Run the process
            Process cvsProcess = new Process();
            cvsProcess.StartInfo = cvsProcessInfo;
            cvsProcess.Start();
            string output = cvsProcess.StandardOutput.ReadToEnd();
            cvsProcess.WaitForExit(DEFAULT_WAIT_TIME);

            // Check the results of process output
            Assert.AreEqual (Usage.General + "\r\n", output);
        }
        /// <summary>
        ///     Ensure no parameters processing.
        ///
        /// </summary>
        [Test]
        public void NoParamTest ()
        {
            // Get Executable name
            String filename = Path.Combine (buildDir, "cvs.exe");

            // Create new process
            ProcessStartInfo cvsProcessInfo = new ProcessStartInfo(filename);
            cvsProcessInfo.UseShellExecute = false;
            cvsProcessInfo.RedirectStandardOutput = true;
            cvsProcessInfo.CreateNoWindow = true;

            // Run the process
            Process cvsProcess = new Process();
            cvsProcess.StartInfo = cvsProcessInfo;
            cvsProcess.Start();
            string output = cvsProcess.StandardOutput.ReadToEnd();
            cvsProcess.WaitForExit(DEFAULT_WAIT_TIME);

            // Check the results of process output
            Assert.AreEqual (Usage.General + "\r\n", output);
        }
    }
}
