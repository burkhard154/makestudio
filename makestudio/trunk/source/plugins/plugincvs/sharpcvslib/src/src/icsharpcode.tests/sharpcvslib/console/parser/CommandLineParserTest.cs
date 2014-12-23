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
using ICSharpCode.SharpCvsLib.Commands;

using ICSharpCode.SharpCvsLib.Console.Parser;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Console.Parser {
    /// <summary>
    ///     Test the command line args parameters for valid ones
    ///         and test invalid ones.
    /// </summary>
    [TestFixture]
    public class CommandLineParserTest{
        private ILog LOGGER = LogManager.GetLogger(typeof (CommandLineParserTest));
        /// <summary>
        ///     Constructory for test case.
        /// </summary>
        public CommandLineParserTest (){
        }

        /// <summary>
        /// Test a CommandLineParser object is created successfully.
        ///
        /// </summary>
        [Test]
        public void MakeCommandParserTest (){
            String[] args = {"--help"};
            // Test Creating a CommandLineParser object
            CommandLineParser newCommandLineParser = new CommandLineParser( args);
            Assert.IsNotNull (newCommandLineParser);
            LOGGER.Debug("Before parse execute for MakeCommandParserTest.");
            newCommandLineParser.Execute();
        }

        /// <summary>
        /// Test that the -d option parameter is connected to the cvsroot, to keep
        ///     the implementation similar to cvs/ cvsnt.
        ///
        ///     Correct
        ///     <code>
        ///         cvs -d:pserver:anonymous@cvs.sf.net:/cvsroot/sharpcvslib login
        ///     </code>
        ///     Incorrect:
        ///     <code>
        ///         cvs -d :pserver:anonymous:cvs.sf.net:/cvsroot/sharpcvslib login
        ///     </code>
        /// </summary>
        [Test]
        public void MinusDOptionConnectedToCvsRoot () {
            try {
                String[] args = {"-d :pserver:anonymous@cvs.sf.net:/cvsroot/sharpcvslib", "login"};
                CommandLineParser parser = new CommandLineParser (args);

                parser.Execute ();
            } catch (Exception e){
                Assert.Fail(string.Format("Should not have an exception.  Had: {0}", e.ToString()));
            }
            try {
                String[] args = {"-d:pserver:anonymous@cvs.sf.net:/cvsroot/sharpcvslib", "login"};
                CommandLineParser parser = new CommandLineParser (args);

                parser.Execute();
            } catch (Exception e){
                Assert.Fail(string.Format("Should not have an exception.  Had: {0}", e.ToString()));
            } 
        }
        /// <summary>
        /// Test the options are parsed correctly and added to the Options property.
        /// </summary>
        [Test]
        public void ParseOptions () {
            String commandLine = "-d:pserver:anonymous@cvs.sf.net:/cvsroot/sharpcvslib co -r v0_3_1 -d newLocation sharpcvslib";
            String[] args = commandLine.Split(' ');
            CommandLineParser parser = new CommandLineParser (args);
            try {
                LOGGER.Debug("Before execute ParseOptions.");
                ICommand command = parser.Execute ();

                Assert.IsTrue(command.GetType() == typeof(CheckoutModuleCommand));

                CheckoutModuleCommand co = (CheckoutModuleCommand)command;
                
                Assert.AreEqual("v0_3_1", co.Revision);
                Assert.AreEqual("newlocation", co.OverrideDirectory);

            } 
            catch (Exception e) {
                LOGGER.Error(e);
                throw e;
            }
        }
    }
}
