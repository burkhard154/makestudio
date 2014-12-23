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
//    <credit>Credit to Dick Grune, Vrije Universiteit, Amsterdam, for writing
//    the shell-script CVS system that this is based on.  In addition credit
//    to Brian Berliner and Jeff Polk for their work on the cvsnt port of
//    this work. </credit>
//    <author>Steve Kenzell</author>
//    <author>Clayton Harbour</author>
#endregion

using System;
using System.IO;
using System.Text;
using System.Xml;

using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Exceptions;
using ICSharpCode.SharpCvsLib.Misc;

using ICSharpCode.SharpCvsLib.Console.Commands;
using ICSharpCode.SharpCvsLib.Console.Parser;
using ICSharpCode.SharpCvsLib.Messages;

using log4net;
using log4net.Config;

namespace ICSharpCode.SharpCvsLib.Console {

    /// <summary>The main driver/ entry point into the program.</summary>
    [Serializable]
    public class ConsoleMain {
        private ILog LOGGER;

        private string[] _args;

        private const string DEFAULT_CONFIG = 
            @"<?xml version='1.0' encoding='utf-8'?>
<configuration>
    <configSections>
        <section name='log4net' type='log4net.Config.Log4NetConfigurationSectionHandler,log4net'/>
    </configSections>       
    <log4net debug='false'>
        <appender name='ConsoleAppender' type='log4net.Appender.ConsoleAppender'>
            <layout type='log4net.Layout.PatternLayout'>
                <param name='ConversionPattern' value='[%c{2}:%m  - [%x] &lt;%X{auth}&gt;]%n' />
            </layout>
        </appender>
        <appender name='RollingLogFileAppender' type='log4net.Appender.RollingFileAppender'>
            <param name='File' value='cvs.log' />
            <param name='AppendToFile' value='true' />
            <param name='MaxSizeRollBackups' value='10' />
            <param name='MaximumFileSize' value='1000000' />
            <param name='RollingStyle' value='Size' />
            <param name='StaticLogFileName' value='true' />
            <layout type='log4net.Layout.PatternLayout'>
                <param name='Header' value='[Header]\r\n' />
                <param name='Footer' value='[Footer]\r\n' />
                <param name='ConversionPattern' value='%d [%t] %-5p %c [%x] - %m%n' />
            </layout>
        </appender>
        <root>
            <level value='INFO' />
            <!--<appender-ref ref='RollingLogFileAppender' />-->
            <appender-ref ref='ConsoleAppender' />
        </root>
    </log4net>
</configuration>
";

        private static string DefaultConfig {
            get {return DEFAULT_CONFIG.Replace("'", "\"");}
        }

        /// <summary>
        /// Command line arguments.
        /// </summary>
        public string[] Args {
            get {return this._args;}
            set {this._args = value;}
        } 

        /// <summary>
        /// Initialize the logger class.
        /// </summary>
        public static void InitLog4net () {
            try {
                FileInfo configFile = new FileInfo(
                    Path.Combine(Path.GetTempPath(), "ICSharpCode.Console" + ".dll.config"));
                if (!configFile.Exists) {
                    StreamWriter writer = null;
                    try {
                        writer = configFile.CreateText();
                        writer.Write(DefaultConfig);
                    } finally {
                        if (null != writer) {
                            writer.Close();
                        }
                    }
                }
                log4net.Config.XmlConfigurator.Configure(configFile);
            } catch (Exception) {
                BasicConfigurator.Configure();
            }
        }

        /// <summary>Constructor.
        ///     TODO: Fill in more of a usage/ explanation.</summary>
        public ConsoleMain () {
            try {
                LOGGER = log4net.LogManager.GetLogger(typeof(ConsoleMain));
            } finally {
                // do nothing
            }
        }

        private static MemoryStream WriteDefaultConfig () {
            byte[] data = new byte[DefaultConfig.Length];
            MemoryStream stream = new MemoryStream(data, 0, DefaultConfig.Length);;
            return stream;
        }

        private ConsoleWriter writer;
        private ConsoleWriter Writer {
            get {
                if (null == this.writer) {
                    writer = new ConsoleWriter();
                }
                return this.writer;}
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="args"></param>
        public void Execute(string[] args) {
            this.Args = args;
            this.Execute();
        }

        /// <summary>
        /// Driver for console application.
        ///
        /// TODO: Write a better description :-)
        /// </summary>
        public void Execute () {
            try {
                this.DoExecute();
            } catch (Exception e) {
                string msg = 
                    String.Format("Something very bad has happened ( {0} ).", 
                    e.Message);
                ExitProgram(msg, e);
            }
        }

        private void DoExecute() {
            string[] args = this._args;
            CommandLineParser parser = new CommandLineParser (args);

            ICommand command = null;
            try {
                command = parser.Execute ();
            } catch (CommandLineParseException e) {
                Writer.WriteLine(
                    String.Format("{0}{1}{2}",
                        Usage.General, Environment.NewLine, e.Message));
                return;
            } catch (Exception e) {
                ExitProgram("Exception parsing command.", e);
            }

            if (null != command) {
                // might need to move this up to the library, make working
                //  directory a public property??  Not sure.
                WorkingDirectory workingDirectory = parser.CurrentWorkingDirectory;

                string password = this.GetPassword(parser, workingDirectory);

                // Create CVSServerConnection object that has the ICommandConnection
                CVSServerConnection serverConn = new CVSServerConnection(workingDirectory);

                if (parser.Verbose) {
                    serverConn.RequestMessageEvent += 
                        new MessageEventHandler(Writer.WriteLine);
                    serverConn.ResponseMessageEvent += 
                        new MessageEventHandler(Writer.WriteLine);
                }

                serverConn.StartProcessEvent += 
                    new ProcessEventHandler(Writer.StartProcess);
                serverConn.StopProcessEvent += 
                    new ProcessEventHandler(Writer.StopProcess);
                serverConn.ResponseMessageEvents.UpdatedResponseMessageEvent += 
                    new MessageEventHandler(Writer.WriteLine);
                serverConn.ResponseMessageEvents.ClearStaticDirectoryResponseMessageEvent += 
                    new MessageEventHandler(Writer.WriteLine);
                serverConn.ResponseMessageEvents.SetStaticDirectoryResponseMessageEvent += 
                    new MessageEventHandler(Writer.WriteLine);
                serverConn.ResponseMessageEvents.ErrorResponseMessageEvent += 
                    new MessageEventHandler(Writer.WriteError);
                serverConn.ResponseMessageEvents.ListResponseMessageEvent +=
                    new MessageEventHandler(Writer.WriteLine);

                if (null == serverConn) {
                    string msg = "Unable to connect to server.";
                    ExitProgram(msg);
                }

                try{
                    // try connecting with empty password for anonymous users
                    serverConn.Connect(workingDirectory, password);
                } catch (AuthenticationException e){
                    string msg = String.Format("Fatal error, aborting.  cvs [login aborted]: {0}: unknown user or bad password.",
                        workingDirectory.CvsRoot.User);
                    ExitProgram(msg, e);
                } catch (Exception ex) {
                    string msg = String.Format("Fatal cvs error ( {0} ).",
                        ex.Message);
                    ExitProgram(msg, ex);
                }

                // Execute the command on cvs repository.
                command.Execute(serverConn);
                serverConn.Close();
            }
        }

        public static void ExitProgram (string msg, Exception exception) {
#if (DEBUG)
            ExitProgram(string.Format("{0}\n{1}", msg, exception.ToString()));
#else
            ExitProgram(string.Format("{0}", msg));
#endif
        }

        /// <summary>
        /// Exit the program and display the given exit message.
        /// </summary>
        /// <param name="msg"></param>
        public static void ExitProgram (string msg) {
            ConsoleWriter writer = new ConsoleWriter();
            writer.WriteLine(msg);
#if (DEBUG)
            System.Console.ReadLine();
#endif
            Environment.Exit(-1);
        }

        private string GetPassword(CommandLineParser parser, WorkingDirectory workingDir) {
            string pwd = null;
            if (null != parser && null != parser.Password &&
				parser.Password.Length != 0) {
                pwd = parser.Password;
            } else {
                LoginCommand loginCommand = new LoginCommand(workingDir.CvsRoot);
                loginCommand.Execute();
                pwd = loginCommand.Password;
            }

            if (null == pwd) {
                pwd = String.Empty;
            }

            return pwd;
        }
    }
}
