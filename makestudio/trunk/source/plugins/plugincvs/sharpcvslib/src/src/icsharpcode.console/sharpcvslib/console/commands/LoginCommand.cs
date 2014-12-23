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
using System.Text;
using System.Text.RegularExpressions;

using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Console;
using ICSharpCode.SharpCvsLib.Console.Parser;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Protocols;

using log4net;

namespace ICSharpCode.SharpCvsLib.Console.Commands {

    /// <summary>
    /// Login to a cvs repository.
    /// </summary>
    public class LoginCommand : ICommand {
        private CvsRoot cvsRoot;
        private string password;
        private  WorkingDirectory workingDirectory;

        /// <summary>Regular expression to parse out the password prompt from the commandline</summary>
        public const string REGEX_PASSWORD = @"[-]*pwd:([^\s]*)";

        private const string OPT_PASSWORD = "pwd";
        private string[] args;

        private readonly ILog LOGGER = 
            LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

        private ConsoleWriter writer;

        private ConsoleWriter Writer {
            get {
                if (null == this.writer) {
                    this.writer = new ConsoleWriter();
                }
                return this.writer;
            }
        }

        /// <summary>
        /// Commandline arguments.
        /// </summary>
        public string[] Args {
            get {return this.args;}
            set {this.args = value;}
        }

        /// <summary>
        /// Get the command line arguments as a string.
        /// </summary>
        public string CommandLine {
            get {
                StringBuilder msg = new StringBuilder ();
                foreach (string arg in this.args) {
                    msg.Append(String.Format("{0} ", arg));
                }
                return msg.ToString();
            }
        }

        /// <summary>
        /// The text value of the password that will be used to login.  This should be
        ///     translated into one of the public API command objects.
        /// </summary>
        public CvsRoot CvsRoot {
            get {return this.cvsRoot;}
        }

        /// <summary>
        /// Get the password.
        /// </summary>
        public string Password {
            get {return this.password;}
            set {this.password = value;}
        }

        public WorkingDirectory CurrentWorkingDirectory {
            get {
                if (null == this.workingDirectory) {
                    this.workingDirectory =new WorkingDirectory(this.CvsRoot,
                        Environment.CurrentDirectory, this.CvsRoot.CvsRepository);
                }
                return this.workingDirectory;
            }
        }

        /// <summary>
        /// Login to a cvs repository.
        /// </summary>
        /// <param name="cvsRoot">User information</param>
        public LoginCommand(string cvsRoot) : this(new CvsRoot(cvsRoot)) {
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="cvsRoot"></param>
        public LoginCommand (CvsRoot cvsRoot) {
            this.cvsRoot = cvsRoot;
        }

        /// <summary>
        /// Login to a cvs repository with workDirectory object
        /// </summary>
        /// <param name="cvsRoot">The repository root.</param>
        /// <param name="workingDirectory">User information</param>
        public LoginCommand(CvsRoot cvsRoot, WorkingDirectory workingDirectory){
            this.cvsRoot = cvsRoot;
            this.workingDirectory = workingDirectory;
            // Is there a password file?
            //     yes, get password for this username
            //     no, prompt user for password to use
        }

        /// <summary>
        /// Process the login command with cvs library API calls.
        /// </summary>
        public void Execute () {
            if (null != this.CvsRoot && this.CvsRoot.TransportProtocol != 
                ICSharpCode.SharpCvsLib.Misc.CvsRoot.ProtocolType.pserver) {
                LOGGER.Debug(string.Format("cvs [login aborted]: The :{0}: protocol does not support the login command",
                    this.CvsRoot.Protocol));
                return;
            }

            CVSServerConnection serverConn = 
                new CVSServerConnection(CurrentWorkingDirectory);
            try {
                serverConn.Connect(CurrentWorkingDirectory, password);
            } catch (ICSharpCode.SharpCvsLib.Exceptions.AuthenticationException) {
                try {
                    this.password = 
                        PServerProtocol.PromptForPassword(this.CvsRoot.ToString());
                    if (this.password == null) {
                        this.password = string.Empty;
                    }
                    Manager manager = new Manager(this.workingDirectory.LocalDirectory);
                    manager.UpdatePassFile(this.password, this.CvsRoot);

                    serverConn.Connect(CurrentWorkingDirectory, password);
                } catch (ICSharpCode.SharpCvsLib.Exceptions.AuthenticationException e) {
                    ConsoleMain.ExitProgram(e.Message);
                }
            }
        }

        /// <summary>
        /// Process the login command with cvs library API calls.
        /// </summary>
        public void Execute (ICommandConnection connection) {
            this.Execute();
        }
    }
}
