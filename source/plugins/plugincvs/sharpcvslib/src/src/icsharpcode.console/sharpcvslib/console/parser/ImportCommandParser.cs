#region "Copyright"
//
// Copyright (C) 2004 Clayton Harbour
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
using System.Collections;
using System.Globalization;
using System.IO;
using System.Text;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Console.Parser;
using ICSharpCode.SharpCvsLib.FileSystem;

using log4net;

namespace ICSharpCode.SharpCvsLib.Console.Parser {

    /// <summary>
    /// Initialize the cvs repository.
    /// </summary>
    public class ImportCommandParser : AbstractCommandParser {
        private CvsRoot cvsRoot;
        private string[] unparsedOptions;
        private string repository;

        private string message;
        private string vendor;
        private string release;
        private string branch;

        /// <summary>
        /// Default constructor.
        /// </summary>
        public ImportCommandParser () {

        }

        /// <summary>
        /// Initialize a cvs command.
        /// </summary>
        /// <param name="cvsroot">User information</param>
        /// <param name="args">Commandline arguments.</param>
        public ImportCommandParser(string cvsroot, string[] args) : 
            this(new CvsRoot(cvsroot), args){
        }

        /// <summary>
        /// Initialize a cvs repository
        /// </summary>
        /// <param name="cvsroot">User Information</param>
        /// <param name="args">Commandline arguments.</param>
        public ImportCommandParser(CvsRoot cvsroot, string[] args) {
            this.cvsRoot = cvsroot;
            this.unparsedOptions = args;
        }

        /// <summary>
        /// Create a new instance of the <see cref="CheckoutCommandParser"/>.
        /// </summary>
        /// <returns></returns>
        public static ICommandParser GetInstance() {
            return GetInstance(typeof(InitCommandParser));
        }

        /// <summary>
        /// Name of the command being parsed.
        /// </summary>
        public override string CommandName {
            get {return "import";}
        }

        /// <summary>
        /// Nicknames for the add command.
        /// </summary>
        public override ICollection Nicks {
            get {
                if (commandNicks.Count == 0) {
                    commandNicks.Add("import");
                    commandNicks.Add("im");
                    commandNicks.Add("imp");
                }

                return commandNicks;
            }
        }

        /// <summary>
        /// Description of the command.
        /// </summary>
        public override string CommandDescription {
            get {return "Import sources into CVS, using vendor branches";}
        }

        /// <summary>
        /// Create the command object that will be used to act on the repository.
        /// </summary>
        /// <returns>The command object that will be used to act on the
        ///     repository.</returns>
        /// <exception cref="Exception">TODO: Make a more specific exception</exception>
        /// <exception cref="NotImplementedException">If the command argument
        ///     is not implemented currently.  TODO: Implement the argument.</exception>
        public override ICommand CreateCommand () {
            ICSharpCode.SharpCvsLib.Commands.ImportModuleCommand importCommand;
            try {
                this.ParseOptions(this.unparsedOptions);
                CurrentWorkingDirectory = new WorkingDirectory( this.cvsRoot,
                    Environment.CurrentDirectory, this.repository);
                Manager manager = new Manager(Environment.CurrentDirectory);

                DirectoryInfo importDir = new DirectoryInfo(Environment.CurrentDirectory);

                if (!importDir.Exists) {
                    ConsoleMain.ExitProgram("Import directory does not exist.");
                }

                CurrentWorkingDirectory.Folders = 
                    manager.FetchFilesToAdd(importDir.FullName);
                importCommand = 
                    new ICSharpCode.SharpCvsLib.Commands.ImportModuleCommand(
                    CurrentWorkingDirectory, this.message);

                importCommand.VendorString = this.vendor;
                importCommand.ReleaseString = this.release;
                importCommand.LogMessage = this.message;
//                importCommand.Repository = this.repository;
            }
            catch (Exception e) {
                LOGGER.Error (e);
                throw e;
            }
            return importCommand;
        }

        public override void ParseOptions() {
            this.ParseOptions(this.Args);
        }

        /// <summary>
        /// Parse the command line options/ arguments and populate the command
        ///     object with the arguments.
        /// </summary>
        /// <param name="arguments">A string value that holds the command
        ///     line options the user has selected.</param>
        private void ParseOptions (string[] arguments) {
            string singleOptions = "Cdfn";
            string options = string.Empty;
            int i = 0;
            // get rest of arguments which is options on the checkout command.
            while (arguments.Length > i && arguments[i].Trim().IndexOf("-") == 0){
                // Get options with second parameters?
                string arg = arguments[i].Trim();
                if (arg.IndexOfAny( singleOptions.ToCharArray(), 1, 1) >= 0){
                    switch (arg) {
                        case "-C":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-d":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-f":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-n":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        default:
                            break;
                    }
                } else {
                    switch (arg) {
                        case "-k":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-I":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-b":
                            this.branch = arguments[++i];
                            break;
                        case "-m":
                            this.message = arguments[++i];
                            break;
                        case "-W":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        default:
                            break;
                    }
                }
                i++;
            }
            if (arguments.Length > i){
                // Safely grab the module, if not specified then
                //  pass null into the repository...the cvs command
                //  line for cvsnt/ cvs seems to bomb out when
                //  it sends to the server
                this.repository = arguments[i++];
            } else {
                this.repository = String.Empty;
            }

            if (arguments.Length > i){
                this.vendor = arguments[i++];
            } else {
                this.vendor = String.Empty;
            }
            if (arguments.Length > i){
                this.release = arguments[i++];
            } else {
                this.release = String.Empty;
            }
        }

        /// <summary>
        /// Output the command usage and arguements.
        /// </summary>
        public override string Usage {
            get {
                string usage =
@"Usage: cvs import [-C] [-d] [-f] [-k subst] [-I ign] [-m msg] [-b branch]
    [-W spec] [-n] repository [vendor-tag] [release-tags...]
        -C      Create CVS directories while importing.
        -d      Use the file's modification time as the time of import.
        -f      Overwrite existing release tags.
        -k sub  Set default RCS keyword substitution mode.
        -I ign  More files to ignore (! to reset).
        -b bra  Vendor branch id.
        -m msg  Log message.
        -W spec Wrappers specification line.
        -n      Don't create vendor branch or release tags.
(Specify the --help global option for a list of other help options)";
                return usage;
            }
        }
    }
}