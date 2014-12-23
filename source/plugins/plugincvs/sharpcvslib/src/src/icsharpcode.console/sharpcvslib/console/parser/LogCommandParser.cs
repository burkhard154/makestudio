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
    public class LogCommandParser : AbstractCommandParser {
        private CvsRoot cvsRoot;
        private string[] unparsedOptions;
        private string repository;

        private string revision;
        private DateTime date;
        private Folders folders;

        /// <summary>
        /// Default constructor.
        /// </summary>
        public LogCommandParser () {

        }

        /// <summary>
        /// Retrieve the cvs revision history for a file or group of files.
        /// </summary>
        /// <param name="cvsroot">User information</param>
        /// <param name="args">Commandline arguments.</param>
        public LogCommandParser(string cvsroot, string[] args) : 
            this(new CvsRoot(cvsroot), args){
        }

        /// <summary>
        /// Retrieve the cvs revision history for a file or group of files.
        /// </summary>
        /// <param name="cvsroot">User Information</param>
        /// <param name="args">Commandline arguments.</param>
        public LogCommandParser(CvsRoot cvsroot, string[] args) {
            this.cvsRoot = cvsroot;
            this.unparsedOptions = args;
        }

        /// <summary>
        /// Create a new instance of the <see cref="LogCommandParser"/>.
        /// </summary>
        /// <returns></returns>
        public static ICommandParser GetInstance() {
            return GetInstance(typeof(LogCommandParser));
        }

        /// <summary>
        /// Name of the command being parsed.
        /// </summary>
        public override string CommandName {
            get {return "log";}
        }

        /// <summary>
        /// Nicknames for the command.
        /// </summary>
        public override ICollection Nicks {
            get {
                if (commandNicks.Count == 0) {
                    commandNicks.Add("log");
                    commandNicks.Add("lo");
                }

                return commandNicks;
            }
        }

        /// <summary>
        /// Description of the command.
        /// </summary>
        public override string CommandDescription {
            get {return "Print out history information for files";}
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
            ICSharpCode.SharpCvsLib.Commands.LogCommand logCommand;
            DirectoryInfo dir = new DirectoryInfo(Directory.GetCurrentDirectory());

            this.ParseOptions(this.unparsedOptions);
            try {
                Repository repository = Repository.Load(dir);
                if (null == repository || null == repository.FileContents) {
                    throw new CvsFileNotFoundException(
                        string.Format("Valid CVS\\Repository file not found in {0}",
                        dir));
                }
                this.repository = repository.FileContents;
                Root root = Root.Load(dir);
                if (null == root || null == root.FileContents) {
                    throw new CvsFileNotFoundException(
                        string.Format("Valid CVS\\Root file not found in {0}",
                        dir));
                }   
                this.cvsRoot = new CvsRoot(root.FileContents);
            } catch (CvsFileNotFoundException e) {
                LOGGER.Error(e);
                ConsoleMain.ExitProgram("Not a CVS repository.", e);
            }

            CurrentWorkingDirectory = new WorkingDirectory(this.cvsRoot,
                dir.FullName, this.repository);


            logCommand = 
                new ICSharpCode.SharpCvsLib.Commands.LogCommand(
                CurrentWorkingDirectory, folders);

            return logCommand;
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
            string singleOptions = "lRhtNbT";
            string options = string.Empty;
            int i = 0;
            // get rest of arguments which is options on the checkout command.
            while (arguments.Length > i && arguments[i].Trim().IndexOf("-") == 0){
                // Get options with second parameters?
                string arg = arguments[i].Trim();
                if (arg.IndexOfAny( singleOptions.ToCharArray(), 1, 1) >= 0){
                    switch (arg) {
                        case "-l":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-R":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-h":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-t":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-N":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-b":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-T":
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        default:
                            break;
                    }
                } else {
                    switch (arg) {
                        case "-r":
                            this.revision = arguments[++i];
                            break;
                        case "-d":
                            this.date = Convert.ToDateTime(arguments[++i]);
                            break;
                        case "-s":
                            i++;
                            throw new NotImplementedException(string.Format("Argument not implemented {0}.", arg));
                        case "-w":
                            i++;
                            break;
                        default:
                            break;
                    }
                }
                i++;
            }

            // add the current folder by default
            this.folders = new Folders();
            folders.Add(new Folder(new DirectoryInfo(Directory.GetCurrentDirectory())));
            // parse out the file information, skip the command name
            int fileIndex = 0;
            while (arguments.Length > fileIndex && arguments[fileIndex].IndexOf("-") == -1) {
                string file = arguments[fileIndex];
                FileInfo fileInfo = null;
                if (Path.IsPathRooted(file)) {
                    fileInfo = new FileInfo(file);
                } else {
                    fileInfo = new FileInfo(Path.Combine(Directory.GetCurrentDirectory(), file));
                }

                if (!folders.Contains(fileInfo.DirectoryName)) {
                    folders.Add(new Folder(new DirectoryInfo(fileInfo.DirectoryName)));
                }
                Folder folder = folders[fileInfo.DirectoryName];
                Entry entry = Entries.Load(new DirectoryInfo(fileInfo.DirectoryName))[fileInfo.FullName];
                folder.Entries.Add(entry);

                fileIndex++;
            }
        }

        /// <summary>
        /// Output the command usage and arguements.
        /// </summary>
        public override string Usage {
            get {
                string usage =
                    @"Usage: cvs log [-lRhtNbT] [-r[revisions]] [-d dates] [-s states]
    [-w[logins]] [files...]
        -l      Local directory only, no recursion.
        -R      Only print name of RCS file.
        -h      Only print header.
        -t      Only print header and descriptive text.
        -T      Use local time not GMT.
        -S      Supress header information when no revisions are selected.
        -N      Do not list tags.
        -b      Only list revisions on the default branch.
        -r[revisions]   Specify revision(s)s to list.
        -d dates        Specify dates (D1<D2 for range, D for latest before).
        -s states       Only list revisions with specified states.
        -w[logins]      Only list revisions checked in by specified logins.

(Specify the --help global option for a list of other help options)";
                return usage;
            }
        }
    }
}