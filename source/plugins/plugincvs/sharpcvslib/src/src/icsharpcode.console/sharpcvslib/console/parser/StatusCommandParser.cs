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
using System.Globalization;
using System.Text;
using System.IO;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Console.Parser;

using log4net;

namespace ICSharpCode.SharpCvsLib.Console.Parser {

    /// <summary>
    /// Update modules in the cvs repository.
    /// </summary>
    public class StatusCommandParser : AbstractCommandParser {
        private CvsRoot cvsRootVar;
        private string fileNames;
        private string revision;
        private string localDirectory;
        private DateTime date;
        private string unparsedOptions;
        private string repository;

        private CvsRoot CvsRootVar {
            get { return this.cvsRootVar; }
            set { this.cvsRootVar = value; }
        }

        private void InvalidRepository () {
            System.Console.WriteLine(String.Format("cvs update: No CVSROOT specified!  Please use the `-d' option"));
            System.Console.WriteLine(String.Format("cvs [update aborted]: or set the CVSROOT environment variable."));
            System.Environment.Exit(-1);
        }

        /// <summary>
        /// Create a new instance of the <see cref="UpdateCommandParser"/>.
        /// </summary>
        /// <returns></returns>
        public static ICommandParser GetInstance() {
            return GetInstance(typeof(StatusCommandParser));
        }

        /// <summary>
        /// Name of the command being parsed.
        /// </summary>
        public override string CommandName {
            get {return "status";}
        }

        /// <summary>
        /// Description of the command.
        /// </summary>
        public override string CommandDescription {
            get {return "Display status information on checked out files";}
        }

        /// <summary>
        /// Default constructor.
        /// </summary>
        public StatusCommandParser () {

        }

        /// <summary>
        /// Update module files from a cvs repository.
        /// </summary>
        /// <param name="cvsroot">User information</param>
        /// <param name="fileNames">Files</param>
        /// <param name="upOptions">Options</param>
        public StatusCommandParser(string cvsroot, string fileNames, string upOptions) : 
            this(new CvsRoot(cvsroot), fileNames, upOptions){
        }

        /// <summary>
        ///    Update modules or files in the cvs repository
        /// </summary>
        /// <param name="cvsroot">User Information</param>
        /// <param name="fileNames">Files</param>
        /// <param name="upOptions">Options</param>
        public StatusCommandParser(CvsRoot cvsroot, string fileNames, string upOptions) {
            this.cvsRootVar = cvsroot;
            this.fileNames = fileNames;
            this.unparsedOptions = upOptions;
        }

        /// <summary>
        /// Nicknames for the add command.
        /// </summary>
        public override ICollection Nicks {
            get {
                if (0 == commandNicks.Count) {
                    commandNicks.Add("st");
                    commandNicks.Add("stat");
                }
                return commandNicks;
            }
        }

        /// <summary>
        /// The add command is implemented in the library and commandline parser.
        /// </summary>
        public override bool Implemented {
            get {return true;}
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
            DirectoryInfo dir = 
                new DirectoryInfo(Path.Combine(Directory.GetCurrentDirectory(), "CVS"));
            StatusCommand statusCommand;
            this.localDirectory = Directory.GetCurrentDirectory();

            try {
                this.repository = Repository.Load(dir).FileContents; 
                this.CvsRoot = new CvsRoot(Root.Load(dir).FileContents);
            } catch (NullReferenceException) {
                this.InvalidRepository();
            } catch (CvsFileNotFoundException) {
                this.InvalidRepository();
            } catch (ICSharpCode.SharpCvsLib.Exceptions.CvsRootParseException) {
                this.InvalidRepository();
            }

            CurrentWorkingDirectory = new WorkingDirectory(this.CvsRoot,
                localDirectory, this.repository);

            // Create new command object
            statusCommand = new StatusCommand(CurrentWorkingDirectory);
            statusCommand.Folders = this.GetCurrentDirectory(new DirectoryInfo(localDirectory));
            this.ParseOptions(statusCommand, this.Args);

            return statusCommand;
        }

        private Folders GetCurrentDirectory(DirectoryInfo directory) {
            Folders folders = new Folders();
            Folder folder = new Folder(new DirectoryInfo(Directory.GetCurrentDirectory()));
            folder.Entries = Entries.Load(folder.Path);
            folders.Add(folder);
            return folders;
        }
 
        /// <summary>
        /// Parse the command line options/ arguments and populate the command
        ///     object with the arguments.
        /// </summary>
        /// <param name="upOptions">A string value that holds the command
        ///     line options the user has selected.</param>
        /// <exception cref="NotImplementedException">If the command argument
        ///     is not implemented currently.  TODO: Implement the argument.</exception>
        private void ParseOptions (StatusCommand command, string[] options) {
            int currentOptionIndex = 0;
            while (currentOptionIndex < options.Length) {
                string currentOption = options[currentOptionIndex];
                switch (currentOption) {
                    case "-v":
                        command.Verbose = true;
                        break;
                    case "-l":
                        command.LocalOnly = true;
                        break;
                    case "-R":
                        command.Recursive = true;
                        break;
                    case "-q":
                        command.Terse = true;
                        break;
                    case "-x":
                        command.CvsNt2Output = true;
                        break;
                    case "-X":
                        command.Cvs1Output = true;
                        break;
                    default:
                        // if a switch is passed in that we don't know about 
                        //  then throw an exception
                        if (currentOption.StartsWith("-")) {
                            throw new NotSupportedException(
                                string.Format("Unknown option specified: {0}", currentOption));
                        }   
                        // otherwise the parameter is probably a file or module name, ignore
                        break;
                }
            }

            this.ParseFiles(options);
        }

        private void ParseFiles (string[] args) {
            FileParser parser = new FileParser(args);
            foreach (FileInfo file in parser.Files) {
                System.Console.WriteLine(string.Format("File: {0}", file.FullName));
            }
        }

        /// <summary>
        /// Output the command usage and arguements.
        /// </summary>
        public override string Usage {
            get {
                string usage = 
@"Usage: cvs status [-vlR] [files...]
        -v      Verbose format; includes tag information for the file
        -l      Process this directory only (not recursive).
        -R      Process directories recursively.
        -q      Display a quick summary of each file (send more increased terseness).
        -x      cvsnt 2.x compatible output (default).
        -X      cvs 1.x compatible output.
(Specify the --help global option for a list of other help options)";

                return usage;
            }
        }

    }
}