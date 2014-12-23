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
using System.Globalization;
using System.Collections;
using System.IO;
using System.Text;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Console.Parser;

using log4net;

namespace ICSharpCode.SharpCvsLib.Console.Parser {

    /// <summary>
    /// Commit changes in the cvs repository.
    /// </summary>
    public class CommitCommandParser : AbstractCommandParser {
        private CvsRoot cvsRoot;
        private string fileNames;
        private string unparsedOptions;
        private string revision;
        private string logFile;
        private string message;

        /// <summary>
        /// Default constructor.
        /// </summary>
        public CommitCommandParser () {

        }

        /// <summary>
        /// Commit changes to a cvs repository.
        /// </summary>
        /// <param name="cvsroot">User information</param>
        /// <param name="fileNames">Files to remove</param>
        /// <param name="ciOptions">Options</param>
        public CommitCommandParser(string cvsroot, string fileNames, string ciOptions) : 
            this(new CvsRoot(cvsroot), fileNames, ciOptions){
        }

        /// <summary>
        ///    Commit changes in the cvs repository
        /// </summary>
        /// <param name="cvsroot">User Information</param>
        /// <param name="fileNames">Files to remove</param>
        /// <param name="ciOptions">Options</param>
        public CommitCommandParser(CvsRoot cvsroot, string fileNames, string ciOptions) {
            this.cvsRoot = cvsroot;
            this.fileNames = fileNames;
            this.unparsedOptions = ciOptions;
        }

        /// <summary>
        /// Create a new instance of the <see cref="XmlLogCommandParser"/>.
        /// </summary>
        /// <returns></returns>
        public static ICommandParser GetInstance() {
            return GetInstance(typeof(CommitCommandParser));
        }

        /// <summary>
        /// Name of the command being parsed.
        /// </summary>
        public override string CommandName {
            get {return "commit";}
        }

        /// <summary>
        /// Description of the command.
        /// </summary>
        public override string CommandDescription {
            get {return "Check files into the repository";}
        }

        /// <summary>
        /// Nicknames for the add command.
        /// </summary>
        public override ICollection Nicks {
            get {
                if (0 == commandNicks.Count) {
                    commandNicks.Add("ci");
                    commandNicks.Add("com");
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
            ICSharpCode.SharpCvsLib.Commands.CommitCommand2 commitCommand;
            try {
                this.ParseOptions(this.unparsedOptions);
                string cvsFolder = Path.Combine(Environment.CurrentDirectory, "CVS");
                // set properties before creation of CommitCommand2
                // Open the Repository file in the CVS directory
                Manager manager = new Manager(cvsFolder);
                Repository repository = null;
                Root root = null;
                try {
                    repository = manager.FetchRepository(cvsFolder); 
                } catch (CvsFileNotFoundException e) {
                    ConsoleMain.ExitProgram("Not a valid cvs repository.", e);
                }
                try {
                    root = manager.FetchRoot(cvsFolder);
                    if (null == this.cvsRoot) {
                        this.cvsRoot = new CvsRoot(root.FileContents);
                    }
                } catch (CvsFileNotFoundException e) {
                    ConsoleMain.ExitProgram("Not a valid cvs repository.", e);
                }
                // If this fails error out and the user
                //    is not in a CVS repository directory tree.
                CurrentWorkingDirectory = new WorkingDirectory( this.cvsRoot,
                    cvsFolder, repository.FileContents);
                if (revision != null) {
                    this.CurrentWorkingDirectory.Revision = revision;
                }

                ArrayList files = new ArrayList();
                if (fileNames == null || fileNames == string.Empty) {
                    this.GetFilesRecursive((new DirectoryInfo(cvsFolder)).Parent, files);
                } else {
                    DirectoryInfo cvsFolderInfo = new DirectoryInfo(cvsFolder);
                    files = new ArrayList(cvsFolderInfo.GetFiles(fileNames));
                }

                CurrentWorkingDirectory.Folders = GetFoldersToCommit(files);
                // Create new CommitCommand2 object
                commitCommand = new ICSharpCode.SharpCvsLib.Commands.CommitCommand2(
                    this.CurrentWorkingDirectory );

                // set public properties on the commit command
                if (message != null) {
                    commitCommand.LogMessage = message;
                }
         
                return commitCommand;
            } catch (CvsFileNotFoundException e) {
                ConsoleMain.ExitProgram(string.Format("No CVS folder found in path {0}",
                    Environment.CurrentDirectory), e);
                return null;
            } catch (Exception e) {
                LOGGER.Error (e);
                throw e;
            }
        }
        /// <summary>
        /// Setup the list of files to be a folder object for the cvs
        ///     library to process.
        /// </summary>
        /// <param name="filesCommitted">An array filenames that are to be committed
        ///     to the cvs repository.</param>
        private Folders GetFoldersToCommit (ICollection filesCommitted) {
            Folders folders = new Folders();
            Manager manager = new Manager(Environment.CurrentDirectory);
            foreach (FileInfo file in filesCommitted) {
                Folder folder;
                if (!folders.Contains(file.DirectoryName)) {
                    folder = new Folder();
                    DirectoryInfo cvsFolder = 
                        new DirectoryInfo(Path.Combine(file.DirectoryName, "CVS"));
                    folder.Repository = Repository.Load(cvsFolder);
                    folder.Root = Root.Load(cvsFolder);
                    try {
                        folder.Tag = Tag.Load(cvsFolder);
                    } catch (CvsFileNotFoundException) {
                        // ignore, tag missing normal
                    }
                    folder.Entries = Entries.Load(cvsFolder);
                    folders.Add(file.DirectoryName, folder);
                } 
            }
            return folders;
        }

        private void GetFilesRecursive(DirectoryInfo dir, ArrayList files) {
            if (!(dir.Name.IndexOf("CVS") > -1)) {
                foreach (FileInfo file in dir.GetFiles()) {
                    files.Add(file);
                }

                foreach (DirectoryInfo dirInfo in dir.GetDirectories()) {
                    this.GetFilesRecursive(dirInfo, files);
                }
            }
        }

        /// <summary>
        /// Parse the command line options/ arguments and populate the command
        ///     object with the arguments.
        /// </summary>
        /// <param name="ciOptions">A string value that holds the command
        ///     line options the user has selected.</param>
        /// <exception cref="NotImplementedException">If the command argument
        ///     is not implemented currently.  TODO: Implement the argument.</exception>
        private void ParseOptions (String ciOptions) {
            int endofOptions = 0;
            for (int i = 0; i < ciOptions.Length; i++) {
                if (ciOptions[i]== '-' && ciOptions[i+1] == 'r') {
                    i += 2;
                    // get revision of files to commit
                    if (ciOptions.IndexOf(" -", i, ciOptions.Length - i) == -1) {
                        endofOptions = ciOptions.Length - i - 1;
                    }
                    else {
                        endofOptions = ciOptions.IndexOf(" -", i, ciOptions.Length - i) - 2;
                    }
                    revision = ciOptions.Substring(i, endofOptions);
					i = i + endofOptions;
				}
                if (ciOptions[i]== '-' && ciOptions[i+1] == 'F') {
                    i += 2;
                    // get filename to get message from
                    if (ciOptions.IndexOf(" -", i, ciOptions.Length - i) == -1) {
                        endofOptions = ciOptions.Length - i - 1;
                    }
                    else {
                        endofOptions = ciOptions.IndexOf(" -", i, ciOptions.Length - i) - 2;
                    }
                    logFile = ciOptions.Substring(i, endofOptions);
					i = i + endofOptions;
				}
                if (ciOptions[i]== '-' && ciOptions[i+1] == 'm') {
                    i += 2;
                    // get message to attach to files 
                    if (ciOptions.IndexOf(" -", i, ciOptions.Length - i) == -1) {
                        endofOptions = ciOptions.Length - i - 1;
                    }
                    else {
                        endofOptions = ciOptions.IndexOf(" -", i, ciOptions.Length - i) - 2;
                    }
                    message = ciOptions.Substring(i, endofOptions);
					i = i + endofOptions;
				}
                if (ciOptions[i]== '-' && ciOptions[i+1] == 'c') {
                    String msg = "The -c commit option is not  " +
                        "implemented.";
                    throw new NotImplementedException (msg);
                }
                if (ciOptions[i]== '-' && ciOptions[i+1] == 'D') {
                    String msg = "The -D commit option is not  " +
                        "implemented.";
                    throw new NotImplementedException (msg);
                }
                if (ciOptions[i]== '-' && ciOptions[i+1] == 'f') {
                    String msg = "The -f commit option is not  " +
                        "implemented.";
                    throw new NotImplementedException (msg);
                }
                if (ciOptions[i]== '-' && ciOptions[i+1] == 'l') {
                    String msg = "The -l commit option is not  " +
                        "implemented.";
                    throw new NotImplementedException (msg);
                }
                if (ciOptions[i]== '-' && ciOptions[i+1] == 'n') {
                    String msg = "The -n commit option is not  " +
                        "implemented.";
                    throw new NotImplementedException (msg);
                }
                if (ciOptions[i]== '-' && ciOptions[i+1] == 'R') 
                {
                    String msg = "The -R commit option is not  " +
                        "implemented.";
                    throw new NotImplementedException (msg);
                }
            }
        }

        /// <summary>
        /// Output the command usage and arguements.
        /// </summary>
        public override string Usage {
            get {
                string usage = 
@"Usage: cvs commit [-DnRlf] [-m msg | -F logfile] [-r rev] files...
    -D          Assume all files are modified.
    -n          Do not run the module program (if any).
    -R          Process directories recursively.
    -l          Local directory only (not recursive).
    -f          Force the file to be committed; disables recursion.
    -F logfile  Read the log message from file.
    -m msg      Log message.
    -r branch   Commit to specific branch or trunk.
    -c          Check for valid edits before committing.
(Specify the --help global option for a list of other help options)";

                return usage;
            }
        }
    }
}