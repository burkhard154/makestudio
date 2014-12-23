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
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Console.Parser;
using ICSharpCode.SharpCvsLib.FileSystem;

using log4net;

namespace ICSharpCode.SharpCvsLib.Console.Parser {

    /// <summary>
    /// Add file(s) in the cvs repository.
    /// </summary>
    public class AddCommandParser : AbstractCommandParser {
        private CvsRoot cvsRoot;
        private string fileNames;
        private string unparsedOptions;
        private string message;
        private string kflag; // could be enumeration

        /// <summary>
        /// Default constructor.
        /// </summary>
        public AddCommandParser () {

        }

        /// <summary>
        /// Add file(s) from a cvs repository.
        /// </summary>
        /// <param name="cvsroot">User information</param>
        /// <param name="fileNames">Files to remove</param>
        /// <param name="adOptions">Options</param>
        public AddCommandParser(string cvsroot, string fileNames, string adOptions) : 
            this(new CvsRoot(cvsroot), fileNames, adOptions){
        }

        /// <summary>
        /// Add file(s) in the cvs repository
        /// </summary>
        /// <param name="cvsroot">User Information</param>
        /// <param name="fileNames">Files to remove</param>
        /// <param name="adOptions">Options</param>
        public AddCommandParser(CvsRoot cvsroot, string fileNames, string adOptions) {
            this.cvsRoot = cvsroot;
            this.fileNames = fileNames;
            this.unparsedOptions = adOptions;
        }

        /// <summary>
        /// Create a new instance of the <see cref="UpdateCommandParser"/>.
        /// </summary>
        /// <returns></returns>
        public static ICommandParser GetInstance() {
            return GetInstance(typeof(AddCommandParser));
        }

        /// <summary>
        /// Name of the command being parsed.
        /// </summary>
        public override string CommandName {
            get {return "add";}
        }

        /// <summary>
        /// Description of the command.
        /// </summary>
        public override string CommandDescription {
            get {return "Add a new file/directory to the repository";}
        }

        /// <summary>
        /// Nicknames for the add command.
        /// </summary>
        public override ICollection Nicks {
            get {
                if (commandNicks.Count == 0) {
                    commandNicks.Add("ad");
                    commandNicks.Add("new");
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
            ICSharpCode.SharpCvsLib.Commands.AddCommand addCommand;
            this.ParseOptions(this.unparsedOptions);
            try {
                // Open the Repository file in the CVS directory
                Manager manager = new Manager(Environment.CurrentDirectory);
                Repository repository = manager.FetchRepository(Environment.CurrentDirectory); 
                // If this fails error out and state the user
                //    is not in a CVS repository directory tree.
                CurrentWorkingDirectory = new WorkingDirectory( this.cvsRoot,
                    Environment.CurrentDirectory, repository.FileContents);
                CurrentWorkingDirectory.OverrideDirectory = Environment.CurrentDirectory;
                // If fileNames has a wild card (*) like '*.txt'
                // Create new AddCommand object
                addCommand = new ICSharpCode.SharpCvsLib.Commands.AddCommand(
                                 this.CurrentWorkingDirectory);

                String[] files = Directory.GetFiles(Environment.CurrentDirectory, fileNames);
                ArrayList copiedFiles = new ArrayList ();
                foreach (String file in files) {
                    LOGGER.Debug("file=[" + file + "]");
                    // Remove the .txt when everything works, giving me bugs...
                    String fullPath = Path.Combine(Environment.CurrentDirectory, file);
                    copiedFiles.Add(fullPath);
                }
                addCommand.Folders = GetFoldersToAdd(copiedFiles);
            }
            catch (Exception e) {
                LOGGER.Error (e);
                throw e;
            }
            return addCommand;
        }
 
        /// <summary>
        /// Parse the command line options/ arguments and populate the command
        ///     object with the arguments.
        /// </summary>
        /// <param name="adOptions">A string value that holds the command
        ///     line options the user has selected.</param>
        private void ParseOptions (String adOptions) {
            int endofOptions = 0;
            for (int i = 0; i < adOptions.Length; i++) {
                if (adOptions[i]== '-' && adOptions[i+1] == 'm') {
                    i += 2;
                    // get message to attach to files 
                    if (adOptions.IndexOf(" -", i, adOptions.Length - i) == -1) {
                        endofOptions = adOptions.Length - i - 1;
                    }
                    else {
                        endofOptions = adOptions.IndexOf(" -", i, adOptions.Length - i) - 2;
                    }
                    message = adOptions.Substring(i, endofOptions);
					i = i + endofOptions;
                }
                if (adOptions[i]== '-' && adOptions[i+1] == 'k') {
                    i += 2;
                    // get rcs-kflag to attach to files 
                    if (adOptions.IndexOf(" -", i, adOptions.Length - i) == -1) {
                        endofOptions = adOptions.Length - i - 1;
                    }
                    else {
                        endofOptions = adOptions.IndexOf(" -", i, adOptions.Length - i) - 2;
                    }
                    kflag = adOptions.Substring(i, endofOptions);
					i = i + endofOptions;
				}
            }
        }
        /// <summary>
        /// Setup the list of files to be a folder object for the cvs
        ///     library to process.
        /// </summary>
        /// <param name="filesAdded">An array filenames that are to be added
        ///     to the cvs repository.</param>
        private Folders GetFoldersToAdd (ICollection filesAdded) {
            Folders folders = new Folders();
            Manager manager = new Manager(Environment.CurrentDirectory);
            LOGGER.Debug("Number of files copied=[" + filesAdded.Count + "]");
            foreach (String file in filesAdded) {
                Folder folder;
                if (!folders.Contains(Path.GetDirectoryName(file))) {
                    folder = new Folder();
                    LOGGER.Debug("file=[" + file + "]");
                    LOGGER.Debug("file path=[" + Path.GetDirectoryName(file) + "]");
                    folder.Repository = 
                        manager.FetchRepository(Path.GetDirectoryName(file));
                    folder.Root = 
                        manager.FetchRoot(Path.GetDirectoryName(file));
                    folder.Tag = 
                        manager.FetchTag(Path.GetDirectoryName(file));
                    folders.Add(Path.GetDirectoryName(file), folder);
                } else {
                    folder = folders[Path.GetDirectoryName(file)];
                }
                if (!folder.Entries.Contains(file)) {
                    Entry entry = Entry.CreateEntry(new FileInfo(file));
                    folder.Entries.Add (file, entry);
                } else {
                    folder.Entries[file] = Entry.CreateEntry(new FileInfo(file));
                }
            }
            return folders;
        }

        /// <summary>
        /// Output the command usage and arguements.
        /// </summary>
        public override string Usage {
            get {
                string usage = 
@"Usage: cvs add [-k rcs-kflag] [-m message] files...
        -k      Use ""rcs-kflag"" to add the file with the specified kflag.
        -m      Use ""message"" for the creation log.
(Specify the --help global option for a list of other help options)";

                return usage;
            }
        }
    }
}