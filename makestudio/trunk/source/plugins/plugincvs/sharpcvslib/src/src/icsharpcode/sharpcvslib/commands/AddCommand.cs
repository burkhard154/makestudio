#region "Copyright"
// AddCommand.cs
// Copyright (C) 2003 Clayton Harbour
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
#endregion

using System;
using System.Collections;
using System.IO;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Requests;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.FileSystem;

using log4net;

namespace ICSharpCode.SharpCvsLib.Commands {
	/// <summary>
	/// 
	/// copied from: http://developer.apple.com/darwin/tools/cvs/cederquist/cvs_60.html
	/// 
    /// Command: cvs add [-k kflag] [-m message] files 
    ///
    /// Schedule files to be added to the repository. The files or directories 
    /// specified with add must already exist in the current directory. To add a 
    /// whole new directory hierarchy to the source repository (for example, files 
    /// received from a third-party vendor), use the import command instead. 
    ///
    /// The added files are not placed in the source repository until you use commit 
    /// to make the change permanent. Doing an add on a file that was removed with 
    /// the remove command will undo the effect of the remove, unless a commit 
    /// command intervened. 
    ///
    /// The `-k' option specifies the default way that this file will be checked out; 
    /// for more information see section 12.4 Substitution modes.
    ///
    ///    The `-m' option specifies a description for the file. This description appears in the history log (if it is enabled, see section C.10 The history file). It will also be saved in the version history inside the repository when the file is committed. The log command displays this description. The description can be changed using `admin -t'. See section A.6 admin--Administration. If you omit the `-m description' flag, an empty string will be used. You will not be prompted for a description. 
    ///
    ///    For example, the following commands add the file `backend.c' to the repository:
    ///
    ///    $ cvs add backend.c
    ///    $ cvs commit -m "Early version. Not yet compilable." backend.c
    ///
    ///    When you add a file it is added only on the branch which you are working on (see section 5 Branching and merging). You can later merge the additions to another branch if you want (see section 5.9 Merging can add or remove files). 
	/// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
	public class AddCommand : ICommand {
        private WorkingDirectory workingDirectory;

        private ILog LOGGER = LogManager.GetLogger (typeof (AddCommand));
        private Folders folders;

        /// <summary>
        /// Folders that will be added/ updated when the add command is executed.
        ///     Each folder contains an entry request, even though the actual entry
        ///     does not exist on the hard drive.
        /// </summary>
        public Folders Folders {
            get {return this.folders;}
            set {this.folders = value;}
        }
            

        /// <summary>
        /// Initialize the working directory to be used in the add.
        /// </summary>
        public AddCommand(WorkingDirectory workingDirectory) {
            this.workingDirectory    = workingDirectory;
        }

        /// <summary>
        /// Execute checkout module command.
        /// 
        /// taken from: http://www.elegosoft.com/cvs/cvsclient.html
        /// add \n
        ///     Response expected: yes. Add a file or directory. This uses any 
        ///     previous Argument, Directory, Entry, or Modified requests, if they 
        ///     have been sent. The last Directory sent specifies the working 
        ///     directory at the time of the operation. To add a directory, send the 
        ///     directory to be added using Directory and Argument requests.
        ///
        /// </summary>
        /// <example>
        /// 
        /// C: Root /u/cvsroot
        /// . . .
        /// C: Argument nsdir
        /// C: Directory nsdir
        /// C: /u/cvsroot/1dir/nsdir
        /// C: Directory .
        /// C: /u/cvsroot/1dir
        /// C: add
        /// S: M Directory /u/cvsroot/1dir/nsdir added to the repository
        /// S: ok
        ///
        /// You will notice that the server does not signal to the client in any 
        /// particular way that the directory has been successfully added. The client 
        /// is supposed to just assume that the directory has been added and update 
        /// its records accordingly. Note also that adding a directory is immediate; 
        /// it does not wait until a ci request as files do. To add a file, send the 
        /// file to be added using a Modified request. For example:
        ///
        /// C: Argument nfile
        /// C: Directory .
        /// C: /u/cvsroot/1dir
        /// C: Modified nfile
        /// C: u=rw,g=r,o=r
        /// C: 6
        /// C: hello
        /// C: add
        /// S: E cvs server: scheduling file `nfile' for addition
        /// S: Mode u=rw,g=r,o=r
        /// S: Checked-in ./
        /// S: /u/cvsroot/1dir/nfile
        /// S: /nfile/0///
        /// S: E cvs server: use 'cvs commit' to add this file permanently
        /// S: ok
        ///
        /// Note that the file has not been added to the repository; the only effect 
        /// of a successful add request, for a file, is to supply the client with a 
        /// new entries line containing `0' to indicate an added file. In fact, the 
        /// client probably could perform this operation without contacting the 
        /// server, although using add does cause the server to perform a few more 
        /// checks. The client sends a subsequent ci to actually add the file to the 
        /// repository. Another quirk of the add request is that with CVS 1.9 and 
        /// older, a pathname specified in an Argument request cannot contain `/'. 
        /// There is no good reason for this restriction, and in fact more recent 
        /// CVS servers don't have it. But the way to interoperate with the older 
        /// servers is to ensure that all Directory requests for add (except those 
        /// used to add directories, as described above), use `.' for local-directory. 
        /// Specifying another string for local-directory may not get an error, but 
        /// it will get you strange Checked-in responses from the buggy servers.
        /// </example>
        /// <param name="connection">Server connection</param>
        public void Execute(ICommandConnection connection) {
            connection.SubmitRequest(new ArgumentRequest(ArgumentRequest.Options.DASH));
            int loops = 0;
            foreach (DictionaryEntry folderEntry in this.Folders) {
                LOGGER.Debug("loops=[" + loops++ + "]");
                Folder folder = (Folder)folderEntry.Value;
                this.SetDirectory (connection, folder);

                // send each is-modified request
                foreach (DictionaryEntry entryEntry in folder.Entries) {
                    Entry entry = (Entry)entryEntry.Value;
                    //connection.SubmitRequest(new IsModifiedRequest(entry.Name));

                    //String fileName = Path.Combine(entry.Path, entry.Name);
                    this.SendFileRequest (connection, entry);

                    // Add the file to the cvs entries file
                    Manager manager = new Manager(connection.Repository.WorkingPath);
                    manager.Add(entry);
                    if (LOGGER.IsDebugEnabled) {
                        LOGGER.Debug("AddCommand.  Entry=[" + entry + "]");
                    }
                }

                // send each argument request
                foreach (DictionaryEntry entryEntry in folder.Entries) {
                    Entry entry = (Entry)entryEntry.Value;
                    connection.SubmitRequest(new ArgumentRequest(entry.Name));

                    //String fileName = Path.Combine(entry.Path, entry.Name);
                    //this.SendFileRequest (connection, entry);

                    // Add the file to the cvs entries file
                    Manager manager = new Manager(connection.Repository.WorkingPath);
                    manager.Add(entry);
                    if (LOGGER.IsDebugEnabled) {
                        LOGGER.Debug("AddCommand.  Entry=[" + entry + "]");
                        Entries entries = manager.FetchEntries(entry.FullPath);
                        foreach (DictionaryEntry dicEntry in entries) {
                            LOGGER.Debug("entry=[" + dicEntry.Value + "]");
                        }
                    }
                }
            }

            connection.SubmitRequest(new AddRequest());
        }

        private void SetDirectory (ICommandConnection connection,
            Folder folder) {
            String absoluteDir =
                connection.Repository.CvsRoot.CvsRepository + "/" +
                folder.Repository.FileContents;

            try {
                connection.SubmitRequest(new DirectoryRequest(".",
                    absoluteDir));
            }
            catch (Exception e) {
                String msg = "Exception while submitting directory request.  " +
                    "path=[" + folder.Repository.FileContents + "]";
                LOGGER.Error (e);
            }
        }

        private void SendFileRequest (ICommandConnection connection, Entry entry) {
            DateTime old = entry.TimeStamp;
            entry.TimeStamp = entry.TimeStamp;
            connection.SubmitRequest (new EntryRequest (entry));
            connection.SubmitRequest(new ModifiedRequest(entry.Name));
            connection.SendFile(entry.FullPath, entry.IsBinaryFile);

            entry.TimeStamp = old;
        }
	}
}
