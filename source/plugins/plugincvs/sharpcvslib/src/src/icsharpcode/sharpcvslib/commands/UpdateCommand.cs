#region Copyright
// ImportModuleCommand.cs
// Copyright (C) 2001 Mike Krueger
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
    /// Command to refresh the working folder with the current sources
    ///     from the repository.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class UpdateCommand2 : ICommand {
        private readonly ILog LOGGER =
            LogManager.GetLogger (typeof (UpdateCommand2));

        private WorkingDirectory workingDirectory;
        private string  logmessage;
        private string  vendor  = "vendor";
        private string  release = "release";
        private string  revision;
        bool hasDate = false;    // DateTime is a value type so we can't use null to indicate it hasn't been set
        DateTime date = new DateTime();

        private class Arguments {
            /// <summary>
            /// Purge any local directories that are empty.  This is a client function
            ///     and nothing is done on the server.
            /// </summary>
            public const String PURGE = "-P";
            /// <summary>
            /// Create any new directories that do not exist on the client.
            /// </summary>
            public const String CREATE_NEW_DIRECTORIES = "-d";
        }
        /// <summary>
        /// Log message.
        /// </summary>
        public string LogMessage {
            get {return logmessage;}
            set {logmessage = value;}
        }

        /// <summary>
        /// Vendor string.
        /// </summary>
        public string VendorString {
            get {return vendor;}
            set {vendor = value;}
        }

        /// <summary>
        /// Release string.
        /// </summary>
        public string ReleaseString {
            get {return release;}
            set {release = value;}
        }

        /// <summary>
        /// Update using specified revision/tag (is sticky).
        /// <br/>
        /// <warn>Only one of revision or date can be specified, NOT BOTH.</warn>
        /// </summary>
        /// <value>This corresponds to the option value 
        ///     -r sent to the cvs server.</value>
        public string Revision {
            get { 
                // check the working directory revision if there is no value, 
                //  this is to preserve backwards compatibility.
                if (null == this.revision) {
                    this.revision = this.workingDirectory.Revision;
                }
                return this.revision; 
            } 
            set { 
                if (this.hasDate) {
                    throw new ArgumentException("Cannot specify both date and revision");
                }

                this.revision = value; 
            }
        } 
  
        /// <summary>
        /// Set date to update from (is sticky).
        /// <br/>
        /// <warn>Only one of revision or date can be specified, NOT BOTH.</warn>
        /// </summary>
        /// <value>This corresponds to the option value -D
        ///     sent to the cvs server.
        /// </value>
        public DateTime Date {
            get {
                if (!this.hasDate && this.workingDirectory.HasDate) {
                    this.date = this.workingDirectory.Date;
                }
                return this.date;
            }
            set {
                if (this.revision != null) {
                    throw new ArgumentException("Cannot specify both date and revision");
                }
                this.date = value; 
                hasDate = true;
            }
        }

        /// <summary>
        ///     Returns the data as a string as required by the cvs server.
        /// </summary>
        public string GetDateAsString() {
            string dateAsString = "";
            string dateFormat = "dd MMM yyyy";

            if (hasDate) {
                dateAsString = date.ToString(dateFormat);
            }
            return dateAsString;
        }

        /// <summary>
        /// Indicate whether a date has been specified for this update.
        /// </summary>
        /// <value><code>true</code> if a date has been specified, 
        ///     otherwise <code>false</code>.</value>
        public bool HasDate {
            get { return this.hasDate || this.workingDirectory.HasDate; }
        }

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="workingDirectory"></param>
        public UpdateCommand2(WorkingDirectory workingDirectory) {
            this.workingDirectory = workingDirectory;
        }

        /// <summary>
        /// Perform the update.
        /// </summary>
        /// <param name="connection"></param>
        public void Execute(ICommandConnection connection) {
            if (workingDirectory.FoldersToUpdate == null) {
                LOGGER.Info("Nothing to update on WorkingDirectory.FoldersToUpdate.");
                return;
            }
            Folder[] _foldersToUpdate =
                (Folder[])workingDirectory.FoldersToUpdate.Clone ();
            foreach (Folder folder in _foldersToUpdate) {
                this.SetDirectory (connection, folder);

                Tag tag = folder.Tag;
                if (null != tag) {
                    connection.SubmitRequest (new StickyRequest (tag.FileContents));
                }
                if (workingDirectory.HasOverrideDirectory) {
                    connection.SubmitRequest (new ArgumentRequest (ArgumentRequest.Options.OVERRIDE_DIRECTORY));
                    connection.SubmitRequest (
                        new ArgumentRequest (workingDirectory.OverrideDirectory));
                }

                if (this.Revision != null && this.Revision != string.Empty) {
                    connection.SubmitRequest (new ArgumentRequest (ArgumentRequest.Options.REVISION));
                    connection.SubmitRequest(new ArgumentRequest(this.Revision));
                }
                if (workingDirectory.HasDate) {
                    connection.SubmitRequest (new ArgumentRequest (ArgumentRequest.Options.DATE));
                    connection.SubmitRequest(new ArgumentRequest(workingDirectory.GetDateAsString()));
                }
                
                foreach (DictionaryEntry dicEntry  in folder.Entries) {
                    Entry entry = (Entry)dicEntry.Value;
                    // directory entry modifications are not tracked in cvs,
                    //  once a directory is created it cannot be removed
                    if (!entry.IsDirectory) {
                        DateTime old = entry.TimeStamp;
                        entry.TimeStamp = entry.TimeStamp;
    				
                        this.SendFileRequest (connection, entry);
                    }
                }
                connection.SubmitRequest(new UpdateRequest());
            }

        }


        private void SetDirectory (ICommandConnection connection,
                                Folder folder) {
            String absoluteDir = String.Format("{0}", 
                connection.Repository.CvsRoot.CvsRepository);

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

        private void SendFileRequest (ICommandConnection connection,
                                Entry entry) {
            bool fileExists;
            DateTime old = entry.TimeStamp;
            entry.TimeStamp = entry.TimeStamp;
            try {
                fileExists = File.Exists (entry.FullPath);
            }
            catch (Exception e) {
                LOGGER.Error (e);
                fileExists = false;
            }

            if (!fileExists) {
                connection.SubmitRequest (new EntryRequest (entry));
            } else if (File.GetLastAccessTime(entry.FullPath) !=
                    entry.TimeStamp.ToUniversalTime ()) {
                connection.SubmitRequest(new ModifiedRequest(entry.Name));
                connection.SendFile(entry.FullPath, entry.IsBinaryFile);
            } else {
                connection.SubmitRequest(new EntryRequest(entry));
                connection.SubmitRequest(new UnchangedRequest(entry.Name));
            }

            entry.TimeStamp = old;
        }

        /// <summary>Returns the local filename and path.
        /// </summary>
        /// <param name="baseDir">The base working dir.</param>
        /// <param name="reposDir">The remote repository relative path.</param>
        /// <param name="entryName">The name of the file to look at.</param>
        private String GetFileNameAndPath (    string baseDir,
                                            string reposDir,
                                            string entryName) {
            if (LOGGER.IsDebugEnabled) {
                String msg = "baseDir=[" + baseDir + "]" +
                            "reposDir=[" + reposDir + "]" +
                            "entryName=[" + entryName + "]";
                LOGGER.Debug (msg);
            }
            string _baseDir;
            if (baseDir.EndsWith (Path.DirectorySeparatorChar.ToString ())) {
                _baseDir = baseDir.Replace (Path.DirectorySeparatorChar.ToString (), "");
            }
            else {
                _baseDir = baseDir;
            }

            //string _reposDir = reposDir.Replace ('/', Path.DirectorySeparatorChar) +
            //                    Path.DirectorySeparatorChar;
            //string _entryName = entryName.Replace ('/', Path.DirectorySeparatorChar);

            String path = Path.Combine (_baseDir, reposDir);
            String fileNameAndPath = Path.Combine (path, entryName);

            fileNameAndPath.Replace ('/', Path.DirectorySeparatorChar);

            if (LOGGER.IsDebugEnabled) {
                String msg = "fileNameAndPath=[" + fileNameAndPath + "]";
                LOGGER.Debug (msg);
            }

            return fileNameAndPath;
        }
    }
}
