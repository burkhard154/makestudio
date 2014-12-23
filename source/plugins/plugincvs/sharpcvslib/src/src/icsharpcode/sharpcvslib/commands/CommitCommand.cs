#region "Copyright"
// CommitCommand.cs
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
#endregion

using System;
using System.Collections;
using System.Collections.Specialized;
using System.IO;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Requests;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.FileSystem;

using log4net;

namespace ICSharpCode.SharpCvsLib.Commands {
    /// <summary>
    /// Commit command
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class CommitCommand2 : ICommand {
        private readonly ILog LOGGER = LogManager.GetLogger (typeof (CommitCommand2));
        private WorkingDirectory workingdirectory;
        private string  logmessage;
        private string  vendor  = "vendor";
        private string  release = "release";

        /// <summary>
        /// Log message
        /// </summary>
        public string LogMessage {
            get {
                return logmessage;
            }
            set {
                logmessage = value;
            }
        }

        /// <summary>
        /// Vendor string
        /// </summary>
        public string VendorString {
            get {
                return vendor;
            }
            set {
                vendor = value;
            }
        }

        /// <summary>
        /// Release String
        /// </summary>
        public string ReleaseString {
            get {
                return release;
            }
            set {
                release = value;
            }
        }

        /// <summary>
        /// Commit command two constructor
        /// </summary>
        /// <param name="workingdirectory"></param>
        public CommitCommand2(WorkingDirectory workingdirectory)
        {
            this.workingdirectory = workingdirectory;
        }

        /// <summary>
        /// Execute the commit command
        /// </summary>
        /// <param name="connection">Cvs server connection</param>
        public void Execute(ICommandConnection connection) {
            connection.SubmitRequest(new ArgumentRequest("-m"));
            connection.SubmitRequest(new ArgumentRequest("LOG MESSAGE"));
            connection.SubmitRequest(new ArgumentRequest(ArgumentRequest.Options.DASH));
            foreach (DictionaryEntry folderEntry in workingdirectory.Folders) {
                Folder folder = (Folder)folderEntry.Value;
                this.SetDirectory(connection, folder);
                foreach (DictionaryEntry entryEntry  in folder.Entries) {
                    Entry entry = (Entry)entryEntry.Value;
                    if (!entry.IsDirectory) {
                        this.SendFileRequest(connection, entry);
                    }
                }

                this.SetDirectory(connection, folder);

                foreach (DictionaryEntry entryEntry in folder.Entries) {
                    Entry entry = (Entry)entryEntry.Value;
                    if (!entry.IsDirectory) {
                        connection.SubmitRequest(new ArgumentRequest(entry.Name));
                    }
                }
            }
            connection.SubmitRequest(new CommitRequest());
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

        private void SendFileRequest (ICommandConnection connection,
            Entry entry) {
            DateTime old = entry.TimeStamp;
            entry.TimeStamp = entry.TimeStamp;
            connection.SubmitRequest (new EntryRequest (entry));
            connection.SubmitRequest(new ModifiedRequest(entry.Name));
            connection.SendFile(entry.FullPath, entry.IsBinaryFile);

            entry.TimeStamp = old;
        }
           
    }
}
