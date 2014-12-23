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
    /// Import a module into the cvs repository
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class ImportModuleCommand : ICommand {
        private WorkingDirectory workingdirectory;
        private string  logmessage;
        private string  vendor  = "vendor";
        private string  release = "release";

        private readonly ILog LOGGER =
            LogManager.GetLogger (typeof (ImportModuleCommand));

        /// <summary>
        /// The log message returned by the cvs server.
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
        /// Vendor string.
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
        /// Release string
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
        /// Constructor for the import module command.
        /// </summary>
        /// <param name="workingdirectory"></param>
        /// <param name="logmessage"></param>
        public ImportModuleCommand(WorkingDirectory workingdirectory, string logmessage)
        {
            this.logmessage = logmessage;
            this.workingdirectory = workingdirectory;
        }

        /// <summary>
        /// Do the dirty work.
        /// </summary>
        /// <param name="connection"></param>
        public void Execute(ICommandConnection connection)
        {
            //connection.SubmitRequest(new CaseRequest());
            connection.SubmitRequest(new ArgumentRequest("-b"));
            connection.SubmitRequest(new ArgumentRequest("1.1.1"));
            connection.SubmitRequest(new ArgumentRequest("-m"));
            connection.SubmitRequest(new ArgumentRequest(logmessage));

            LOGGER.Info("IMPORT START");

            foreach (DictionaryEntry folder in workingdirectory.Folders) {
                this.SetDirectory(connection, (Folder)folder.Value);
                foreach (Entry entry  in ((Folder)folder.Value).Entries.Values) {
                    this.SendFileRequest(connection, entry);
                }
            }

            connection.SubmitRequest(new ArgumentRequest(workingdirectory.WorkingDirectoryName));
            connection.SubmitRequest(new ArgumentRequest(vendor));
            connection.SubmitRequest(new ArgumentRequest(release));

            connection.SubmitRequest(new ImportRequest());
            if (LOGGER.IsDebugEnabled) {
                LOGGER.Debug ("IMPORT END");
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

    }
}
