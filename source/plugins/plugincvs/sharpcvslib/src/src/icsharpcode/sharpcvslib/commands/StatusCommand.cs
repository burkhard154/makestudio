#region "Copyright"
// StatusCommand.cs
// Copyright (C) 2002 Mike Krueger
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
//    Author:     Mike Krueger,
//                Clayton Harbour  {claytonharbour@sporadicism.com}
#endregion

using System;
using System.IO;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Messages;
using ICSharpCode.SharpCvsLib.Requests;
using ICSharpCode.SharpCvsLib.Responses;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.FileSystem;

using log4net;

namespace ICSharpCode.SharpCvsLib.Commands {
    /// <summary>
    /// The status command is used to determine the local file version and the repsoitory 
    /// file version for the file or files specified.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2002")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class StatusCommand : ICommand {
        private WorkingDirectory _workingdirectory;
        private string directory;
        private Entry entry;
        private ILog LOGGER = LogManager.GetLogger (typeof (StatusCommand));
        private Folders _folders;

        private bool _verbose;
        private bool _localOnly;
        private bool _recursive;
        private bool _terse;
        private bool _cvsNt2Output;
        private bool _cvs1Output = true;

        /// <summary>
        /// Verbose format; includes tag information for the file
        /// </summary>
        public bool Verbose {
            get { return this._verbose; }
            set { this._verbose = value; }
        }

        /// <summary>
        /// Process this directory only (not recursive).
        /// </summary>
        public bool LocalOnly {
            get { return this._localOnly; }
            set { this._localOnly = value; }
        }

        /// <summary>
        /// Process directories recursively.
        /// </summary>
        public bool Recursive {
            get { return this._recursive; }
            set { this._recursive = value; }
        }

        /// <summary>
        /// Display a quick summary of each file (send more increased terseness).
        /// </summary>
        public bool Terse {
            get { return this._terse; }
            set { this._terse = value; }
        }

        /// <summary>
        /// cvsnt 2.x compatible output.
        /// </summary>
        public bool CvsNt2Output {
            get { return this._cvsNt2Output; }
            set { this._cvsNt2Output = value; }
        }

        /// <summary>
        /// cvs 1.x compatible output.
        /// </summary>
        /// <value>[Default] = true</value>
        public bool Cvs1Output {
            get { return this._cvs1Output; }
            set { this._cvs1Output = value; }
        }

        /// <summary>
        /// Folders and entries to act on.
        /// </summary>
        public Folders Folders {
            get {return this._folders;}
            set {this._folders = value;}
        }

        /// <summary>
        /// Create a new instance of the working directory.
        /// </summary>
        /// <param name="workingDirectory"></param>
        public StatusCommand(WorkingDirectory workingDirectory){
            this._workingdirectory = workingDirectory;
        }

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="workingdirectory"></param>
        /// <param name="directory"></param>
        /// <param name="entry"></param>
        public StatusCommand(WorkingDirectory workingdirectory, 
            string directory, Entry entry){
            this._workingdirectory    = workingdirectory;
            this.directory = directory;
            this.entry = entry;
            if (null == this.Folders) {
                this._folders = new Folders();
                Folder folder = new Folder();
                folder.Entries.Add(entry);
                this._folders.Add(folder);
            }
        }

        /// <summary>
        /// Execute the status command against the repository.
        /// </summary>
        /// <param name="connection"></param>
        public void Execute(ICommandConnection connection){
            if (null != this._folders) {
                connection.SubmitRequest(new ArgumentRequest(ArgumentRequest.Options.DASH));

                foreach (Folder folder in this.Folders.Values) {
                    connection.SubmitRequest(new DirectoryRequest(".",
                        this._workingdirectory.CvsRoot.CvsRepository + "/" +
                        folder.Repository.FileContents));
                    foreach (Entry entry in folder.Entries.Values) {
                        connection.SubmitRequest(new EntryRequest(entry));
                        connection.SubmitRequest(new UnchangedRequest(entry.Name));
                    }
                }
            }
            connection.ResponseMessageEvents.MessageResponseMessageEvent +=
                new MessageEventHandler(this.WriteEvent);
            connection.SubmitRequest(new StatusRequest());
        }

        public void WriteEvent(object sender, MessageEventArgs args) {
            System.Console.WriteLine(args.Message);
        }
    }
}
