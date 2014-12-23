#region "Copyright"
// LogCommand.cs
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
    /// The log command retrieves revision history from the cvs repository.  It brings 
    /// back information about each version of a file, such as date, any sticky tags
    /// and in the case of text files the modifications that were made to the file.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2002")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class LogCommand : ICommand, ILogCommand {
        private ILog LOGGER =
            LogManager.GetLogger (typeof (LogCommand));
        
        private WorkingDirectory workingDirectory;
        private Folders folders;
        private string directory;
        private Entry entry;
        
        private ArrayList dateArgs = new ArrayList();

        private bool defaultBranch     = false;
        private bool headerAndDescOnly = false;
        private bool headerOnly        = false;
        private bool noTags            = false;
        private bool recursive         = true;

        /// <summary>
        /// The date arguments for the log command.
        /// </summary>
        public ICollection DateArgs {
            get {return this.dateArgs;}
        }

        /// <summary>
        /// The default branch to use for the module.
        /// </summary>
        public bool DefaultBranch {
            get { return defaultBranch; }
            set { defaultBranch = value; }
        }

        /// <summary>
        /// TODO: Figure out what this is used for.
        /// </summary>
        public bool HeaderAndDescOnly {
            get { return headerAndDescOnly; }
            set { headerAndDescOnly = value; }
        }

        /// <summary>
        /// TODO: Figure out what this is used for.
        /// </summary>
        public bool HeaderOnly {
            get { return headerOnly; }
            set { headerOnly = value; }
        }

        /// <summary>
        /// TODO: Figure out what this is used for.
        /// </summary>
        public bool NoTags {
            get { return noTags; }
            set { noTags = value; }
        }

        /// <summary>
        /// The collection of folders to operate on.
        /// </summary>
        public Folders Folders {
            get { 
                if (null != this.workingDirectory.Folders) {
                    if (null == this.folders) {
                        this.folders = this.workingDirectory.Folders;
                    } else {
                        foreach (Folder folder in this.workingDirectory.Folders) {
                            if (!this.folders.Contains(folder.Path.FullName)) {
                                this.folders.Add(folder);
                            }
                        }
                    }
                } else if (null != this.workingDirectory.FoldersToUpdate &&
                    this.workingDirectory.FoldersToUpdate.Length > 0){
                    foreach (Folder folder in this.workingDirectory.FoldersToUpdate) {
                        if (!this.folders.Contains(folder.Path.FullName)) {
                            this.folders.Add(folder);
                        }
                    }
                }
                return this.folders; }
            set { this.folders = value; }
        }

        // TODO: see if there is a better way to handle optional DateTime arguments
        // Note: you can't use null, as DateTime is a value type.
        
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="workingDirectory"></param>
        /// <param name="directory">relative to the root of the working directory.
        /// null for the entire working directory</param>
        /// <param name="entry">null for all files</param>
        public LogCommand(WorkingDirectory workingDirectory, string directory, Entry entry) {
            this.workingDirectory = workingDirectory;
            this.directory = directory;
            this.entry = entry;
        }

        public LogCommand(WorkingDirectory workingDirectory, Folders folders) {
            this.workingDirectory = workingDirectory;
            this.folders = folders;
        }

        /// <summary>
        /// Do the dirty work.
        /// </summary>
        /// <param name="connection"></param>
        public void Execute(ICommandConnection connection) {
            foreach (Folder folder in this.Folders.Values) {
                this.SetDirectory (connection, folder);
                
                if (defaultBranch) {
                    connection.SubmitRequest(new ArgumentRequest("-b"));
                }
                if (headerAndDescOnly) {
                    connection.SubmitRequest(new ArgumentRequest("-t"));
                }
                if (headerOnly) {
                    connection.SubmitRequest(new ArgumentRequest("-h"));
                }
                if (noTags) {
                    connection.SubmitRequest(new ArgumentRequest("-N"));
                }

                // add any date arguments
                foreach (object o in dateArgs) {
                    string dateArg = (string)o;
                    connection.SubmitRequest(new ArgumentRequest("-d"));
                    connection.SubmitRequest(new ArgumentRequest(dateArg));
                }

                foreach (DictionaryEntry de in folder.Entries) {
                    Entry entry = (Entry)de.Value;
                    // Only submit the entry information if the entry is not
                    // a directory.
                    if (!entry.IsDirectory) {
                        DateTime old = entry.TimeStamp;
                        entry.TimeStamp = entry.TimeStamp;

                        String fileName = Path.Combine(entry.Path, entry.Name);
                        this.SendEntryRequest (connection, entry);
                    }
                }
            }

            string relativeDirectory;
            if (this.directory != null && this.directory.Length > 0) {
                if (null == this.directory) {
                    this.directory = ".";
                }
                relativeDirectory = this.directory;
            } else {
                relativeDirectory = workingDirectory.WorkingDirectoryName;
            }
            // Note: don't use Path.Combine() as the separator must be "/"
            string repositoryDir = workingDirectory.CvsRoot.CvsRepository + "/" + relativeDirectory;
            connection.SubmitRequest(new DirectoryRequest(relativeDirectory, repositoryDir));

            if (defaultBranch) {
                connection.SubmitRequest(new ArgumentRequest("-b"));
            }
            if (headerAndDescOnly) {
                connection.SubmitRequest(new ArgumentRequest("-t"));
            }
            if (headerOnly) {
                connection.SubmitRequest(new ArgumentRequest("-h"));
            }
            if (noTags) {
                connection.SubmitRequest(new ArgumentRequest("-N"));
            }

            if (!recursive) {
                connection.SubmitRequest(new ArgumentRequest("-l"));
            }

            // add any date arguments
            foreach (object o in dateArgs) {
                string dateArg = (string)o;
                connection.SubmitRequest(new ArgumentRequest("-d"));
                connection.SubmitRequest(new ArgumentRequest(dateArg));
            }

            if (this.entry != null) {
                connection.SubmitRequest (new EntryRequest (this.entry));
            }

            connection.SubmitRequest (new LogRequest());

        }

        /// <summary>
        /// Adds a date range using exclusive dates.
        /// This is equivalent to the command line option "-d startDate&lt;endDate"
        /// 
        /// <param name="startDate"></param>
        /// <param name="endDate"></param>
        /// </summary>
        public void AddExclusiveDateRange(DateTime startDate, DateTime endDate) {
            AddDateRange(true, startDate, true, endDate, "<");
        }
        
        /// <summary>
        /// Adds a open ended date range with an exclusive start date.
        /// This is equivalent to the command line option "-d startDate&lt;"
        /// 
        /// <param name="startDate"></param>
        /// </summary>
        public void AddExclusiveDateStart(DateTime startDate) {
            DateTime dummyDate = new DateTime();
            AddDateRange(true, startDate, false, dummyDate, "<");
        }
        
        /// <summary>
        /// Adds a open ended date range with an exclusive start date.
        /// This is equivalent to the command line option "-d &lt;endDate"
        /// 
        /// <param name="endDate"></param>
        /// </summary>
        public void AddExclusiveDateEnd(DateTime endDate) {
            DateTime dummyDate = new DateTime();
            AddDateRange(false, dummyDate, true, endDate, "<");
        }

        /// <summary>
        /// Adds a date range using inclusive dates.
        /// This is equivalent to the command line option "-d startDate&lt;=endDate"
        /// 
        /// <param name="startDate"></param>
        /// <param name="endDate"></param>
        /// </summary>
        public void AddInclusiveDateRange(DateTime startDate, DateTime endDate) {
            AddDateRange(true, startDate, true, endDate, "<=");
        }
        
        /// <summary>
        /// Adds a open ended date range with an inclusive start date.
        /// This is equivalent to the command line option "-d startDate&lt;="
        /// 
        /// <param name="startDate"></param>
        /// </summary>
        public void AddInclusiveDateStart(DateTime startDate) {
            DateTime dummyDate = new DateTime();
            AddDateRange(true, startDate, false, dummyDate, "<=");
        }
        
        /// <summary>
        /// Adds a open ended date range with an inclusive start date.
        /// This is equivalent to the command line option "-d &lt;=endDate"
        /// 
        /// <param name="endDate"></param>
        /// </summary>
        public void AddInclusiveDateEnd(DateTime endDate) {
            DateTime dummyDate = new DateTime();
            AddDateRange(false, dummyDate, true, endDate, "<=");
        }
        
        /// <summary>
        /// Adds a single date to specify the most recent revision at or prior to this date.
        /// This is equivalent to the command line option "-d date"
        /// 
        /// <param name="date"></param>
        /// </summary>
        public void AddDate(DateTime date) {
            // re-use the code for adding ranges.
            DateTime dummyDate = new DateTime();
            AddDateRange(true, date, false, dummyDate, "");
        }
        
        private void AddDateRange(bool hasStartDate, DateTime startDate, 
            bool hasEndDate, DateTime endDate, 
            string separator) {
            string dateArg = "";
            string dateFormat = "dd MMM yyyy HH:mm:ss zz00";

            if (hasStartDate || hasEndDate) {
                if (hasStartDate) {
                    dateArg += startDate.ToString(dateFormat);
                }
                dateArg += separator;
                if (hasEndDate) {
                    dateArg += endDate.ToString(dateFormat);
                }
                this.AddDateArg(dateArg);
            }
        }

        /// <summary>
        /// Add the date argument to the date range collection.
        /// </summary>
        /// <param name="dateArg"></param>
        protected void AddDateArg(string dateArg) {
            this.dateArgs.Add(dateArg);
        }
     
        private void SetDirectory (ICommandConnection connection,
                                Folder folder) {
            String absoluteDir =
                connection.Repository.CvsRoot.CvsRepository + "/" +
                folder.Repository.FileContents;

            try {
                connection.SubmitRequest(new DirectoryRequest(folder.Repository.FileContents,
                                        absoluteDir));
            }
            catch (Exception e) {
                String msg = "Exception while submitting directory request.  " +
                            "path=[" + folder.Repository.FileContents + "]";
                LOGGER.Error (e);
            }
        }

        private void SendEntryRequest (ICommandConnection connection,
                                Entry entry) {
            bool fileExists;
            DateTime old = entry.TimeStamp;
            entry.TimeStamp = entry.TimeStamp;
            try {
                fileExists = File.Exists (entry.Filename);
            }
            catch (Exception e) {
                LOGGER.Error (e);
                fileExists = false;
            }

            connection.SubmitRequest (new EntryRequest (entry));
            if (fileExists) {
                if (File.GetLastAccessTime(entry.Filename) !=
                    entry.TimeStamp.ToUniversalTime ()) {
                    connection.SubmitRequest(new ModifiedRequest(entry.Name));
                } else {
                    connection.SubmitRequest(new UnchangedRequest(entry.Name));
                }
            }

            entry.TimeStamp = old;
        }

    }
}

