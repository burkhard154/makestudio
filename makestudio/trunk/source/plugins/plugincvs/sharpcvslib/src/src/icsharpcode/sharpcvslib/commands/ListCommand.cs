#region "Copyright"
// ListCommand.cs
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
    /// List the files on a cvs server.
    /// 
    /// <warn>NOTE: Only works on cvsnt servers.</warn>
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class ListCommand : ICommand {
        private class Option {
            public const string DATE = "-D";
            public const string ENTRY_FORMAT = "-e";
            public const string ALL_DETAILS = "-l";
            public const string PRUNE = "-P";
            public const string QUIETER = "-q";
            public const string RECURSIVE = "-R";
            public const string TAG = "-r";
            public const string LOCAL_TIME = "-T";
        }
        private ILog LOGGER =
            LogManager.GetLogger (typeof (ListCommand));
        
        private WorkingDirectory workingDirectory;
        private string directory;
        private Entry entry;
        
        private ArrayList dateArgs = new ArrayList();

        private bool entryFormat       = false;
        private bool allDetails        = false;
        private bool prune             = false;
        private bool quieter           = false;
        private bool recursive         = false;
        private string tag             = string.Empty;
        private bool localTime         = false;

        /// <summary>
        /// The date arguments for the log command.
        /// </summary>
        /// <value>[-D] sent to the server, takes an argument.</value>
        public ICollection DateArgs {
            get {return this.dateArgs;}
        }

        /// <summary>
        /// Display in <code>CVS/Entries</code> format.
        /// </summary>
        /// <value>[-e] sent to the server.</value>
        public bool EntryFormat {
            get {return this.entryFormat;}
            set {this.entryFormat = value;}
        }

        /// <summary>
        /// Display all details from the server.
        /// </summary>
        /// <value>[-l] sent to the server.</value>
        public bool AllDetails {
            get {return this.allDetails;}
            set {this.allDetails = value;}
        }

        /// <summary>
        /// Prune empty directories from output.
        /// </summary>
        /// <value>[-P] sent to the server.</value>
        public bool Prune {
            get {return this.prune;}
            set {this.prune = value;}
        }

        /// <summary>
        /// Display even less output.
        /// </summary>
        /// <value>[-q] sent to the server.</value>
        public bool Quieter {
            get {return this.quieter;}
            set {this.quieter = value;}
        }

        /// <summary>
        /// List files recursively.
        /// </summary>
        /// <value>[-R] sent to the server.</value>
        public bool Recursive {
            get {return this.recursive;}
            set {this.recursive = value;}
        }

        /// <summary>
        /// List files by revision or tag.
        /// </summary>
        /// <value>[-r] sent to the server.</value>
        public string Tag {
            get {return this.tag;}
            set {this.tag = value;}
        }

        /// <summary>
        /// Show as local time instead of <code>GMT</code>.
        /// </summary>
        /// <value>[-T] sent to the server.</value>
        public bool LocalTime {
            get {return this.localTime;}
            set {this.localTime = value;}
        }

        // TODO: see if there is a better way to handle optional DateTime arguments
        // Note: you can't use null, as DateTime is a value type.
        
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

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="workingDirectory"></param>
        public ListCommand(WorkingDirectory workingDirectory) {
            this.workingDirectory    = workingDirectory;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="workingDirectory"></param>
        /// <param name="directory">relative to the root of the working directory.
        /// null for the entire working directory</param>
        /// <param name="entry">null for all files</param>
        public ListCommand(WorkingDirectory workingDirectory, string directory, Entry entry)
        {
            this.workingDirectory = workingDirectory;
            this.directory = directory;
            this.entry = entry;
        }

        /// <summary>
        /// Do the dirty work.
        /// </summary>
        /// <param name="connection"></param>
        public void Execute(ICommandConnection connection)
        {
            if (this.Quieter) {
                connection.SubmitRequest(new ArgumentRequest(Option.QUIETER));
            }
            if (this.EntryFormat) {
                connection.SubmitRequest(new ArgumentRequest(Option.ENTRY_FORMAT));
            }
            if (this.AllDetails) {
                connection.SubmitRequest(new ArgumentRequest(Option.ALL_DETAILS));
            }
            if (this.Recursive) {
                connection.SubmitRequest(new ArgumentRequest(Option.RECURSIVE));
            }
            if (this.LocalTime) {
                connection.SubmitRequest(new ArgumentRequest(Option.LOCAL_TIME));
            }
            if (null != this.Tag && this.Tag.Length != 0) {
                connection.SubmitRequest(new ArgumentRequest(Option.TAG));
            }
            if (this.Prune) {
                connection.SubmitRequest(new ArgumentRequest(Option.PRUNE));
            }

            // add any date arguments
            foreach (object o in dateArgs) {
                string dateArg = (string)o;
                connection.SubmitRequest(new ArgumentRequest(Option.DATE));
                connection.SubmitRequest(new ArgumentRequest(dateArg));
            }

            connection.SubmitRequest (new ListRequest());
        }     
    }
}

