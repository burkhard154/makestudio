#region "Copyright"
// Copyright (C) 2004 Gerald Evans
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
using System.Collections.Specialized;
using System.IO;
using System.Text;
using System.Xml;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Exceptions;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Messages;
using ICSharpCode.SharpCvsLib.Misc;

namespace ICSharpCode.SharpCvsLib.Extension.LogReporter {
    /// <summary>
    /// Fires off a 'log' command to the server and 
    /// returns the log info as a tree.
    /// Typical usage is as follows:
    /// 
    ///    string password = "password";
    ///	    
    ///    LogReportCommand logCommand = new LogReportCommand("sharpcvslib", "C:\\sharpcvslib");
    /// 
    ///    logCommand.SetLastNDays(7);
    ///    // or logCommand.StartDate = new DateTime(...);
    ///    // and/or logCommand.EndDate = new DateTime(...);
    ///	    
    ///    LogReport logReport = logCommand.Run(password);
	///    foreach (LogFile logFile in logReport)
    ///    {
    ///	        ...
    ///	        foreach (LogRevision logRevision in logFile)
    ///	        {
    ///    	        ...
    ///	        }
    ///	   }
    ///
    /// </summary>
    [Author("Gerald Evans", "gne@users.sourceforge.net", "2004")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class LogReportCommand {
        private ILog LOGGER = LogManager.GetLogger (typeof (LogReportCommand));

    	private CvsRoot cvsRoot;
        private WorkingDirectory workingDirectory;

   		// data set by ctor
        private string module;
        private string localDirectory;
        
    	// date information set by caller
    	private DateTime startDate;
    	private bool hasStartDate;
    	private DateTime endDate;
    	private bool hasEndDate;
 
        // Patterns we look for in the lines we receive from the cvs server
        private const string repositoryFnmPrefix = "RCS file: ";
        private const string repositoryFnmPrefixWithM = "M RCS file: ";
        private const string workingFnmPrefix = "Working file: ";
        private const string symbolicNamesPrefix = "symbolic names:";
        private const string descriptionPrefix = "description:";
        private const string revisionPrefix = "revision ";
        private const string datePrefix = "date: ";
        private const string branchesPrefix = "branches: ";
        private const string fileEndPrefix = "==========";
        private const string revisionEndPrefix = "----------";

        // Represents what we want next from the messages output by the log command
        private enum LogState { 
            WANT_FILE_HEADER_START,          // initial state where we want the file header
            WANT_FILE_HEADER,                // we are in the file header, but want more
            WANT_FILE_HEADER_SYMBOLIC_NAMES, // in symbolic names within file header
            WANT_FILE_DESCRIPTION,
            WANT_REVISION,
        }
        private LogState logState = LogState.WANT_FILE_HEADER_START;
    
        // this is where we build up the LogReport
        private LogReport curLogReport;
        private LogFile curLogFile;
        private LogRevision curLogRevision;
        
        /// <summary>
        /// ctor
        /// </summary>
        public LogReportCommand(string module, string localDirectory)
        {
            this.module = module;
            this.localDirectory = localDirectory;
        }

        /// <summary>
        /// Create a new instance of the log report command.
        /// </summary>
        /// <param name="workingDirectory"></param>
        /// <param name="module"></param>
        public LogReportCommand(WorkingDirectory workingDirectory, string module) {
            this.workingDirectory = workingDirectory;
            this.module = module;
            this.localDirectory = workingDirectory.LocalDirectory;
            this.cvsRoot = workingDirectory.CvsRoot;
        }
    
        /// <summary>
        /// If set, only report changes on or after this date
        /// </summary>
        public DateTime StartDate 
        {
            get {return this.endDate;}
        	set { 
        		startDate = value;
        		hasStartDate = true;
        	}
        }
            
        /// <summary>
        /// If set, only report changes on or before this date
        /// </summary>
        public DateTime EndDate 
        {
            get {return this.endDate;}
        	set { 
        		endDate = value;
        		hasEndDate = true;
        	}
        }
        
        /// <summary>
        /// Only report changes during the last given number of days.
        /// </summary>
        public void SetLastNDays(int days)
        {
        	//endDate = DateTime.Now;
        	startDate = DateTime.Now.AddDays(- days);
        	hasStartDate = true;
        	//hasEndDate = true;
        }

        /// <summary>
        /// Produce the report
        /// </summary>
        public LogReport Run(string password)
        {
          // read Root and Repository from local directory
            if (null == this.cvsRoot) {
                Manager manager = new Manager(localDirectory);
                Root root = (Root)manager.FetchSingle (localDirectory,
                    Factory.FileType.Root);
                cvsRoot = new CvsRoot(root.FileContents);
            }
           
            if (null == this.workingDirectory) {
                workingDirectory = new WorkingDirectory(cvsRoot,
                    localDirectory,
                    module);
            }
            
            // Get a connection
            CVSServerConnection connection = new CVSServerConnection();

        	connection.Connect(workingDirectory, password);
        	
        	return Run(connection);
        }
    
        /// <summary>
        /// Produce the report
        /// Alternate interface for when we are given a server cooection
        /// This is needed for the SharpCvsLib command line client
        /// </summary>
        public LogReport Run(ICommandConnection connection)
        {
           // read Root and Repository from local directory
            if (null == this.cvsRoot) {
                Manager manager = new Manager(localDirectory);
                Root root = (Root)manager.FetchSingle (localDirectory,
                    Factory.FileType.Root);
        
                this.cvsRoot = new CvsRoot(root.FileContents);
            }

            if (null == workingDirectory) {
                Manager manager = new Manager(localDirectory);
                Repository repository = (Repository)manager.FetchSingle (localDirectory,
                    Factory.FileType.Repository);

                this.workingDirectory = new WorkingDirectory(cvsRoot,
                    localDirectory,
                    repository.FileContents);
            }
        
            ILogCommand command;
            // Recursively add all cvs folders/files under the localDirectory
System.Console.WriteLine("GNE workingDirectory.WorkingPath = {0}", workingDirectory.WorkingPath);
System.Console.WriteLine("GNE localDirectory: {0}", localDirectory);
 //           if (Directory.Exists(workingDirectory.WorkingPath)) {
            if (Directory.Exists(localDirectory) && File.Exists(Path.Combine(localDirectory, "Repository"))) {
                workingDirectory.FoldersToUpdate = FetchFiles(localDirectory);
                command = 
                    new LogCommand(workingDirectory, this.workingDirectory.ModuleName, null);
            } else {
                command = 
// GNE - this wont compile                   new LogCommand(workingDirectory, this.workingDirectory.ModuleName);
                    new RLogCommand(workingDirectory, this.workingDirectory.ModuleName);
            }
    
            // add any date restrictions        
            if (hasStartDate && hasEndDate) {
            	command.AddInclusiveDateRange(startDate, endDate);
            } else if (hasStartDate) {
            	command.AddInclusiveDateStart(startDate);
            } else if (hasEndDate) {
            	command.AddInclusiveDateEnd(endDate);
            }
     
            // Initialse state machine
            curLogReport = new LogReport(); // this is what we are going to return to the caller
            curLogFile = new LogFile(this.cvsRoot);
            curLogRevision = new LogRevision();
            logState = LogState.WANT_FILE_HEADER_START;
             
            if (connection.GetType() == typeof(CVSServerConnection)) {
                CVSServerConnection cvsServerConnection = (CVSServerConnection)connection;
                cvsServerConnection.MessageEvent.MessageEvent += new EncodedMessage.MessageHandler(OnMessage);
            }
            command.Execute(connection);

            // return curLogReport but clear our reference to it
            LogReport report = curLogReport;
            curLogReport = null;
            return report;
        }
        
        /// <summary>
        /// Returns a list of all the folders (and the files in each of those folders)
        /// that we need to get the change log for.
        /// </summary>
        private Folder[] FetchFiles(string localDirectory)
        {
            ArrayList folders = new ArrayList ();
            FetchFilesRecursive(folders, localDirectory);
            return (Folder[])folders.ToArray (typeof (Folder));
        }
         
        private void FetchFilesRecursive(ArrayList folders, string localDirectory)
        {
            String modulePath = localDirectory;
            Manager manager = new Manager(modulePath);

            Folder folder = new Folder ();
            folder.Repository = (Repository)manager.FetchSingle (modulePath,
                                Factory.FileType.Repository);
            
            Entries entries1= manager.FetchEntries(Path.Combine(modulePath, Entry.FILE_NAME));

            foreach (DictionaryEntry dicEntry in entries1) {
                Entry entry = (Entry)dicEntry.Value;
                if (!entry.IsDirectory) {
            		if (LOGGER.IsDebugEnabled) {
                		LOGGER.Debug("Found file=[" + entry.FullPath + "]");
            		}
                    folder.Entries.Add (entry.FullPath, entry);
                }
            }
            folders.Add (folder);
           
            foreach (DictionaryEntry dicEntry in entries1) {
                Entry entry = (Entry)dicEntry.Value;
                if (entry.IsDirectory) {
                    string childDir = Path.Combine(localDirectory, entry.Name);
            		if (LOGGER.IsDebugEnabled) {
                		LOGGER.Debug("Found directory=[" + childDir + "]");
            		}
                    FetchFilesRecursive(folders, childDir);
                }
            }
        }
        
        /// <summary>
        /// This is called for each Message response we receive from the cvs server.
        /// </summary>
        public void OnMessage(string message)
        {
     
//            System.Console.WriteLine(message);
 
             // for some reason the message handler is now preceeding 
            // each message with "cvs server: " so we need to strip this off first
            if (message.StartsWith("cvs server: "))
            {
                message = message.Substring(12);
            }

            // only process the lines starting with "M "        
            if (message.StartsWith("M ")) {
                // Strip of the leading "M "
                message = message.Substring(2);
                
                if (message.StartsWith(revisionEndPrefix)) {
                    // seperator between file and revision or between revisions
                    if (logState == LogState.WANT_FILE_HEADER_START) {
                        // ignore this (shouldn't happen)
                    }
                    else if (logState == LogState.WANT_FILE_HEADER || logState == LogState.WANT_FILE_DESCRIPTION) {
                        // this is the seperator between te file header and the first revision
                    }
                    else {
                        // seperator between revisions
                        curLogFile.AddRevision(curLogRevision);
                    }
                    curLogRevision = new LogRevision();
                    logState = LogState.WANT_REVISION;    
                }
                else if (message.StartsWith(fileEndPrefix)) {
                    // seperator between files
                    if (logState == LogState.WANT_FILE_HEADER_START) {
                        // ignore this (shouldn't happen)
                    }
                    else if (logState == LogState.WANT_FILE_HEADER || logState == LogState.WANT_FILE_DESCRIPTION) {
                        // file with no revisions
                        curLogReport.AddFile(curLogFile);
                    }
                    else {
                        // first add the revision
                        curLogFile.AddRevision(curLogRevision);
                        curLogRevision = new LogRevision();
                        // and now the file
                        curLogReport.AddFile(curLogFile);
                    }
                    curLogFile = new LogFile(this.cvsRoot);
                    logState = LogState.WANT_FILE_HEADER_START;    
                }
                else {
                    switch (logState) {
                        case LogState.WANT_FILE_HEADER_START:          // drop into WANT_FILE_HEADER
                        case LogState.WANT_FILE_HEADER_SYMBOLIC_NAMES: // drop into WANT_FILE_HEADER
                        case LogState.WANT_FILE_HEADER:
                            OnMessageHeader(message);
                            break;
                                    
                        case LogState.WANT_FILE_DESCRIPTION:
                            OnMessageDescription(message);
                            break;
                        
                        case LogState.WANT_REVISION:
                            OnMessageRevision(message);
                            break;
                    }
                }
            }
        }
        
        /// <summary>
        /// This is called for each Message response we receive from the cvs server when we are 
        /// processing the header
        /// </summary>
        public void OnMessageHeader(string message)
        {
            // First handle symbolic names substate
            if (logState == LogState.WANT_FILE_HEADER_SYMBOLIC_NAMES) {
                // symbolic names start with a tab
                // but we also detect space, just in case
                if (message.StartsWith("\t") || message.StartsWith(" ")) {
                    // extract symbolic name and revision
                    ExtractSymbolicName(message);
                }
                else {
                    // this must be the end of the symbolic names
                    logState = LogState.WANT_FILE_HEADER;
                    // Note: we must now check for other tags that we are interested in 
                }
            }
        
            if (message.StartsWith(symbolicNamesPrefix)) {
                // file line is of form 'symbolic names:'
                logState = LogState.WANT_FILE_HEADER_SYMBOLIC_NAMES;
            }
            else if (message.StartsWith(repositoryFnmPrefix)) {
                // file line is of form 'RCS file: <filename>'
                curLogFile.RepositoryFnm = message.Substring(repositoryFnmPrefix.Length);
                logState = LogState.WANT_FILE_HEADER;
            } 
            else if (message.StartsWith(repositoryFnmPrefixWithM)) {
                // file line is of form 'M RCS file: <filename>'
                curLogFile.RepositoryFnm = message.Substring(repositoryFnmPrefixWithM.Length);
                logState = LogState.WANT_FILE_HEADER;
            }
            else if (message.StartsWith(workingFnmPrefix)) {
                // file line is of form 'Working file: <filename>'
                curLogFile.WorkingFnm = message.Substring(workingFnmPrefix.Length);
                logState = LogState.WANT_FILE_HEADER;
            }
            else if (message.StartsWith(descriptionPrefix)) {
                // description line is of form 'description:'
                // and is then optionally followed by a multi-line description
                logState = LogState.WANT_FILE_DESCRIPTION;
            }
        }
        
        /// <summary>
        /// This is called for each Message response we receive from the cvs server when we are 
        /// processing the description
        /// </summary>
        public void OnMessageDescription(string message)
        {
            // append description line to the description
            if (curLogFile.Description.Length > 0) {
                curLogFile.Description += Environment.NewLine;
            }
            curLogFile.Description += message;
        }
         
        /// <summary>
        /// This is called for each Message response we receive from the cvs server when we are 
        /// processing the description
        /// </summary>
        public void OnMessageRevision(string message)
        {
            if (message.StartsWith(revisionPrefix) && curLogRevision.Revision.Length == 0) {
                curLogRevision.Revision = message.Substring(revisionPrefix.Length);
            } else if (message.StartsWith(datePrefix) && curLogRevision.Author.Length == 0) {
                ExtractDateAndAuthor(message);
            } else if (message.StartsWith(branchesPrefix) && curLogRevision.Branches.Length == 0) {
                curLogRevision.Branches = message.Substring(branchesPrefix.Length);
            } else {
                // assume this is part of the comment
                if (curLogRevision.Comment.Length > 0) {
                    curLogRevision.Comment += Environment.NewLine;
                }
                curLogRevision.Comment += message;
            }
        }
      
        /// <summary>
        /// Extracts all required information from the synbolic line
        /// and add a new entry for it to the SymbolicNames collection
        /// </summary>
        private void ExtractSymbolicName(string message)
        {
            // line is of form:
            // '\t<symbolic name>: <revision>'
            
            // locate the ':' separating the symbolic name from the revision
            int idx = message.IndexOf(':');
            if (idx > 0 && idx < message.Length - 1)
            {
                string name = message.Substring(0, idx).Trim();
                string revision = message.Substring(idx + 1).Trim();
            
                LogSymbolicName symbolicName = new LogSymbolicName(name, revision);
                curLogFile.SymbolicNames.AddSymbolicName(symbolicName);
            }
        }
        
        /// <summary>
        /// Extracts all required information from the date line.
        /// </summary>
        private void ExtractDateAndAuthor(string message)
        {
            // date line is of form:
            // 'date: yyyy/mm/dd hh:mm:ss; author: <author>;  state: <state>;  lines: +<added> -<deleted>'
            const int IDX_DATE = 1;
            const int IDX_TIME = 2;
            const int IDX_AUTHOR = 4;
            const int IDX_STATE = 6;
            const int IDX_LINES1 = 8;
            const int IDX_LINES2 = 9;
            
            // make things a bit easier be removing the ';' chars 
            // and replace all double spaces with a single space
            string cleanedMessage = message.Replace(";", "").Replace("  ", " ");
            string[] tokens = cleanedMessage.Split(" ".ToCharArray());
 
//            for (int i = 0; i < tokens.Length; i++)
//            {
//                System.Console.WriteLine("[{0}]='{1}'", i, tokens[i]);
//            }
            // get the date
            if (tokens.Length > IDX_TIME)
            {
                string date = String.Format("{0} {1}", tokens[IDX_DATE], tokens[IDX_TIME]);
                curLogRevision.Timestamp = DateTime.Parse(date);
            }
            
            // get the author
            if (tokens.Length > IDX_AUTHOR)
            {
                curLogRevision.Author = tokens[IDX_AUTHOR];
            }
           
            // get the state
            if (tokens.Length > IDX_STATE)
            {
                curLogRevision.State = tokens[IDX_STATE];
            }
            
            // get number of lines added / deleted
            for (int idx = IDX_LINES1; idx <= IDX_LINES2; idx++)
            {
                if (tokens.Length > idx)
                {
                    try
                    {
                        int num = Int32.Parse(tokens[idx]);
                        if (num < 0)
                        {
                            curLogRevision.LinesDeleted = - num;
                        }
                        else
                        {
                            curLogRevision.LinesAdded = num;
                        }
                    }
                    catch (Exception)
                    {
                        // don't expect a non-number, but we'll ignore it if we get one
                    }
                }
            }
        }
    }
}

