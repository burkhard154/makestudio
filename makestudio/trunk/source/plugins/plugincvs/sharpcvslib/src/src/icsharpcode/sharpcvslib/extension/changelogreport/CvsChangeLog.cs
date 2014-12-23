#region "Copyright"
// Copyright (C) 2003 Gerald Evans
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

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Exceptions;
using ICSharpCode.SharpCvsLib.Extension.LogReporter;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Messages;
using ICSharpCode.SharpCvsLib.Misc;

namespace ICSharpCode.SharpCvsLib.Extension.ChangeLogReport {
    /// <summary>
    /// Produces an XML change log of a cvs project.
    /// Typical usage is as follows:
    /// 
    ///    string password = "password";
	///    string xmlFilename = "C:\\tmp\\output.xml";
    ///	    
    ///    CvsChangeLog cvsChangeLog = new CvsChangeLog("sharpcvslib", "C:\\sharpcvslib");
    ///
    ///    cvsChangeLog.AddNameMapping("gne", "Gerald Evans"); 
    ///    cvsChangeLog.AddNameMapping("drakmar", "Clayton Harbour");
    ///    cvsChangeLog.AddNameMapping("skyward", "Steve Kenzell");
    /// 
    ///    cvsChangeLog.SetLastNDays(7);
    ///    // or cvsChangeLog.StartDate = new DateTime(...);
    ///    // and/or cvsChangeLog.EndDate = new DateTime(...);
    ///	    
    ///    cvsChangeLog.Run(xmlFilename, password);
    ///    // or cvsChangeLog.Run(stdioFile, password)
    ///
    /// </summary>
    [Author("Gerald Evans", "gne@users.sourceforge.net", "2003")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class CvsChangeLog {
        // Name mapping set by caller    
        private StringDictionary nameMap = new StringDictionary();   

        // The LogReportCommand does the real work of getting a LogReport
        private LogReportCommand logCommand;
        
        /// <summary>
        /// ctor
        /// </summary>
        public CvsChangeLog(string module, string localDirectory)
        {
            logCommand = new LogReportCommand(module, localDirectory);
        }

        /// <summary>
        /// Create a new instance of the cvs changelog command.
        /// </summary>
        /// <param name="workingDirectory"></param>
        /// <param name="module"></param>
        public CvsChangeLog(WorkingDirectory workingDirectory, string module) {
            logCommand = new LogReportCommand(workingDirectory, module);
        }
    
        /// <summary>
        /// If set, only report changes on or after this date
        /// </summary>
        public DateTime StartDate {
            get {return logCommand.StartDate;}
        	set {logCommand.StartDate = value;}
        }
            
        /// <summary>
        /// If set, only report changes on or before this date
        /// </summary>
        public DateTime EndDate {
            get {return logCommand.EndDate;}
        	set {logCommand.EndDate = value;}
        }
        
        /// <summary>
        /// Only report changes during the last given number of days.
        /// </summary>
        public void SetLastNDays(int days)
        {
            logCommand.SetLastNDays(days);
        }

        /// <summary>
        /// Adds a single mapping between a user name as used within cvs and
        /// a full users name.
        /// Can be called multiple times, once for each user name to add.
        /// </summary>
        public void AddNameMapping(string id, string fullName)
        {
            nameMap.Add(id, fullName); 
        }
    
        /// <summary>
        /// Produce the report
        /// </summary>
        public void Run(string xmlFilename, string password)
        {
//            try
//            {
                XmlTextWriter textWriter = new XmlTextWriter(xmlFilename, new UTF8Encoding());
                Run(textWriter, password);
//            } catch (Exception e) {
//                System.Console.WriteLine("XML create/write error: {0}", e.Message);
//                throw e;
//            }
        }

        /// <summary>
        /// Produce the report.
        /// </summary>
        /// <param name="xmlFilename"></param>
        /// <param name="connection"></param>
        public void Run(string xmlFilename, ICommandConnection connection) {
            XmlTextWriter textWriter = new XmlTextWriter(xmlFilename, new UTF8Encoding());
            Run(textWriter, connection);
        }
        
        /// <summary>
        /// Produce the report
        /// </summary>
        public void Run(XmlTextWriter textWriter, string password)
        {
            // send the log command to the cvs server and get the LogReport
            LogReport logReport = logCommand.Run(password);
            
            // now format the LogReport into a change log
            FormatReport(textWriter, logReport);
        }
    
        /// <summary>
        /// Produce the report
        /// </summary>
        public void Run(XmlTextWriter textWriter, ICommandConnection connection)
        {
            // send the log command to the cvs server and get the LogReport
            LogReport logReport = logCommand.Run(connection);
            
            // now format the LogReport into a change log
            FormatReport(textWriter, logReport);
        }
   
        /// <summary>
        /// Format the LogReport into a change log
        /// which will be written to textWriter
        /// </summary>
        private void FormatReport(XmlTextWriter textWriter, LogReport logReport)
        {
            LogRevision logRevision;

            // This is where we accumulate information on all the entries from the log command
            SortedList entries = new SortedList();
            LogEntry entry;
            string prevRevision;
            
            // now collect together revisions that were checked-in together
	        foreach (LogFile logFile in logReport)
    	    {
//    	        foreach (LogRevision logRevision in logFile)
    	        // traverse revisions in reverse order so we look at oldest first
    	        // which simplifies the remembering the previous revision
    	        prevRevision = "";
                for (int idx = logFile.Count - 1; idx >= 0; idx--)
    	        {
    	            logRevision = logFile[idx];
                    entry = new LogEntry(logRevision.Timestamp, logRevision.Author, logRevision.Comment);
                    // determine if this entry already exists
                    if (entries.ContainsKey(entry.Key)) {
                        // need to update an existing entry
                        entry = (LogEntry)entries[entry.Key];
                    } else {
                        // add new entry
                        entries.Add(entry.Key, entry);
                    }
                    // finally add details about the file/revision
                    entry.AddFileRevision(logFile.WorkingFnm, logRevision.Revision, prevRevision);
    	            prevRevision = logRevision.Revision;
    	        }
    	    }
               
            // now finally produce the XML report
//            try {
                textWriter.Formatting = Formatting.Indented;
                textWriter.WriteStartDocument();
                textWriter.WriteStartElement("changelog");
                
                // add the entries ...
                foreach (DictionaryEntry de in entries) {
                    LogEntry logEntry = (LogEntry)de.Value;
                    logEntry.ExportToXml(textWriter, nameMap);
                }
                
                // finish off
                textWriter.WriteEndElement();    // changelog
                textWriter.WriteEndDocument();
                textWriter.Close();
//            } catch (Exception e) {
//                System.Console.WriteLine("XML write error: {0}", e.Message);
//                throw e;
//            }
        }
    }
}

