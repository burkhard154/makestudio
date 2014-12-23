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
using System.Xml;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Extension.ChangeLogReport {
    /// <summary>
    /// Represents a single cvs checkin which may consist of
    /// multiple files.  
    /// </summary>
    [Author("Gerald Evans", "gne@users.sourceforge.net", "2003")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class LogEntry : IComparable {
        DateTime date;
        string author;
        string comment;
        ArrayList fileRevisions = new ArrayList();
    	
    	/// <summary>
    	/// constructor
    	/// </summary>
    	public LogEntry(DateTime date, string author, string comment) {
    	    this.date = date;
    	    this.author = author;
    	    this.comment = comment;
    	}
    	
    	/// <summary>
    	/// The key is based on date/author/comment
    	/// date is first, so it sorts by date first
    	/// The "s" format specifier produces a sortable date
    	/// </summary>
    	public string Key {
    	    get {
    	        return date.ToString("s") + author + "\n" + comment;
    	    }
    	}
    	/// <summary>
    	/// Allow sorting by Key
    	/// </summary>
    	int IComparable.CompareTo(object o) {
    	    LogEntry rhs = (LogEntry)o;
    	    return Key.CompareTo(rhs.Key);
    	}
    
    	/// <summary>
    	/// HashCode is based on date/author/comment
    	/// TODO: Don't know if we are going to need this?
    	/// </summary>
    	public override int GetHashCode() {
    	    return Key.GetHashCode();
    	}
    	
    	/// <summary>
    	/// Adds details about a file revision to the entry
    	/// </summary>
    	public void AddFileRevision(string fnm, string revision, string previousRevision) {
    	    FileRevision fileRevision = new FileRevision(fnm, revision, previousRevision);
    	    fileRevisions.Add(fileRevision);
    	}
    
    	/// <summary>
    	/// Appends details about this change to the XML file
    	/// </summary>
    	public void ExportToXml(XmlTextWriter xmlWriter, StringDictionary nameMap) {
    	    string displayName;
    	    
    	    xmlWriter.WriteStartElement("entry");
    	    
    	    xmlWriter.WriteStartElement("date");
    	    string outputDateFormat = "yyyy'-'MM'-'dd";
    	    xmlWriter.WriteString(date.ToString(outputDateFormat));
    	    xmlWriter.WriteEndElement();
    	    
    	    xmlWriter.WriteStartElement("time");
    	    string outputTimeFormat = "HH':'mm";
    	    xmlWriter.WriteString(date.ToString(outputTimeFormat));
    	    xmlWriter.WriteEndElement();
    	    		    
    	    xmlWriter.WriteStartElement("author");
    	    // Check if we have the authors real name
    	    if (nameMap.ContainsKey(author)) {
    	        displayName = nameMap[author];
    	    } else {
    	        // no - just display the authors user ID
    	        displayName = author;
    	    }
    	    xmlWriter.WriteCData(displayName);
    	    xmlWriter.WriteEndElement();
    	    
    	    foreach (object o in fileRevisions) {
    	        FileRevision fileRevision = (FileRevision)o;
    	        fileRevision.ExportToXml(xmlWriter);
    	    }
    	    		
    	    if (comment != null && comment.Length != 0) {
    		    xmlWriter.WriteStartElement("msg");
    		    xmlWriter.WriteCData(comment);
    		    xmlWriter.WriteEndElement();
    	    }
    
    	    xmlWriter.WriteEndElement();    // "file"
    	}
    }
}
