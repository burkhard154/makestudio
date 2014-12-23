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

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Extension.LogReporter {
	using System;
	
	
	/// <summary>
	/// Represents a single revision of a single file from a LogReport
	/// </summary>
	/// <remarks>
	/// 	created by - gne
	/// 	created on - 28/02/2004 15:37:13
	/// </remarks>
    [Author("Gerald Evans", "gne@users.sourceforge.net", "2004")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
	public class LogRevision : object {
        private string revision;
		/// <summary>
		/// The revision number
		/// </summary>
        public string Revision {
            get { return revision; }
 	        set { revision = value; }
       }
        
        private DateTime timestamp;
		/// <summary>
		/// date/time of the revision
		/// </summary>
        public DateTime Timestamp {
	        get { return timestamp; }
	        set { timestamp = value; }
	    }
	    
	    private string author;
		/// <summary>
		/// The author of the revision
		/// </summary>
	    public string Author {
	        get { return author; }
	        set { author = value; }
	    }
	    
	    private string state;
		/// <summary>
		/// TODO: find out what the values of this can be
		/// </summary>
	    public string State {
	        get { return state; }
	        set { state = value; }
	    }
    
        private string comment;
		/// <summary>
		/// Comment describing revision
		/// </summary>
        public string Comment {
            get { return comment; }
	        set { comment = value; }
        }
    
        private int linesAdded;
 		/// <summary>
		/// Number of lines added in this revision
		/// </summary>
        public int LinesAdded {
            get { return linesAdded; }
	        set { linesAdded = value; }
        }
    
        private int linesDeleted;
 		/// <summary>
		/// Number of lines deleted in this revision
		/// </summary>
        public int LinesDeleted {
            get { return linesDeleted; }
	        set { linesDeleted = value; }
        }
	    
	    private string branches;
 		/// <summary>
		/// Revision that was brached from if applicable. 
		/// </summary>
	    public string Branches {
	        get { return branches; }
	        set { branches = value; }
	    }
		
		/// <summary>
		/// Default constructor - initializes all fields to default values
		/// </summary>
		public LogRevision()
		{
		    this.revision = "";
		    this.timestamp = DateTime.Now;
		    this.author = "";
		    this.state = "";
		    this.comment = "";
		    this.linesAdded = 0;
		    this.linesDeleted = 0;
		    this.branches = "";
		}
//		/// <summary>
//		/// State constructor - initializes all fields
//		/// </summary>
//		public LogRevision(string revision, 
//		                   DateTime timestamp,
//		                   string author,
//		                   string comment)
//		{
//System.Console.WriteLine("LogRevision({0}, {1})", revision, author);
//		    this.revision = revision;
//		    this.timestamp = timestamp;
//		    this.author = author;
//		    this.comment = comment;
//		}
		
		/// <summary>
		/// ToString() for debugging etc.
		/// </summary>
		public override string ToString()
		{
		    return String.Format("[Revision={0}, Timestamp={1}, Author={2}, State={3}, LinesAdded={4}, LinesDeleted={5}, Comment={6}]",
		                         revision, timestamp, author, state, linesAdded, linesDeleted);
		}
	}
}
