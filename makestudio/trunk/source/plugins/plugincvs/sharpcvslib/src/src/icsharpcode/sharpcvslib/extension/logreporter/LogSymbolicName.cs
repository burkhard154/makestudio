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
	/// Represents a single symbolic name of a single file from a LogReport
	/// </summary>
	/// <remarks>
	/// 	created by - gne
	/// 	created on - 04/09/2004 12:16:13
	/// </remarks>
    [Author("Gerald Evans", "gne@users.sourceforge.net", "2004")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
	public class LogSymbolicName : object {
        private string name;
		/// <summary>
		/// The symbolic name
		/// </summary>
        public string Name {
            get { return name; }
 	        set { name = value; }
        }
       
        private string revision;
		/// <summary>
		/// The revision number
		/// </summary>
        public string Revision {
            get { return revision; }
 	        set { revision = value; }
        }
		
		/// <summary>
		/// Default constructor - initializes all fields to default values
		/// </summary>
		public LogSymbolicName()
		{
		    this.name = "";
			this.revision = "";
		}
		/// <summary>
		/// State constructor - initializes all fields
		/// </summary>
		public LogSymbolicName(string name, 
		                       string revision)
		{
//System.Console.WriteLine("LogSymbolicName({0}, {1})", name, revision);
		    this.name = name;
		    this.revision = revision;
		}
		
		/// <summary>
		/// ToString() for debugging etc.
		/// </summary>
		public override string ToString()
		{
		    return String.Format("[Name={0}, Revision={1}]",
		                         name, revision);
		}
	}
}
