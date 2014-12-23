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
	using System.Collections;
	
	
	/// <summary>
	/// Represents a collection of SymbolicNames
	/// </summary>
	/// <remarks>
	/// 	created by - gne
	/// 	created on - 04/09/2004 12:16:13
	/// </remarks>
    [Author("Gerald Evans", "gne@users.sourceforge.net", "2004")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
	public class LogSymbolicNames : IEnumerable {
	    private ArrayList symbolicNames = new ArrayList();
		
		/// <summary>
		/// Adds a LogSymbolicName to the collection
		/// Only called when the LogReport is being constructed
		/// </summary>
		internal void AddSymbolicName(LogSymbolicName symbolicName)
		{
		    symbolicNames.Add(symbolicName);
		}
		
		/// <summary>
		/// Gets an enumerator to enumerate over the symbolic names in the collection
		/// </summary>
		public IEnumerator GetEnumerator()
		{
		    return symbolicNames.GetEnumerator();
		}
		
		/// <summary>
		/// The number of symbolic names in this collection
		/// </summary>
		public int Count {
		    get { return symbolicNames.Count; }
		}
		
		/// <summary>
		/// Indexer to the symbolic names in this collection
		/// </summary>
		public LogSymbolicName this[int index] {
		    get { return (LogSymbolicName)symbolicNames[index]; }
		}
		
		/// <summary>
		/// ToString() for debugging etc.
		/// </summary>
		public override string ToString()
		{
		    return String.Format("[Count={0}]",
		                         Count);
		}
	}
}
