#region "Copyright"
// EntryRequest.cs
// Copyright (C) 2001 Mike Krueger
// comments are taken from CVS Client/Server reference manual which
// comes with the cvs client (www.cvshome.org)
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
// As a special exception, if you link this library with other files to
// produce an executable, this library does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why the
// executable file might be covered by the GNU General Public License.
//
//    Author:     Mike Krueger,
//                Clayton Harbour  {claytonharbour@sporadicism.com}
#endregion

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.FileSystem;

namespace ICSharpCode.SharpCvsLib.Requests {
    /// <summary>
    /// Response expected: no.
    /// Tell the server what version of a file is on the local machine.
    /// The name in entry-line is a name relative to the directory most
    /// recently specified with Directory.
    ///
    /// If the user is operating on only some files in a directory,
    /// Entry requests for only those files need be included.
    ///
    /// If an Entry request is sent without Modified, Is-modified,
    /// or Unchanged, it means the file is lost (does not exist in the
    /// working directory).
    ///
    /// If both Entry and one of Modified, Is-modified, or Unchanged are sent for the
    /// same file, Entry must be sent first. For a given file, one can send Modified,
    /// Is-modified, or Unchanged, but not more than one of these three.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class EntryRequest : AbstractRequest {
        private Entry entry;

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="entry"></param>
        public EntryRequest(Entry entry)
        {
            this.entry = entry;
        }

        /// <summary>
        /// Specify the file revision that resides on a local machine.
        /// </summary>
        public override string RequestString {
            get {
                return "Entry " + entry.FileContents + "\n";
            }
        }

        /// <summary>
        /// <code>false</code>, a response is not expected.
        /// </summary>
        public override bool IsResponseExpected {
            get {
                return false;
            }
        }
    }
}
