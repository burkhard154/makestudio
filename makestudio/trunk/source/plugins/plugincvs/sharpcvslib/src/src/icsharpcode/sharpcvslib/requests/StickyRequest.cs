#region "Copyright"
// StickyRequest.cs
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
#endregion

using ICSharpCode.SharpCvsLib.Attributes;
namespace ICSharpCode.SharpCvsLib.Requests {

    /// <summary>
    /// Response expected: no.
    /// Tell the server that the directory most recently specified with Directory
    /// has a sticky tag or date tagspec. The first character of tagspec is `T' for
    /// a tag, or `D' for a date. The remainder of tagspec contains the actual tag or
    /// date. The server should remember Static-directory and Sticky requests for a
    /// particular directory; the client need not resend them each time it sends a
    /// Directory request for a given directory. However, the server is not obliged
    /// to remember them beyond the context of a single command.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class StickyRequest : AbstractRequest
    {
        private string tag;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="tag">The sticky tag associated with the most recent file.</param>
        public StickyRequest(string tag)
        {
            this.tag = tag;
        }

        /// <summary>
        /// Notify the server that the most recent directory/ file specified has
        ///     a sticky tag associated with it.
        /// </summary>
        public override string RequestString {
            get {
                return "Sticky " + tag + "\n";
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
