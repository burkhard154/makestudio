#region "Copyright"
// List.cs
// Copyright (C) 2004 Clayton Harbour
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
    /// Response expected: yes.
    /// Get files from the repository. This uses any previous Argument, Directory, Entry,
    /// or Modified requests, if they have been sent. Arguments to this command are module
    /// names; the client cannot know what directories they correspond to except by (1) just
    /// sending the co request, and then seeing what directory names the server sends back in
    /// its responses, and (2) the expand-modules request.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class ListRequest : AbstractRequest
    {
        /// <summary>
        /// Send request to checkout files from the server.
        /// </summary>
        public override string RequestString {
            get {
                return "ls\n";
            }
        }

        /// <summary>
        /// <code>true</code>, a response is expected.
        /// </summary>
        public override bool IsResponseExpected {
            get {
                return true;
            }
        }
    }
}
