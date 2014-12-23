#region "Copyright"
// DiffRequest.cs
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
    /// Response expected: yes.
    /// Actually do a cvs command. This uses any previous Argument, Directory, Entry,
    /// or Modified requests, if they have been sent. The last Directory sent specifies
    /// the working directory at the time of the operation. No provision is made for any
    /// input from the user. This means that ci must use a -m argument if it wants to specify
    /// a log message.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class DiffRequest : AbstractRequest {
        /// <summary>
        /// Request a comparison between the two revisions of a file.
        /// </summary>
        public override string RequestString {
            get {
                return "diff\n";
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
