#region "Copyright"
// RemoveRequest.cs
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
    /// Response expected: yes. Remove a file.
    /// This uses any previous Argument, Directory, Entry, or Modified requests, if they have been sent.
    /// The last Directory sent specifies the working directory at the time of the operation. Note that
    /// this request does not actually do anything to the repository; the only effect of a successful
    /// remove request is to supply the client with a new entries line containing `-' to indicate a removed
    /// file. In fact, the client probably could perform this operation without contacting the server, although
    /// using remove may cause the server to perform a few more checks. The client sends a subsequent ci request
    /// to actually record the removal in the repository.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class RemoveRequest : AbstractRequest
    {
        /// <summary>
        /// Request that a file/ directory be removed.
        /// </summary>
        public override string RequestString {
            get {
                return "remove\n";
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
