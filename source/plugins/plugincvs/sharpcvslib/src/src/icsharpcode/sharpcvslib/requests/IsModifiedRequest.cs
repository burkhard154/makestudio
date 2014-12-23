#region "Copyright"
// IsModifiedRequest.cs
// Copyright (C) 2003 Clayton Harbour
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
//  <author>Clayton Harbour</author>
//
#endregion

using System;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Requests {
	/// <summary>
	/// 
    /// Is-modified filename \n
    ///     Response expected: no. Additional data: none. Like Modified, but used if 
    ///     the server only needs to know whether the file is modified, not the 
    ///     contents. The commands which can take Is-modified instead of Modified 
    ///     with no known change in behavior are: admin, diff (if and only if two 
    ///     `-r' or `-D' options are specified), watch-on, watch-off, watch-add, 
    ///     watch-remove, watchers, editors, log, and annotate. For the status 
    ///     command, one can send Is-modified but if the client is using imperfect 
    ///     mechanisms such as timestamps to determine whether to consider a file 
    ///     modified, then the behavior will be different. That is, if one sends 
    ///     Modified, then the server will actually compare the contents of the file 
    ///     sent and the one it derives from to determine whether the file is 
    ///     genuinely modified. But if one sends Is-modified, then the server takes 
    ///     the client's word for it. A similar situation exists for tag, if the `-c' 
    ///     option is specified. Commands for which Modified is necessary are co, ci, 
    ///     update, and import. Commands which do not need to inform the server about 
    ///     a working directory, and thus should not be sending either Modified or 
    ///     Is-modified: rdiff, rtag, history, init, and release. Commands for which 
    ///     further investigation is warranted are: remove, add, and export. Pending 
    ///     such investigation, the more conservative course of action is to stick to 
    ///     Modified.
    ///     
	/// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class IsModifiedRequest : AbstractRequest {

        private string file;

        /// <summary>
        /// Create a new instance of the Is-Modified request.
        /// </summary>
        /// <param name="file">The name of the file that has been modifed.</param>
        public IsModifiedRequest(string file) {
            this.file = file;
        }

        /// <summary>
        /// Request an update to the files/ directories specified.
        /// </summary>
        public override string RequestString {
            get {
                return "Is-modified " + file + "\n";
            }
        }

        /// <summary>
        /// Response expected: <code>true</code>.
        /// </summary>
        public override bool IsResponseExpected {
            get {return false;}
        }
    }
}
