#region "Copyright"
// AbstractRequest.cs
// Copyright (C) 2001 Mike Krueger
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
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Util;

namespace ICSharpCode.SharpCvsLib.Requests {
    /// <summary>
    /// To be implemented by server requests.  Requests are used
    ///     by commands to communicate with the cvs server.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public abstract class AbstractRequest : IRequest {
        /// <summary>
        /// The string to send to the server.
        /// </summary>
        public abstract string RequestString {
            get;
        }

        /// <summary>
        /// Indicates whether a response is expected from
        ///     this request.  <code>true</code> if a response
        ///     is expected, <code>false</code> otherwise.
        /// </summary>
        public abstract bool IsResponseExpected {
            get;
        }

        /// <summary>
        /// Indicates whether the response modifies the input
        ///     stream.  <code>true</code> if the input stream
        ///     is modified, <code>false</code> otherwise.
        /// </summary>
        public virtual bool DoesModifyInputStream {
            get {
                return false;
            }
        }

        /// <summary>
        /// Indicates whether the response modifies the cvs
        ///     connection.  <code>true</code> if the connection
        ///     is modified, <code>false</code> otherwise.
        /// </summary>
        public virtual bool DoesModifyConnection {
            get {
                return false;
            }
        }

        /// <summary>
        /// Implement this command if your request does modify the
        ///     input cvs server connection.
        /// </summary>
        /// <param name="connection"></param>
        public virtual void ModifyConnection(IConnection connection) {

        }

        /// <summary>
        /// Converts the object values into a human readable string.
        /// </summary>
        /// <returns>A string representation of the object.</returns>
        public override string ToString () {
            ToStringFormatter formatter =
                new ToStringFormatter ("Abstract Request");
            formatter.AddProperty ("DoesModifyConnection", this.DoesModifyConnection);
            formatter.AddProperty ("DoesModifyInputStream", this.DoesModifyInputStream);
            formatter.AddProperty ("IsResponseExpected", this.IsResponseExpected);
            formatter.AddProperty ("RequestString", this.RequestString);

            return formatter.ToString ();
        }
    }
}
