#region "Copyright"
// Copyright (C) 2004 Clayton Harbour
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
//  <author>Clayton Harbour</author>
//
#endregion

using System;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Messages;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Streams;

namespace ICSharpCode.SharpCvsLib.Protocols {
	/// <summary>
	/// Handle connect and authentication for the pserver protocol.
	/// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2004-2005")]
	public interface IProtocol {

        /// <summary>
        /// Message sent when client has successfully connected to the cvs server.
        /// </summary>
        event MessageEventHandler ConnectedMessageEvent;
        /// <summary>
        /// Message sent when client has successfully disconnected from the cvs server.
        /// </summary>
        event MessageEventHandler DisconnectedMessageEvent;
        /// <summary>
        /// Optional password field.
        /// </summary>
        string Password {get;set;}

        /// <summary>
        /// Repository information used to get cvs server connection.
        /// </summary>
        WorkingDirectory Repository {get;set;}

        /// <summary>
        /// Input stream that allows sending messages to the cvs server.
        /// </summary>
        CvsStream InputStream {get;}
        /// <summary>
        /// Output stream that accepts messages from the cvs server.
        /// </summary>
        CvsStream OutputStream {get;}

        /// <summary>
        /// Connect to the remote cvs server.
        /// </summary>
        /// <exception cref="ICSharpCode.SharpCvsLib.Exceptions.AuthenticationException">If the client does not have the
        /// have the proper credentials to access the repository.</exception>
        void Connect();

        /// <summary>
        /// Disconnect and perform any resource clean-up necessary.
        /// </summary>
        void Disconnect();
	}
}
