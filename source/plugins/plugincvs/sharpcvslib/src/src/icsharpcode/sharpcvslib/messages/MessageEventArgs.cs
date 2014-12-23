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
#endregion

using System;
using System.Text;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Requests;
using ICSharpCode.SharpCvsLib.Responses;

namespace ICSharpCode.SharpCvsLib.Messages {
	/// <summary>
	/// Message event arguments.
	/// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
	public class MessageEventArgs : EventArgs {
        private string message;
        private string prefix = String.Empty;

        /// <summary>Empty prefix.</summary>
        public const string EMPTY_PREFIX = "";
        /// <summary>Default prefix that is appended to all requests without a 
        /// prefix specified.</summary>
        public const string DEFAULT_PREFIX = EMPTY_PREFIX;
        /// <summary>Prefix that is appended to client requests.</summary>
        public const string CLIENT_PREFIX = "client";
        /// <summary>Prefix that is appended to server responses.</summary>
        public const string SERVER_PREFIX = "cvs server";
        /// <summary>Prefix that is appended to error responses.</summary>
        public const string ERROR_PREFIX = "cvs error";

        /// <summary>
        /// The prefix to decorate the message with.
        /// </summary>
        public string Prefix {
            get {return this.prefix;}
            set {this.prefix = value;}
        }

        /// <summary>
        /// Message.
        /// </summary>
        public string Message{
            get {return this.message;}
            set {this.message = value;}
        }

        /// <summary>
        /// Create a new instance of the message event arugments class.
        /// </summary>
		public MessageEventArgs() {
            this.message = String.Empty;
		}

        /// <summary>
        /// Create a new instance of the message event arguments class.
        /// </summary>
        /// <param name="message">Message to send.</param>
        public MessageEventArgs(string message) :
            this(message, DEFAULT_PREFIX) {
        }

        /// <summary>
        /// Create a new instance of the message event arguments class.
        /// </summary>
        /// <param name="message">Message to send.</param>
        /// <param name="prefix">The prefix to append to the message.</param>
        public MessageEventArgs(string message, string prefix) {
            this.message = message;
            this.prefix = prefix;
        }

        /// <summary>
        /// Create a new message event arguments.
        /// </summary>
        /// <param name="request">An <see cref="ICSharpCode.SharpCvsLib.Requests.IRequest"/> 
        /// object used to construct the message argument.</param>
        /// <param name="message">An additional message to append to the message event.</param>
        public MessageEventArgs(IRequest request, string message) {
            string [] requestMessage = request.RequestString.Split('\n');
            StringBuilder msg = new StringBuilder();
            for (int i = 0; i < requestMessage.Length; i++) {
                if (!(message == "\n")) {
                    if (i > 0) {
                        msg.Append("\n    --> ");
                    }
                    msg.Append(string.Format("[{0}]", requestMessage[i]));                    
                }
            }
            this.message = msg.ToString();
            this.prefix = MessageEventArgs.CLIENT_PREFIX;
        }

        /// <summary>
        /// The response string.
        /// </summary>
        /// <param name="response">An <see cref="ICSharpCode.SharpCvsLib.Responses.IResponse"/>
        /// object used to construct the message argument.</param>
        public MessageEventArgs(IResponse response) : 
            this(response.ResponseString, MessageEventArgs.SERVER_PREFIX) {
        }

        /// <summary>
        /// The response string.
        /// </summary>
        /// <param name="response">An <see cref="ICSharpCode.SharpCvsLib.Responses.IResponse"/>
        /// object used to construct the message argument.</param>
        /// <param name="message">An additional message to append to the message event.</param>
        public MessageEventArgs(IResponse response, string message) : 
            this(String.Format("{0} - ( {1} )", response.ResponseString, message), 
            MessageEventArgs.SERVER_PREFIX) {
        }
	}
}
