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
using log4net;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Messages;

namespace ICSharpCode.SharpCvsLib.Responses {
	/// <summary>
	/// Encapsulates the messages that can be triggered by a cvs server response.
	/// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
	public class ResponseMessageEvents {
        private readonly ILog LOGGER = LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);
        /// <summary>
        /// Occurs when a file is being updated from the repository.
        /// </summary>
        public event MessageEventHandler UpdatedResponseMessageEvent;
        /// <summary>
        /// Occurs when a <see cref="Responses.SetStaticDirectoryResponse"/> event is sent
        /// from the server.
        /// </summary>
        public event MessageEventHandler SetStaticDirectoryResponseMessageEvent;
        /// <summary>
        /// Occurs when a <see cref="Responses.ClearStaticDirectoryResponse"/> event is sent
        /// from the server.
        /// </summary>
        public event MessageEventHandler ClearStaticDirectoryResponseMessageEvent;
        /// <summary>
        /// Occurs when a <see cref="ICSharpCode.SharpCvsLib.Responses.ErrorResponse"/> is sent
        /// from the cvs server.
        /// </summary>
        public event MessageEventHandler ErrorResponseMessageEvent;
        /// <summary>
        /// Occurs when a <see cref="ICSharpCode.SharpCvsLib.Responses.ErrorMessageResponse"/> 
        /// is sent from the cvs server and the message sent down is 
        /// <code></code>
        /// </summary>
        public event MessageEventHandler ListResponseMessageEvent;
        /// <summary>
        /// Send a generic response message event.  Used for all responses that are not needed for now, 
        /// however if used often enough the response will be broken out into it's own specific response 
        /// event.
        /// </summary>
        public event MessageEventHandler UnspecifiedResponseMessageEvent;

        /// <summary>
        /// Listen for generic message events from the server.
        /// </summary>
        public event MessageEventHandler MessageResponseMessageEvent;

        /// <summary>
        /// Default constructor.
        /// </summary>
		public ResponseMessageEvents() {
		}

        /// <summary>
        /// Send a message event to the specific event handler signalling that a <see cref="IResponse"/>
        /// has been recieved from the cvs server.
        /// </summary>
        /// <param name="message">Message to send to clients.</param>
        /// <param name="responseType">The <see cref="IResponse"/> type that is sending
        /// the message.</param>
        public void SendResponseMessage (string message, Type responseType) {
            if (responseType.IsSubclassOf(typeof(IResponse))) {
                throw new ArgumentException(String.Format("Response message must be sent from type of {0}; was sent from {1}.",
                    (typeof(IResponse)).FullName, responseType.FullName));
            }

            if (responseType == typeof(UpdatedResponse)) {
                if (null != this.UpdatedResponseMessageEvent) {
                    this.UpdatedResponseMessageEvent(this, new MessageEventArgs(message, MessageEventArgs.DEFAULT_PREFIX));
                }
            } else if (responseType == typeof(SetStaticDirectoryResponse)) {
                if (null != this.SetStaticDirectoryResponseMessageEvent) {
                    this.SetStaticDirectoryResponseMessageEvent(this, new MessageEventArgs(message, MessageEventArgs.SERVER_PREFIX));
                }
            }  else if (responseType == typeof(ClearStaticDirectoryResponse)) {
                if (null != this.ClearStaticDirectoryResponseMessageEvent) {
                    this.ClearStaticDirectoryResponseMessageEvent(this, 
                        new MessageEventArgs(message, MessageEventArgs.SERVER_PREFIX));
                }
            } else if (responseType == typeof(ErrorResponse) || responseType == typeof(ErrorMessageResponse)) {
                if (null != ErrorResponseMessageEvent) {
                    this.ErrorResponseMessageEvent(this, new MessageEventArgs(message, MessageEventArgs.ERROR_PREFIX));
                }
            } else if (responseType == typeof(ListResponse)) {
                if (null != ListResponseMessageEvent) {
                    this.ListResponseMessageEvent(this, new MessageEventArgs(message, MessageEventArgs.EMPTY_PREFIX));
                }
            } else if (responseType == typeof(MessageResponse)) {
                if (null != MessageResponseMessageEvent) {
                    this.MessageResponseMessageEvent(this, new MessageEventArgs(message, MessageEventArgs.EMPTY_PREFIX));
                }
            }
            else {
                if (null != this.UnspecifiedResponseMessageEvent) {
                    this.UnspecifiedResponseMessageEvent(this, new MessageEventArgs(message, MessageEventArgs.DEFAULT_PREFIX));
                }
            } 

        }
	}
}
