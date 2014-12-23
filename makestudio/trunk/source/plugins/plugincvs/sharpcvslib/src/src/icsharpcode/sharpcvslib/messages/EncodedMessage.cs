#region "Copyright"
// EncodedMessage.cs
// Copyright (C) 2003 Clayton Harbour
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

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Messages {
    /// <summary>
    /// Class to handle messaging events.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class EncodedMessage {
        private ILog LOGGER = LogManager.GetLogger (typeof (EncodedMessage));
        /// <summary>
        /// A message handler that operates as an interface for a messaging event.
        /// </summary>
        /// <param name="message">A message to send to the implementing class.</param>
        public delegate void MessageHandler(string message);

        /// <summary>
        /// The message event handler that is used to channel messages to the
        ///     delegate and implementing classes.
        /// </summary>
        public event MessageHandler MessageEvent;

        /// <summary>
        /// Send a message to the delegate.
        /// </summary>
        /// <param name="message">The message to send to the delegate.</param>
        public void SendMessage (String message) {
            if (null != MessageEvent && 
                null != message && 
                message.Length != 0) {
                MessageEvent(message);
            }
        }

        /// <summary>
        /// Send a message to the delegate.
        /// </summary>
        /// <param name="message">A message to send to the delegate.</param>
        public void SendMessage (StringBuilder message) {
            if (null != MessageEvent && 
                null != message) {
                this.SendMessage (message.ToString ());
            }
        }
    }

}
