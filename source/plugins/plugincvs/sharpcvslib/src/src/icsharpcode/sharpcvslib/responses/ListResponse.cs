#region "Copyright"
// ListResponse.cs
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
#endregion

using System;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Streams;

using log4net;

namespace ICSharpCode.SharpCvsLib.Responses {
    /// <summary>
    /// Handle a list files response from the cvs server.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class ListResponse : AbstractResponse {
        private static bool isHandling = false;
        /// <summary>
        /// Indicate if this response is handling the error response message from the
        /// server.
        /// </summary>
        /// <value><code>true</code> if it is handling the response, otherwise 
        ///     <code>false</code>.</value>
        public static bool IsHandling {
            get {return isHandling;}
            set {isHandling = value;}
        }

        private string message;
        /// <summary>
        /// Message being delegated from another response.
        /// </summary>
        public string DelegateMessage {
            get {return this.message;}
            set {this.message = value;}
        }

        private readonly ILog LOGGER =
            LogManager.GetLogger (typeof (ListResponse));
        /// <summary>
        /// Process the list files response.
        /// </summary>
        public override void Process() {
            if (this.message == null) {
                this.message = this.ReadLine();
            }
            if (this.DelegateMessage.Equals("Listing modules on server")) {
                this.DelegateMessage = string.Format("\n{0}\n", this.DelegateMessage);
            }
            this.DelegateMessage = string.Format("{0}", message.Replace("M ", ""));
            Services.ResponseMessageEvents.SendResponseMessage(message, this.GetType());
        }

        /// <summary>
        /// Return true if this response cancels the transaction
        /// </summary>
        public override bool IsTerminating {
            get {return true;}
        }
    }
}
