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

using ICSharpCode.SharpCvsLib.Config;
using ICSharpCode.SharpCvsLib.Messages;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Streams;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;
namespace ICSharpCode.SharpCvsLib.Protocols {
	/// <summary>
	/// Handle connect and authentication for the pserver protocol.
	/// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
	public abstract class AbstractProtocol : IProtocol {
        /// <summary>
        /// Event that fires when a connection is made to the cvs server.
        /// </summary>
        public event MessageEventHandler ConnectedMessageEvent;
        /// <summary>
        /// Event that fires when a connection is broken to the cvs server.
        /// </summary>
        public event MessageEventHandler DisconnectedMessageEvent;

        private readonly ILog LOGGER =
            LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

        private string password;
        private CvsStream inputStream;
        private CvsStream outputStream;

        private SharpCvsLibConfig config;
        private WorkingDirectory repository;

        /// <summary>
        /// Configuration settings for sharpcvslib.
        /// </summary>
        protected SharpCvsLibConfig Config {
            get {
                if (null == this.config) {
                    this.config = SharpCvsLibConfig.GetInstance();
                }   
                return this.config;
            }
        }

        /// <summary>
        /// Set the internal copy of the input stream.
        /// </summary>
        /// <param name="stream"></param>
        protected void SetInputStream (CvsStream stream) {
            this.inputStream = stream;
        }

        /// <summary>
        /// Set the internal copy of the input stream.
        /// </summary>
        /// <param name="stream"></param>
        protected void SetOutputStream (CvsStream stream) {
            this.outputStream = stream;
        }

        /// <summary>
        /// Send a connected message event to any clients listening.
        /// </summary>
        /// <param name="Object"></param>
        /// <param name="e"></param>
        protected void SendConnectedMessageEvent(object Object, MessageEventArgs e) {
            if (null != this.ConnectedMessageEvent) {
                this.ConnectedMessageEvent(Object, e);
            } 
        }

        /// <summary>
        /// Send notification that the connection has been broken to the cvs server.
        /// </summary>
        /// <param name="Object"></param>
        /// <param name="e"></param>
        protected void SendDisconnectedMessageEvent(object Object, MessageEventArgs e) {
            if (null != this.DisconnectedMessageEvent) {
                this.DisconnectedMessageEvent(Object, e);
            } 
        }

        /// <summary>
        /// Sends messages to the cvs server.
        /// </summary>
        public CvsStream InputStream {
            get {return this.inputStream;}
        }

        /// <summary>
        /// Recieves messaages from the cvs server.
        /// </summary>
        public CvsStream OutputStream {
            get {return this.outputStream;}
        }

        /// <summary>
        /// Optional password field.
        /// </summary>
        public string Password {
            get {return this.password;}
            set {this.password = value;}
        }

        /// <summary>
        /// The working directory or sandbox.
        /// </summary>
        public WorkingDirectory Repository {
            get {return this.repository;}
            set {this.repository = value;}
        }

        /// <summary>
        /// Create a new instance of the ext (ssh) protocol.
        /// </summary>
		public AbstractProtocol() {
		}

        /// <summary>
        /// Handle the connection.
        /// </summary>
        public abstract void Connect ();

        /// <summary>
        /// Handle disconnect from cvs server.
        /// </summary>
        public abstract void Disconnect();
	}
}
