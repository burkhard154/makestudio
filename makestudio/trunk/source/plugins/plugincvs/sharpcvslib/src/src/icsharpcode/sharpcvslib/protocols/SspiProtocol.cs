#region "Copyright"
// Copyright (C) Clayton Harbour
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
using System.IO;
using System.Text;
using System.Diagnostics;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Config;
using ICSharpCode.SharpCvsLib.Exceptions;
using ICSharpCode.SharpCvsLib.Messages;
using ICSharpCode.SharpCvsLib.Streams;
using ICSharpCode.SharpCvsLib.Remoting.Transport.SharpCvsLibTransport;

using log4net;

using DocsVision.Security.SSPI;
using DocsVision.Runtime.Remoting.Transport;

namespace ICSharpCode.SharpCvsLib.Protocols {
	/// <summary>
	/// Handle connect and authentication for the pserver protocol.
	/// </summary>
	[Protocol("sspi")]
	public class SspiProtocol : AbstractProtocol, IProtocol {
        private readonly ILog LOGGER =
            LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

        private static IAuthModule authModule;
        private static SecurityCredentials clientCredentials;

        /// <summary>
        /// Create a new instance of the sspi protocol.
        /// </summary>
		public SspiProtocol() {
		}

        /// <summary>
        /// Connect to the cvs server using the ext method.
        /// </summary>
        public override void Connect() {
            this.HandleSspiAuthentication();
        }

        /// <summary>
        /// Disconnect from the cvs server.
        /// </summary>
        public override void Disconnect() {
            throw new NotImplementedException("TODO: Implement me.");
        }


        private void HandleSspiAuthentication () {
            string retStr = this.SendSspiAuthentication();

            if (retStr.Equals("I LOVE YOU")) {
                this.SendConnectedMessageEvent(this, 
                    new MessageEventArgs("Connection established"));
            } else if (retStr.Equals("I HATE YOU")) {
                this.SendDisconnectedMessageEvent(this, new MessageEventArgs("Invalid username or password"));
                throw new AuthenticationException();
            } else {
                StringBuilder msg = new StringBuilder ();
                msg.Append("Unknown Server response : >").Append(retStr).Append("<");
                this.SendDisconnectedMessageEvent(this, new MessageEventArgs(msg.ToString()));
                throw new AuthenticationException(msg.ToString());
            }   
        }

        private enum EncryptionType {
            Digest, Kerberos, NTLM, Negotiate, Schannel
        }

        private EncryptionType CurrentEncryptionType = EncryptionType.NTLM;

        private string SendSspiAuthentication () {
            try {
                // initialize network transport
                TransportClient client = 
                    new TransportClient(this.Repository.CvsRoot.ToString(), 
                    typeof(CvsTransport));

                this.SetInputStream(new CvsStream(client.GetStream()));
                this.SetOutputStream(this.InputStream);

                this.OutputStream.SendString("BEGIN SSPI\n");
                string[] names = System.Enum.GetNames(typeof(EncryptionType));
                string protocols = string.Empty;
                for (int i = 0; i < names.Length; i++) {
                    protocols += names[i];
                    if (i + 1 < names.Length) {
                        protocols += ",";
                    }
                }
                this.OutputStream.SendString(string.Format("{0}\n", protocols));

                string authTypeResponse = this.InputStream.ReadLine();
                CurrentEncryptionType = (EncryptionType)
                    System.Enum.Parse(typeof(EncryptionType), authTypeResponse);

                // initialize authorization module
                authModule = 
                    new NTAuthModule(new SecurityPackage(CurrentEncryptionType.ToString()));

                // acquire client credentials
                clientCredentials = 
                    authModule.AcquireSecurityCredentials(SecurityCredentialsType.OutBound, null);

                byte[] clientToken;
                byte[] serverToken;

                // create client context
                SecurityContext clientContext = 
                    authModule.CreateSecurityContext(clientCredentials, 
                    SecurityContextAttributes.Identify, null, out clientToken);

                while (true) {
                    if (clientToken != null) {
                        // send client token to server
                        string clientTokenString = 
                            Encoding.ASCII.GetString(clientToken, 54, 57);
                        this.OutputStream.SendString(
                            clientTokenString);
                    }

                    if (clientContext.State == SecurityContextState.Completed) {
                        // authentication completed
                        break;
                    }

                    // receive server token
                    serverToken = 
                        Encoding.ASCII.GetBytes(this.InputStream.ReadToFirstWS());

                    // update security context
                    authModule.UpdateSecurityContext(clientContext, 
                        SecurityContextAttributes.Identify, serverToken, out clientToken);
                }

//                AuthenticateClient(client);

                return InputStream.ReadLine();
            } catch (IOException e) {
                String msg = "Failed to read line from server.  " +
                    "It is possible that the remote server was down.";
                LOGGER.Error (msg, e);
                throw new AuthenticationException (msg);
            }
        }

        static SecurityContext AuthenticateClient(TransportClient client) {
            TransportStream stream = client.GetStream();

            byte[] clientToken;
            byte[] serverToken;

            // create client context
            SecurityContext clientContext = authModule.CreateSecurityContext(clientCredentials, SecurityContextAttributes.Identify, null, out clientToken);

            while (true) {
                if (clientToken != null) {
                    // send client token to server
                    SendBuffer(stream, clientToken);
                }

                if (clientContext.State == SecurityContextState.Completed) {
                    // authentication completed
                    break;
                }

                // receive server token
                serverToken = ReceiveBuffer(stream);

                // update security context
                authModule.UpdateSecurityContext(clientContext, SecurityContextAttributes.Identify, serverToken, out clientToken);
            }

            return clientContext;
        }

        static byte[] ReceiveBuffer(TransportStream stream) {	
            BinaryReader reader = new BinaryReader(stream);

            // receive buffer size
            int size = reader.ReadInt32();

            if (size > 0) {
                // receive buffer data
                byte[] buffer = new byte[size];
                reader.Read(buffer, 0, size);
                return buffer;
            }
            else {
                return null;
            }
        }

        static void SendBuffer(TransportStream stream, byte[] buffer) {
            BinaryWriter writer = new BinaryWriter(stream);

            if ((buffer != null) && (buffer.Length > 0)) {
                // send buffer size
                writer.Write((Int32)buffer.Length);

                // send buffer data
                writer.Write(buffer);
            }
            else {
                // send NULL size
                writer.Write((Int32)0);
            }
        }
	}
}
