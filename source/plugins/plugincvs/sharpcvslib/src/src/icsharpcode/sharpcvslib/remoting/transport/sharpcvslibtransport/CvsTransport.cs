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
// Adapted from the TcpTransport and PipeTransport classes in the
//  DocVisions Remoting library (http://sourceforge.net/projects/dvremoting/).
//      Copyright (c) 2004 Digital Design. All rights reserved.
//
//      Author: Vadim Skipin (skipin@digdes.com)
//
#endregion "Copyright"

using System;
using System.Collections;
using System.Text.RegularExpressions;
using System.Net;
using System.Net.Sockets;
using System.Security.Principal;

using DocsVision.Security;
using DocsVision.Runtime.Remoting.Transport;

namespace ICSharpCode.SharpCvsLib.Remoting.Transport.SharpCvsLibTransport {
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2004-2005")]
	public class CvsTransport : ITransport, IAsyncTransport {
		private Socket _socket;
		private string _url;

		public CvsTransport() : this(new Socket(AddressFamily.InterNetwork, 
            SocketType.Stream, ProtocolType.Tcp)) {
		}

		public CvsTransport(Socket socket) {
            if (socket == null) {
                throw new ArgumentNullException("socket");
            }

            if ((socket.AddressFamily != AddressFamily.InterNetwork) || 
                socket.ProtocolType != ProtocolType.Tcp) {
                throw new NotSupportedException("Only TCP connections supported");
            }

			_socket = socket;

			// disable nagle delays
			_socket.SetSocketOption(SocketOptionLevel.Tcp, SocketOptionName.NoDelay, 1);

			// set linger option
			LingerOption lingerOption = new LingerOption(true, 3);
			socket.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.Linger, lingerOption);
		}

		public bool IsConnected {
			get { return _socket.Connected; }
		}

		public bool IsLocal {
			get { 
                return (IPAddress.Loopback.Equals(
                    ((IPEndPoint)_socket.RemoteEndPoint).Address));
			}
		}

		public string Url {
			get { return _url; }
		}

		public IPrincipal ClientPrincipal {
			get { 
                return null; // not supported 
            }
		}

		public void Connect(string url) {
			_socket.Connect(GetSocketAddress(url));
			_url = url;
		}

		public void Bind(string url) {
			_socket.Bind(GetSocketAddress(url));
			_url = url;
		}

		public void BindAuth(string url, SecurityDescriptor securityDescriptor) {
			throw new NotSupportedException();
		}

		public void Listen(int backLog) {
			_socket.Listen(backLog);
		}

		public ITransport Accept() {
			return new CvsTransport(_socket.Accept());
		}

		public void Send(byte[] buffer, int offset, int size) {
			_socket.Send(buffer, offset, size, SocketFlags.None);
		}

		public int Receive(byte[] buffer, int offset, int size) {
			return _socket.Receive(buffer, offset, size, SocketFlags.None);
		}

		public int Peek(byte[] buffer, int offset, int size) {
			return _socket.Receive(buffer, offset, size, SocketFlags.Peek);
		}

		public void Flush() {
			// nothing to do
		}

		public void Close() {
			_socket.Close();
		}

		public IAsyncResult BeginReceive(byte[] buffer, int offset, int size, 
            AsyncCallback callback, object state) {
			return _socket.BeginReceive(buffer, offset, size, 
                SocketFlags.None, callback, state);
		}

		public int EndReceive(IAsyncResult result) {
			return _socket.EndReceive(result);
		}

		public IAsyncResult BeginSend(byte[] buffer, int offset, int size, 
            AsyncCallback callback, object state) {
			return _socket.BeginSend(buffer, offset, size, SocketFlags.None, 
                callback, state);
		}

		public void EndSend(IAsyncResult result) {
			_socket.EndSend(result);
		}

		public Socket Socket {
			get { return _socket; }
		}

		public static bool ParseUrl(string url, out IDictionary parts) {
            parts = new Hashtable(4);
            try {
                ICSharpCode.SharpCvsLib.Misc.CvsRoot root = 
                    new ICSharpCode.SharpCvsLib.Misc.CvsRoot(url);
                parts["schema"] = root.TransportProtocol.ToString();
                parts["host"] = root.Host;
                parts["port"] = root.Port;
                parts["requestUri"] = root.CvsRepository;
                return true;
            } catch (Misc.CvsRootParseException) {
                return false;
            }
		}

		private static IPEndPoint GetSocketAddress(string url) {
			IDictionary parts = null;
            if (!ParseUrl(url, out parts)) {
                throw new ArgumentException("Invalid CVS Root specified");
            }

			IPHostEntry hostInfo = Dns.Resolve((string)parts["host"]);
			int port = (int)parts["port"];

			IPEndPoint socketAddress = 
                new IPEndPoint(hostInfo.AddressList[0], port);
			return socketAddress;
		}
	}
}