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
using System.Collections;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Exceptions;

namespace ICSharpCode.SharpCvsLib.Protocols {
	/// <summary>
	/// Summary description for ProtocolFactory.
	/// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2004-2005")]
	public class ProtocolFactory {
        /// <summary>
        /// An instance of the protocol factory.
        /// </summary>
        public static ProtocolFactory Instance = new ProtocolFactory();

        private Hashtable Protocols = new Hashtable();

		public ProtocolFactory() {
            PopulateProtocols();
		}

        private void PopulateProtocols() {
            foreach (Type type in this.GetType().Assembly.GetTypes()) {
                if (type.IsAbstract) {
                    continue;
                }
                if (type.IsSubclassOf(typeof(AbstractProtocol))) {
                    ProtocolAttribute protocolAttribute = 
                        (ProtocolAttribute)type.GetCustomAttributes(typeof(ProtocolAttribute), false)[0];
                    IProtocol protocol = (IProtocol)Activator.CreateInstance(type, false);
                    this.Protocols.Add(protocolAttribute.Protocol, protocol);
                }
            }
        }

        /// <summary>
        /// Indicates if the protocol exists.
        /// </summary>
        /// <param name="protocol"><see langword="true"/> if the protocol exists,
        /// otherwise <see langword="false"/>.</param>
        public bool Exists(string protocol) {
            if (this.Protocols.Contains(protocol)) {
                return true;
            }
            return false;
        }

        /// <summary>
        /// Get the specified protocol.  If the protocol does not exist an exception is
        /// thrown.
        /// </summary>
        /// <param name="protocolName">String value of the protocol to get.</param>
        /// <returns>The requested implementation of <see cref="IProtocol"/>.</returns>
        /// <exception cref="UnsupportedProtocolException">If the specified protocol does
        /// not exist.</exception>
        public IProtocol GetProtocol(string protocolName) {
            if (!this.Exists(protocolName)) {
                throw new UnsupportedProtocolException (
                    string.Format("Unknown protocol=[{0}]", protocolName));
            }
            object protocol = this.Protocols[protocolName];
            return (IProtocol)protocol;
        }
	}
}
