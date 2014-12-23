#region "Copyright"
// GzipStreamRequest.cs
// Copyright (C) 2001 Mike Krueger
// comments are taken from CVS Client/Server reference manual which
// comes with the cvs client (www.cvshome.org)
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
using ICSharpCode.SharpZipLib.Zip.Compression.Streams;

using ICSharpCode.SharpCvsLib.Client;

namespace ICSharpCode.SharpCvsLib.Requests {

    /// <summary>
    /// Response expected: no.
    /// Use zlib (RFC 1950/1951) compression to compress all further communication between
    /// the client and the server. After this request is sent, all further communication must
    /// be compressed. All further data received from the server will also be compressed.
    /// The level argument suggests to the server the level of compression that it should apply;
    /// it should be an integer between 1 and 9, inclusive, where a higher number indicates more compression.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class GzipStreamRequest : AbstractRequest
    {
        private int level;

        /// <summary>
        /// Constructor, defaults the compression level to
        ///     <code>9</code>.
        /// </summary>
        public GzipStreamRequest() : this(9)
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="level">Level of compression to apply
        ///     to the stream.
        /// </param>
        public GzipStreamRequest(int level)
        {
            this.level = level;
        }

        /// <summary>
        /// Request to zip the entire stream.
        /// </summary>
        public override string RequestString {
            get {
                return "Gzip-stream " + level + "\n";
            }
        }

        /// <summary>
        /// <code>false</code>, a response is not expected.
        /// </summary>
        public override bool IsResponseExpected {
            get {
                return false;
            }
        }

        /// <summary>
        /// <code>true</code>, the request does modify the connection.
        /// </summary>
        public override bool DoesModifyConnection {
            get {
                return true;
            }
        }

        /// <summary>
        /// Override the modify connection method to apply the compression/
        ///     decompression to the server communication.
        /// </summary>
        /// <param name="connection"></param>
        public override void ModifyConnection(IConnection connection)
        {
            connection.OutputStream.BaseStream = new DeflaterOutputStream(connection.OutputStream.BaseStream);
            connection.InputStream.BaseStream  = new InflaterInputStream(connection.InputStream.BaseStream);
        }
    }
}
