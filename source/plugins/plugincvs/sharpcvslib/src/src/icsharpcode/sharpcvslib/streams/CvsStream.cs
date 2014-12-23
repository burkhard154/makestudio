#region "Copyright"
// CvsStream.cs
// Copyright (C) 2001 Mike Krueger
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
//        <author>Mike Krueger</author>
//        <author>Clayton Harbour</author>
#endregion

using System;
using System.Collections;
using System.IO;
using System.Threading;

using ICSharpCode.SharpCvsLib.Config;
using ICSharpCode.SharpCvsLib.Messages;
using ICSharpCode.SharpCvsLib.Util;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Streams {

    /// <summary>
    /// Class for handling streams to the cvs server.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class CvsStream : Stream {
        private static EncodedMessage requestMessage = new EncodedMessage ();
        private static EncodedMessage responseMessage = new EncodedMessage ();

        private SharpCvsLibConfig settings = SharpCvsLibConfig.GetInstance();

        /// <summary>
        /// Triggered when there is a message sent or written to the cvs stream.
        /// </summary>
        public EncodedMessage RequestMessage {
            get {return requestMessage;}
        }
        /// <summary>
        /// Occurs when there is a response recieved or read from the cvs stream.
        /// </summary>
        public EncodedMessage ResponseMessage {
            get {return responseMessage;}
        }

        private readonly ILog LOGGER = LogManager.GetLogger (typeof (CvsStream));
        Stream baseStream;

        /// <summary>
        /// Base stream object.
        /// </summary>
        public Stream BaseStream {
            get {return baseStream;}
            set {baseStream = value;}
        }

        /// <summary>
        /// Cvs server stream object.
        /// </summary>
        /// <param name="baseStream"></param>
        public CvsStream(Stream baseStream) {
            this.baseStream = baseStream;
        }

        /// <summary>
        /// I needed to implement the abstract member.
        /// </summary>
        public override bool CanRead {
            get {return baseStream.CanRead;}
        }

        /// <summary>
        /// I needed to implement the abstract member.
        /// </summary>
        public override bool CanSeek {
            get {return baseStream.CanSeek;}
        }

        /// <summary>
        /// I needed to implement the abstract member.
        /// </summary>
        public override bool CanWrite {
            get {return baseStream.CanWrite;}
        }

        /// <summary>
        /// I needed to implement the abstract member.
        /// </summary>
        public override long Length {
            get {return baseStream.Length;}
        }

        /// <summary>
        /// I needed to implement the abstract member.
        /// </summary>
        public override long Position {
            get {return baseStream.Position;}
            set {baseStream.Position = value;}
        }

        /// <summary>
        /// Flushes the baseInputStream
        /// </summary>
        public override void Flush() {
            baseStream.Flush();
        }

        /// <summary>
        /// I needed to implement the abstract member.
        /// </summary>
        public override long Seek(long offset, SeekOrigin origin) {
            return baseStream.Seek(offset, origin);
        }

        /// <summary>
        /// I needed to implement the abstract member.
        /// </summary>
        public override void SetLength(long val) {
            baseStream.SetLength(val);
        }

        /// <summary>
        /// Write the specified byte array to the stream.
        /// </summary>
        /// <param name="array">A byte array to send to the cvs server.</param>
        public void Write(byte[] array) {
            baseStream.Write(array, 0, array.Length);
            RequestMessage.SendMessage (EncodingUtil.GetString(array, 0, array.Length));
        }

        /// <summary>
        /// I needed to implement the abstract member.
        /// </summary>
        /// <param name="array">A byte array to send to the server.</param>
        /// <param name="offset">A position in the byte array to start writing
        ///     from.</param>
        /// <param name="count">The number of bytes to write.</param>
        public override void Write(byte[] array, int offset, int count) {
            baseStream.Write(array, offset, count);
            RequestMessage.SendMessage (EncodingUtil.GetString (array, offset, count));
        }

        /// <summary>
        /// I needed to implement the abstract member.
        /// </summary>
        public override void WriteByte(byte val) {
            baseStream.WriteByte(val);
            RequestMessage.SendMessage(EncodingUtil.GetString(val));
        }

        /// <summary>
        /// Closes the base stream
        /// </summary>
        public override void Close() {
            baseStream.Close();
        }


        /// <summary>
        /// Reads one byte of decompressed data.
        ///
        /// The byte is baseInputStream the lower 8 bits of the int.
        /// </summary>
        /// <returns>The byte.</returns>
        public override int ReadByte() {
            int val = baseStream.ReadByte();
            //ResponseMessage.SendMessage (EncodingUtil.GetString(val));
            return val;
        }

        /// <summary>
        /// Decompresses data into the byte array
        /// </summary>
        /// <param name ="array">
        /// the array to read and decompress data into
        /// </param>
        /// <param name ="offset">
        /// the offset indicating where the data should be placed
        /// </param>
        /// <param name ="length">
        /// the number of bytes to decompress
        /// </param>
        public override int Read(byte[] array, int offset, int length) {
            int val = baseStream.Read(array, offset, length);
            ResponseMessage.SendMessage (EncodingUtil.GetString (array, offset, length));
            return val;
        }

        /// <summary>
        /// Read the stream from the cvs server.
        /// </summary>
        /// <param name="array">A byte array that is being read from the server.</param>
        /// <returns>The integer value of the byte array.</returns>
        public int Read(byte[] array) {
            int val = baseStream.Read(array, 0, array.Length);
            ResponseMessage.SendMessage (EncodingUtil.GetString(array, 0, array.Length));
            return val;
        }

        /// <summary>
        /// Read from the stream until a line termination
        ///     character is reached.
        /// </summary>
        /// <returns></returns>
        private string ReadLineBlock() {
            ArrayList buffer = new ArrayList(1024);
            while (true) {
                int i = ReadByte();
                if (i == '\n' || i == -1) {
                    break;
                }
                buffer.Add((byte)i);
            }
            return EncodingUtil.DEFAULT_ENCODING.GetString((byte[])buffer.ToArray(typeof(byte)));
        }

        /// <summary>
        /// Read from the stream until the end of the line.
        ///
        /// TODO: If an empty line is found, then this will
        /// skip this line and try the next line (up to 10
        /// times).  Is this the required behaviour?
        /// </summary>
        /// <returns></returns>
        public string ReadLine() {
            string line = "";
            int x = 0;
            while (line.Length == 0 && ++x < 10) {
                line = ReadLineBlock();
                if (line.Length == 0) {
                    Thread.Sleep(10);
                }
            }
            return line;
        }

        /// <summary>
        /// Read from the stream until the end of the line.
        /// </summary>
        /// <returns></returns>
        public string ReadToEndOfLine() {
            string line = ReadLineBlock();
            ResponseMessage.SendMessage(line);
            return line;
        }

        /// <summary>
        /// Read from the stream until the first whitespace
        ///     character is reached.
        /// </summary>
        /// <returns></returns>
        public string ReadToFirstWS() {
            ArrayList buffer = new ArrayList(1024);
            while (true) {
                int i = ReadByte();

                buffer.Add((byte)i);
                if (i == '\n' || i ==' ' || i == -1) {
                    break;
                }
            }
            
            return EncodingUtil.DEFAULT_ENCODING.GetString((byte[])buffer.ToArray(typeof(byte)));
        }

        /// <summary>
        /// Read a block of data from the stream.
        ///
        /// // TODO: Figure out what this would be used for.
        /// </summary>
        /// <param name="buffer"></param>
        /// <param name="size"></param>
        public void ReadBlock(byte[] buffer, int size) {
            for (int i = 0; i < size;) {
                int back = Read(buffer, i, size - i);
                i += back;
                if (i < size) {
                    Thread.Sleep(10);
                }
            }
        }

        /// <summary>
        /// Send the specified string message to the cvs server.
        /// </summary>
        /// <param name="dataStr"></param>
        public void SendString(string dataStr) {
            byte[] buff = EncodingUtil.DEFAULT_ENCODING.GetBytes(dataStr);
            baseStream.Write(buff, 0, buff.Length);
            RequestMessage.SendMessage (dataStr);
            Flush();
        }
    }
}
