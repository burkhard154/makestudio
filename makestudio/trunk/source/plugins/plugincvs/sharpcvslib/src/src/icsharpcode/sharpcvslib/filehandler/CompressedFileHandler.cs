#region "Copyright"
// CompressedFileHandler.cs
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
#endregion

using System;
using System.IO;
using System.Text;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Config;
using ICSharpCode.SharpZipLib.GZip;
using ICSharpCode.SharpCvsLib.Streams;

namespace ICSharpCode.SharpCvsLib.FileHandler {
    /// <summary>
    /// Implements a compressed file handler
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class CompressedFileHandler   /* WARNING : untested class :)*/
        : UncompressedFileHandler  {

        /// <summary>
        /// Upload a text file to the cvs server.
        /// </summary>
        /// <param name="outStream"></param>
        /// <param name="fileName"></param>
        public override void SendTextFile(CvsStream outStream, string fileName) {
            base.SendTextFile(outStream, fileName); // because it depends ond SendBinaryFile this is OK
        }

        /// <summary>
        /// Receive a text file from the cvs server.
        /// </summary>
        /// <param name="inputStream"></param>
        /// <param name="fileName"></param>
        /// <param name="length"></param>
        public override void ReceiveTextFile(CvsStream inputStream, string fileName, int length) {
            Stream oldStream  = inputStream.BaseStream;
            inputStream.BaseStream = new GZipInputStream(inputStream.BaseStream);
            base.ReceiveTextFile(inputStream, fileName, length);
            inputStream.BaseStream = oldStream;
        }

        /// <summary>
        /// Send a binary file to the cvs server.
        /// </summary>
        /// <param name="outStream"></param>
        /// <param name="fileName"></param>
        public override void SendBinaryFile(CvsStream outStream, string fileName) {
            FileStream fs = File.OpenRead(fileName);
            byte[] data = new byte[fs.Length];
            fs.Read(data, 0, data.Length);
            fs.Close();

            Stream oldStream  = outStream.BaseStream;
            outStream.BaseStream = new GZipOutputStream(outStream.BaseStream);

            outStream.SendString("z" + data.Length.ToString() + "\n");
            outStream.Write(data);
            outStream.BaseStream = oldStream;
        }

        /// <summary>
        /// Receive a binary file from the cvs server.
        /// </summary>
        /// <param name="inputStream"></param>
        /// <param name="fileName"></param>
        /// <param name="length"></param>
        public override void ReceiveBinaryFile(CvsStream inputStream, 
            string fileName, int length) {
            Stream oldStream  = inputStream.BaseStream;
            inputStream.BaseStream = new GZipInputStream(inputStream.BaseStream);
            base.ReceiveBinaryFile(inputStream, fileName, length);
            inputStream.BaseStream = oldStream;
        }
    }
}
