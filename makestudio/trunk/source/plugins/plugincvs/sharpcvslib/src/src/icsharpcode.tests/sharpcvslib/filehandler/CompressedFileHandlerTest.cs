#region "Copyright"
// Copyright (C) 2003 Gerald Evans
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
//    <author>Gerald Evans</author>
//    <author>Clayton Harbour</author>
#endregion

using System;
using System.Collections;
using System.IO;
using System.Threading;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpZipLib.GZip;
using ICSharpCode.SharpCvsLib.Streams;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.FileHandler {

    /// <summary>
    ///     Test the UncompressedFileHandler class.
    ///
    ///     The SendFile functions are tested by creating a suitable file
    ///     and getting UncompressedFileHandler to send the output to a
    ///     MemoryStream.  We then check that the contents of this
    ///     MemoryStream match the original file.
    ///
    ///     The ReceiveFile functions are tested by creating a
    ///     MemoryStream with the contents of a file, and using this
    ///     as the input to the ReceiveFile function.  We then check
    ///     the contents of the created file match what we put into
    ///     the MemoryStream.
    ///
    ///     Note: This class makes use of some static functions provided by UncompressedFileHandlerTest.
    /// </summary>
    [TestFixture]
    public class CompressedFileHandlerTest {
        private static readonly ILog LOGGER =
            LogManager.GetLogger (typeof (CompressedFileHandlerTest));

        // Temporary file we use to test the functions
        String testFileName;

        /// <summary>
        ///     Tidies up.
        /// </summary>
        [TearDown]
        public void TearDown()
        {
            // Make sure the test file has been deleted
            if (testFileName != null && testFileName.Length > 0) {
                File.Delete(testFileName);
            }
        }

        /// <summary>
        ///     Tests SendTextFile.
        /// </summary>
        [Test]
        public void SendTextFileTest()
        {
            // Create a temporary text file as the file to send
            testFileName = UncompressedFileHandlerTest.CreateTestTextFile();

            // Create a CvsStream based on a MemoryStream for SendTextFile to send the file to
            MemoryStream memoryStream = new MemoryStream();
            CvsStream cvsStream = new CvsStream(memoryStream);

            // Call the function under test
            CompressedFileHandler fileHandler = new CompressedFileHandler();
            fileHandler.SendTextFile(cvsStream, testFileName);

            // check what SendTextFile put in the stream
            cvsStream.BaseStream = new GZipInputStream(cvsStream.BaseStream);
            UncompressedFileHandlerTest.CheckTextStream(cvsStream, true);
        }

        /// <summary>
        ///     Tests ReceiveTextFile.
        /// </summary>
        [Test]
        public void ReceiveTextFileTest()
        {
            int linefeedChars = 1;    // Input is *nix style so only 1 linefeed char

            // Create a CvsStream based on a MemoryStream for ReceiveTextFile to receive the file from
            MemoryStream memoryStream = new MemoryStream();
            GZipOutputStream gzipOutputStream = new GZipOutputStream(memoryStream);
            CvsStream cvsStream = new CvsStream(gzipOutputStream);

            UncompressedFileHandlerTest.CreateTextStream(cvsStream);
            gzipOutputStream.Finish();    // This is essential to finish off the zipping
            cvsStream.BaseStream = memoryStream;
            cvsStream.Position = 0;

            // Create a temporary file to receive the file to
            testFileName = Path.GetTempFileName();

            // Call the function under test
            CompressedFileHandler fileHandler = new CompressedFileHandler();
            fileHandler.ReceiveTextFile(cvsStream, testFileName,
                                        UncompressedFileHandlerTest.GetTextLen(UncompressedFileHandlerTest.TEXT_BLOCKS, linefeedChars));

            // check the received file
            UncompressedFileHandlerTest.CheckTextFile(testFileName);
        }

        /// <summary>
        ///     Tests SendBinaryFile.
        /// </summary>
        [Test]
        public void SendBinaryFileTest()
        {
            // Create a temporary text file as the file to send
            testFileName = UncompressedFileHandlerTest.CreateTestBinaryFile();

            // Create a CvsStream based on a MemoryStream for SendTextFile to send the file to
            MemoryStream memoryStream = new MemoryStream();
            CvsStream cvsStream = new CvsStream(memoryStream);

            // Call the function under test
            CompressedFileHandler fileHandler = new CompressedFileHandler();
            fileHandler.SendBinaryFile(cvsStream, testFileName);

            // check what SendBinaryFile put in the stream
            cvsStream.BaseStream = new GZipInputStream(cvsStream.BaseStream);
            UncompressedFileHandlerTest.CheckBinaryStream(cvsStream, true);
        }

        /// <summary>
        ///     Tests ReceiveBinaryFile.
        /// </summary>
        [Test]
        public void ReceiveBinaryFileTest()
        {
            // Create a CvsStream based on a MemoryStream for ReceiveTextFile to receive the file from
            MemoryStream memoryStream = new MemoryStream();
            GZipOutputStream gzipOutputStream = new GZipOutputStream(memoryStream);
            CvsStream cvsStream = new CvsStream(gzipOutputStream);

            UncompressedFileHandlerTest.CreateBinaryStream(cvsStream);
            gzipOutputStream.Finish();    // This is essential to finish off the zipping
            cvsStream.BaseStream = memoryStream;
            cvsStream.Position = 0;

            // Create a temporary file to receive the file to
            testFileName = Path.GetTempFileName();

            // Call the function under test
            CompressedFileHandler fileHandler = new CompressedFileHandler();
            fileHandler.ReceiveBinaryFile(cvsStream, testFileName,
                                        UncompressedFileHandlerTest.GetBinaryLen(UncompressedFileHandlerTest.BINARY_BLOCKS));

            // Now validate that the file we received is as expected
            UncompressedFileHandlerTest.CheckBinaryFile(testFileName);
        }
    }
}
