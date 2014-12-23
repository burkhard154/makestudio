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
    ///     Note: many of the helper functions are made static and public
    ///     so they can be used by CompressedFileHandlerTest.
    ///     (I did originally try subclassing, but Nunit didn't like this.)
    /// </summary>
    [TestFixture]
    public class UncompressedFileHandlerTest {
        private static readonly ILog LOGGER =
            LogManager.GetLogger (typeof (UncompressedFileHandlerTest));

        /// <summary>
        /// Count of text blocks to write to text file
        /// Each block is 27 lines long.  See GenerateTextByte() for more info.
        /// </summary>
        public const int TEXT_BLOCKS = 50;

        /// <summary>
        /// Count of binary blocks to write to binary file
        /// Each block is 256 bytes long.  See GenerateBinaryByte() for more info.
        /// </summary>
        public const int BINARY_BLOCKS = 512;

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
            testFileName = CreateTestTextFile();

            // Create a CvsStream based on a MemoryStream for SendTextFile to send the file to
            MemoryStream memoryStream = new MemoryStream();
            CvsStream cvsStream = new CvsStream(memoryStream);

            // Call the function under test
            UncompressedFileHandler fileHandler = new UncompressedFileHandler();
            fileHandler.SendTextFile(cvsStream, testFileName);

            // check what SendTextFile put in the stream
            CheckTextStream(cvsStream, false);
        }

        /// <summary>
        ///     Tests ReceiveTextFile.
        /// </summary>
        [Test]
        public void ReceiveTextFileTest()
        {
            int linefeedChars = 1;    // we emulate input from cvs so only 1 linefeed char

            // Create a CvsStream based on a MemoryStream for ReceiveTextFile to receive the file from
            MemoryStream memoryStream = new MemoryStream();
            CvsStream cvsStream = new CvsStream(memoryStream);

            // put a text file into the stream
            CreateTextStream(cvsStream);
            cvsStream.Position = 0;

            // Create a temporary file to receive the file to
            testFileName = Path.GetTempFileName();

            // Call the function under test
            UncompressedFileHandler fileHandler = new UncompressedFileHandler();
            fileHandler.ReceiveTextFile(cvsStream, testFileName, GetTextLen(TEXT_BLOCKS, linefeedChars));

            // check the received file
            CheckTextFile(testFileName);
        }

        /// <summary>
        ///     Creates a temporary text file to use to test the SendTextFile functions
        ///     Returns the name of the file.
        ///     It is the callers responsibility to make sure the file is deleted
        ///     when finished with.
        /// </summary>
        static public string CreateTestTextFile()
        {
            FileStream file;
            String fileName;

            // The linefeed chars in this file needs to reflect the O/S we are
            // runing on.
            int linefeedChars = System.Environment.NewLine.Length;

            // Create a temporary file and fill with text data
            fileName = Path.GetTempFileName();
            file = File.Create(fileName);
            for (int n = 0; n < GetTextLen(TEXT_BLOCKS, linefeedChars) ; n++) {
                file.WriteByte(GenerateTextByte(n, linefeedChars));
            }
            file.Close();
 
            return fileName;
        }

        /// <summary>
        ///     Checks that the given text file contains the content
        ///     we expect to find in it.
        /// </summary>
        static public void CheckTextFile(string fileName)
        {
            // The linefeed chars in this file needs to reflect the O/S we are
            // runing on.
            int linefeedChars = System.Environment.NewLine.Length;
            FileStream file;
            String msg;

            // Validate that the file we received is as expected
            file = File.OpenRead(fileName);
            Assert.IsTrue(file.Length == GetTextLen(TEXT_BLOCKS, linefeedChars));
            for (int n = 0; n < GetTextLen(TEXT_BLOCKS, linefeedChars) ; n++) {
                byte actual = (byte)file.ReadByte();
                byte wanted = GenerateTextByte(n, linefeedChars);
                msg = String.Format("n:{0} actual:{1} wanted:{2}", n, actual, wanted);
                Assert.AreEqual(wanted, actual, msg);
            }
            file.Close();
        }

        /// <summary>
        ///     Fills the stream with data representing a text file.
        /// </summary>
        static public void CreateTextStream(CvsStream cvsStream)
        {
            int linefeedChars = 1;    // we are emulating a file coming from cvs

            // Put a text file onto the stream
            for (int n = 0; n < GetTextLen(TEXT_BLOCKS, linefeedChars); n++) {
                cvsStream.WriteByte(GenerateTextByte(n, linefeedChars));
            }
        }

        /// <summary>
        ///     Checks that the given stream contains the content
        ///     we expect to find in it.
        /// </summary>
        static public void CheckTextStream(CvsStream cvsStream, bool isCompressed)
        {
            int linefeedChars = 1;    // this stream is intended for cvs
            int len;
            String numStr;
            String msg;

            // Rewind the memory stream so we can check what was written to it
            cvsStream.Position = 0;

            if (isCompressed) {
                // First char should be a 'z'
                Assert.AreEqual(cvsStream.ReadByte(), 'z');
            }

            // Read the first line which should be the line length
            numStr = cvsStream.ReadLine();
            len = Int32.Parse(numStr);
            msg = String.Format("Expected length of {0} but got length {1}",
                                GetTextLen(TEXT_BLOCKS, linefeedChars), len);
            Assert.AreEqual(GetTextLen(TEXT_BLOCKS, linefeedChars), len);

            // Check what was written to the memory stream matches the file we generated
            for (int n = 0; n < GetTextLen(TEXT_BLOCKS, linefeedChars); n++) {
                byte actual = (byte)cvsStream.ReadByte();
                byte wanted = GenerateTextByte(n, linefeedChars);
                msg = String.Format("n:{0} actual:{1} wanted:{2}", n, actual, wanted);
                Assert.AreEqual(wanted, actual, msg);
            }
        }

        /// <summary>
        ///     Gererates the byte to be saved at the specified position in the text file.
        ///     This will generate a file of the form:
        ///     \n
        ///     A\n
        ///     BB\n
        ///     CCC\n
        ///     ...
        ///     ZZZZZZZZZZZZZZZZZZZZZZZZZZ\n
        ///     \n
        ///     A\n
        ///     ...
        ///     linefeedChars controls whether lines are terminated with "\n" or "\r\n"
        /// </summary>
        static public byte GenerateTextByte(int position, int linefeedChars)
        {
            byte ret;
            int line;
            int pos;

            // There are 378 ( +27 on windows) chars in the repeating block
            // so we only interested in the position within this;
            pos = position % GetTextLen(1, linefeedChars);
            for (line = 0; line <= 26; line++) {
                if (pos < line + linefeedChars) {
                    break;
                }
                pos -= line + linefeedChars;
            }
            // line: zero based (0 -> "\n", 1 -> "A\n", ... 26 -> "ZZZ...ZZZ\n")
            // pos: zero based index in that line
            if (pos >= line) {
                if (pos == line + linefeedChars - 1) {
                    ret = (byte)'\n';
                } else {
                    ret = (byte)'\r';
                }
            } else {
                ret = (byte)('A' + line - 1);
            }

            return ret;
        }

        /// <summary>
        ///     Returns the length of the file.
        /// </summary>
        static public int GetTextLen(int blocks, int linefeedChars)
        {
            return blocks * (351 + 27 * linefeedChars);
        }

        /// <summary>
        ///     Tests SendBinaryFile.
        /// </summary>
        [Test]
        public void SendBinaryFileTest()
        {
            // Create a temporary text file as the file to send
            testFileName = CreateTestBinaryFile();

            // Create a CvsStream based on a MemoryStream for SendTextFile to send the file to
            MemoryStream memoryStream = new MemoryStream();
            CvsStream cvsStream = new CvsStream(memoryStream);

            // Call the function under test
            UncompressedFileHandler fileHandler = new UncompressedFileHandler();
            fileHandler.SendBinaryFile(cvsStream, testFileName);

            // check what SendBinaryFile put in the stream
            CheckBinaryStream(cvsStream, false);
        }

        /// <summary>
        ///     Tests ReceiveBinaryFile.
        /// </summary>
        [Test]
        public void ReceiveBinaryFileTest()
        {
            // Create a CvsStream based on a MemoryStream for ReceiveTextFile to receive the file from
            MemoryStream memoryStream = new MemoryStream();
            CvsStream cvsStream = new CvsStream(memoryStream);
            CreateBinaryStream(cvsStream);
            cvsStream.Position = 0;

            // Create a temporary file to receive the file to
            testFileName = Path.GetTempFileName();

            // Call the function under test
            UncompressedFileHandler fileHandler = new UncompressedFileHandler();
            fileHandler.ReceiveBinaryFile(cvsStream, testFileName, GetBinaryLen(BINARY_BLOCKS));

            // Now validate that the file we received is as expected
            CheckBinaryFile(testFileName);
        }

        /// <summary>
        ///     Creates a temporary binary file to use to test the SendBinaryFile functions
        ///     Returns the name of the file.
        ///     It is the callers responsibility to make sure the file is deleted
        ///     when finished with.
        /// </summary>
        static public string CreateTestBinaryFile()
        {
            FileStream file;
            String fileName;

            // Create a temporary file and fill with binary data
            fileName = Path.GetTempFileName();
            file = File.Create(fileName);
            for (int n = 0; n < GetBinaryLen(BINARY_BLOCKS) ; n++) {
                file.WriteByte(GenerateBinaryByte(n));
            }
            file.Close();

            return fileName;
        }

        /// <summary>
        ///     Checks that the given binary file contains the content
        ///     we expect to find in it.
        /// </summary>
        static public void CheckBinaryFile(string fileName)
        {
            FileStream file;
            String msg;

            // Now validate that the file we received is as expected
            file = File.OpenRead(fileName);
            Assert.IsTrue (file.Length == GetBinaryLen(BINARY_BLOCKS));
            for (int n = 0; n < GetBinaryLen(BINARY_BLOCKS) ; n++) {
                byte actual = (byte)file.ReadByte();
                byte wanted = GenerateBinaryByte(n);
                msg = String.Format("n:{0} actual:0x{1:X2} wanted:0x{2:X2}", n, actual, wanted);
                Assert.AreEqual(wanted, actual, msg);
            }
            file.Close();
        }

        /// <summary>
        ///     Fills the stream with data representing a binary file.
        /// </summary>
        static public void CreateBinaryStream(CvsStream cvsStream)
        {
            // Put a binary file onto the stream
            for (int n = 0; n < GetBinaryLen(BINARY_BLOCKS); n++) {
                cvsStream.WriteByte(GenerateBinaryByte(n));
            }
        }

        /// <summary>
        ///     Checks that the given stream contains the content
        ///     we expect to find in it.
        /// </summary>
        static public void CheckBinaryStream(CvsStream cvsStream, bool isCompressed)
        {
            int len;
            String numStr;
            String msg;

            // Rewind the memory stream so we can check what was written to it
            cvsStream.Position = 0;

            if (isCompressed) {
                // First char should be a 'z'
                Assert.AreEqual(cvsStream.ReadByte(), 'z');
            }

            // Read the first line which should be the line length
            numStr = cvsStream.ReadLine();
            len = Int32.Parse(numStr);
            msg = String.Format("Expected length of {0} but got length {1}",
                                GetBinaryLen(BINARY_BLOCKS), len);
            Assert.AreEqual(GetBinaryLen(BINARY_BLOCKS), len);

            // Check what was written to the memory stream matches the file we generated
            for (int n = 0; n < GetBinaryLen(BINARY_BLOCKS); n++) {
                byte actual = (byte)cvsStream.ReadByte();
                byte wanted = GenerateBinaryByte(n);
                msg = String.Format("n:{0} actual:0x{1:X2} wanted:0x{2:X2}", n, actual, wanted);
                Assert.AreEqual(wanted, actual, msg);
            }
        }

        /// <summary>
        ///     Gererates the byte to be saved at the specified position in the binary file.
        ///     For this we just loop from 0x00 to 0xFF
        /// </summary>
        static public byte GenerateBinaryByte(int position)
        {
            return (byte)(position % 256);
        }

        /// <summary>
        ///     Returns the length of the file
        /// </summary>
        static public int GetBinaryLen(int blocks)
        {
            return blocks * 0x100;
        }
    }
}
