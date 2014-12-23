#region "Copyright"
// Copyright (C) 2004 Gerald Evans
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
#endregion

using System;
using System.Collections.Specialized;
using System.IO;
using System.Text;
using System.Xml;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Misc;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Extension.ChangeLogReport {

    /// <summary>
    ///     Test the LogEntry class
    /// </summary>
    [TestFixture]
    public class LogEntryTest {
    
        /// <summary>
        ///     Tests the serialisation.
        /// </summary>
        [Test]
        public void Serialise () {
            DateTime timestamp = DateTime.Now;
            string author = "gne";
            string comment = "checkin comment";
            
            StringDictionary nameMap = new StringDictionary();  
            nameMap.Add("gne", "Gerald Evans");
            
            LogEntry logEntry = new LogEntry(timestamp, author, comment);
            logEntry.AddFileRevision("testfile.cs", "1.4", "1.3");

            MemoryStream stream = new MemoryStream();
            XmlTextWriter writer = new XmlTextWriter(stream, new System.Text.UTF8Encoding());

            // Serialise the FileRevision to a memory stream as XML            
            writer.WriteStartDocument();
            writer.WriteStartElement("test");
            logEntry.ExportToXml(writer, nameMap);
            writer.WriteEndElement();    // test
            writer.WriteEndDocument();
            writer.Flush();
//            writer.Close();
            
            // rewind and check what was written
            stream.Position = 0;
            
            XmlTextReader reader = new XmlTextReader(stream);
            
//            while (reader.Read())
//            {
//            System.Console.WriteLine("{0}-{1}-{2}", reader.NodeType, reader.Value, reader.Name);
//            }
            
            // the xml declaration
            reader.Read();
            Assert.AreEqual(XmlNodeType.XmlDeclaration, reader.NodeType);

            // the root element
            reader.Read();
            Assert.AreEqual(XmlNodeType.Element, reader.NodeType);

            CheckStart(reader, timestamp, "Gerald Evans");
            FileRevisionTest.Check(reader, "testfile.cs", "1.4", "1.3");
            CheckEnd(reader, "checkin comment");
            
            
            // the end of the root element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
            Assert.AreEqual("test", reader.Name);
        }
 
        /// <summary>
        /// Checks the XML output from the reader matches 
        /// the expected LogEntry values
        /// 
        /// Note: this checks up to the start of the FileRevision entries
        /// </summary>
        public static void CheckStart(XmlTextReader reader, DateTime timestamp, string author)
        {
            // the entry element
            reader.Read();
            Assert.AreEqual(XmlNodeType.Element, reader.NodeType);
            Assert.AreEqual("entry", reader.Name);
            
            // the date element
            reader.Read();
            Assert.AreEqual(XmlNodeType.Element, reader.NodeType);
            Assert.AreEqual("date", reader.Name);
            
            // the date value
            reader.Read();
            Assert.AreEqual(XmlNodeType.Text, reader.NodeType);
    	    string outputDateFormat = "yyyy'-'MM'-'dd";
            Assert.AreEqual(XmlNodeType.Text, reader.NodeType);
            Assert.AreEqual(timestamp.ToString(outputDateFormat), reader.Value);
            
            // the end of the date element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
            Assert.AreEqual("date", reader.Name);
            
            // the time element
            reader.Read();
            Assert.AreEqual(XmlNodeType.Element, reader.NodeType);
            Assert.AreEqual("time", reader.Name);
            
            // the time value
            reader.Read();
    	    string outputTimeFormat = "HH':'mm";
            Assert.AreEqual(XmlNodeType.Text, reader.NodeType);
            Assert.AreEqual(timestamp.ToString(outputTimeFormat), reader.Value);
            
            // the end of the time element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
            Assert.AreEqual("time", reader.Name);
            
            // the author element
            reader.Read();
            Assert.AreEqual(XmlNodeType.Element, reader.NodeType);
            Assert.AreEqual("author", reader.Name);
            
            // the author value
            reader.Read();
            Assert.AreEqual(XmlNodeType.CDATA, reader.NodeType);
            Assert.AreEqual("Gerald Evans", reader.Value);
            
            // the end of the author element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
            Assert.AreEqual("author", reader.Name);
        }
        
        /// <summary>
        /// Checks the XML output from the reader matches 
        /// the expected LogEntry values
        /// 
        /// Note: this checks from the end of the FileRevision entries
        /// </summary>
        public static void CheckEnd(XmlTextReader reader, string msg)
        {
            
            // the msg element
            reader.Read();
            Assert.AreEqual(XmlNodeType.Element, reader.NodeType);
            Assert.AreEqual("msg", reader.Name);
            
            // the msg value
            reader.Read();
            Assert.AreEqual(XmlNodeType.CDATA, reader.NodeType);
            Assert.AreEqual("checkin comment", reader.Value);
            
            // the end of the msg element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
            Assert.AreEqual("msg", reader.Name);
            
            // the end of the entry element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
            Assert.AreEqual("entry", reader.Name);
        }
    }
}
