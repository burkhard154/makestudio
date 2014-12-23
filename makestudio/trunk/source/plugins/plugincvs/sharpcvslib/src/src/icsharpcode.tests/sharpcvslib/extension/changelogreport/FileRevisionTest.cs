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
using System.IO;
using System.Text;
using System.Xml;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Misc;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Extension.ChangeLogReport {

    /// <summary>
    ///     Test the FileRevision class
    /// </summary>
    [TestFixture]
    public class FileRevisionTest {
    
        /// <summary>
        ///     Tests the serialisation.
        /// </summary>
        [Test]
        public void Serialise () {
            string filename = "testfile.cs";
            string revision = "1.4";
            string previousRevision = "1.3";
            
            FileRevision fileRevision = new FileRevision(filename, revision, previousRevision);

            MemoryStream stream = new MemoryStream();
            XmlTextWriter writer = new XmlTextWriter(stream, new System.Text.UTF8Encoding());

            // Serialise the FileRevision to a memory stream as XML            
            writer.WriteStartDocument();
            writer.WriteStartElement("test");
            fileRevision.ExportToXml(writer);
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

            Check(reader, "testfile.cs", "1.4", "1.3");
            
            // the end of the root element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
            Assert.AreEqual("test", reader.Name);
        }
 
        /// <summary>
        /// Checks the XML output from the reader matches
        /// the expected FileRevision values
        /// </summary>
        public static void Check(XmlTextReader reader, string name, string revision, string previousrevision)
        {
            // the file element
            reader.Read();
            Assert.AreEqual(XmlNodeType.Element, reader.NodeType);
            Assert.AreEqual("file", reader.Name);
            
            // the name element
            reader.Read();
            Assert.AreEqual(XmlNodeType.Element, reader.NodeType);
            Assert.AreEqual("name", reader.Name);
            
            // the name value
            reader.Read();
            Assert.AreEqual(XmlNodeType.Text, reader.NodeType);
            Assert.AreEqual(name, reader.Value);
            
            // the end of the name element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
            Assert.AreEqual("name", reader.Name);
            
            // the revision element
            reader.Read();
            Assert.AreEqual(XmlNodeType.Element, reader.NodeType);
            Assert.AreEqual("revision", reader.Name);
            
            // the revision value
            reader.Read();
            Assert.AreEqual(XmlNodeType.Text, reader.NodeType);
            Assert.AreEqual(revision, reader.Value);
            
            // the end of the revision element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
            Assert.AreEqual("revision", reader.Name);
            
            // the previousrevision element
            reader.Read();
            Assert.AreEqual(XmlNodeType.Element, reader.NodeType);
//            Assert.AreEqual("previousrevision", reader.Name);
            Assert.AreEqual("prevrevision", reader.Name);           
            // the previousrevision value
            reader.Read();
            Assert.AreEqual(XmlNodeType.Text, reader.NodeType);
            Assert.AreEqual(previousrevision, reader.Value);
            
            // the end of the previousrevision element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
//            Assert.AreEqual("previousrevision", reader.Name);
            Assert.AreEqual("prevrevision", reader.Name);
            
            // the end of the file element
            reader.Read();
            Assert.AreEqual(XmlNodeType.EndElement, reader.NodeType);
            Assert.AreEqual("file", reader.Name);
        }
    }
}
