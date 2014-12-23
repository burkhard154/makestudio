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
using System.Text;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Misc;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Extension.LogReporter {

    /// <summary>
    ///     Test the LogRevision class
    /// </summary>
    [TestFixture]
    public class LogRevisionTest {
    
        /// <summary>
        ///     Tests the default constructor.
        /// </summary>
        [Test]
        public void TestDefaultCtor () {
            LogRevision logRevision = new LogRevision();

            Assert.AreEqual("", logRevision.Revision);
            //Assert.AreEqual(DateTime.Now, this.Timestamp);
            Assert.AreEqual("", logRevision.Author);
            Assert.AreEqual("", logRevision.State);
            Assert.AreEqual("", logRevision.Comment);
            Assert.AreEqual(0, logRevision.LinesAdded);
            Assert.AreEqual(0, logRevision.LinesDeleted);
            Assert.AreEqual("", logRevision.Branches);
        }
    
        /// <summary>
        ///     Tests the properties.
        /// </summary>
        [Test]
        public void TestProperties () {
            LogRevision logRevision = new LogRevision();

            logRevision.Revision = "1.2";
            Assert.AreEqual("1.2", logRevision.Revision);
            
            logRevision.Timestamp = new DateTime(2004, 3, 7, 23, 50, 48, 123); 
            Assert.AreEqual(2004, logRevision.Timestamp.Year);
            Assert.AreEqual(3, logRevision.Timestamp.Month);
            Assert.AreEqual(7, logRevision.Timestamp.Day);
            Assert.AreEqual(23, logRevision.Timestamp.Hour);
            Assert.AreEqual(50, logRevision.Timestamp.Minute);
            Assert.AreEqual(48, logRevision.Timestamp.Second);
            Assert.AreEqual(123, logRevision.Timestamp.Millisecond);
            
            logRevision.Author = "gne";
            Assert.AreEqual("gne", logRevision.Author);
            
            logRevision.State = "Exp";
            Assert.AreEqual("Exp", logRevision.State);
            
            logRevision.Comment = "my comment";
            Assert.AreEqual("my comment", logRevision.Comment);
            
            logRevision.LinesAdded = 15;
            Assert.AreEqual(15, logRevision.LinesAdded);
            
            logRevision.LinesDeleted = 23;
            Assert.AreEqual(23, logRevision.LinesDeleted);
            
            logRevision.Branches = "1.1";
            Assert.AreEqual("1.1", logRevision.Branches);
        }
    }
}
