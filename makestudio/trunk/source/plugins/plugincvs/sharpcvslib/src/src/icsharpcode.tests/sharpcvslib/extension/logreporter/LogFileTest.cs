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
using ICSharpCode.SharpCvsLib.Tests.Config;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Extension.LogReporter {

    /// <summary>
    ///     Test the LogFile class
    /// </summary>
    [TestFixture]
    public class LogFileTest {
        private SharpCvsLibTestsConfig settings = 
            SharpCvsLibTestsConfig.GetInstance();

    
        /// <summary>
        ///     Tests the default constructor.
        /// </summary>
        [Test]
        public void TestDefaultCtor () {
            LogFile logFile = new LogFile(settings.GetCvsRoot());

            Assert.AreEqual("", logFile.RepositoryFnm);
            Assert.AreEqual("", logFile.WorkingFnm);
            Assert.AreEqual("", logFile.Description);
            
            Assert.AreEqual(0, logFile.Count);
        }
    
        /// <summary>
        ///     Tests the properties.
        /// </summary>
        [Test]
        public void TestProperties () {
            LogFile logFile = new LogFile(settings.GetCvsRoot());

            string repositoryFnm = "/cvsroot/sharpcvslib/sharpcvslib/src/ICSharpCode/SharpCvsLib/Client/CVSServerConnection.cs,v";
            logFile.RepositoryFnm = repositoryFnm;
            Assert.AreEqual(repositoryFnm, logFile.RepositoryFnm);
            
            string workingFnm = "src/ICSharpCode/SharpCvsLib/Client/CVSServerConnection.cs";
            logFile.WorkingFnm = workingFnm;
            Assert.AreEqual(workingFnm, logFile.WorkingFnm);
            
            logFile.Description = "Test description";
            Assert.AreEqual("Test description", logFile.Description);
        }
    
        /// <summary>
        ///     Tests accessing [0] when no revisions.
        /// </summary>
        [Test]
        [ExpectedException(typeof(ArgumentOutOfRangeException))]
        public void TestInvalidIndexZero () {
            LogFile logFile = new LogFile(settings.GetCvsRoot());
            
            LogRevision logRevision = logFile[0];
        }
    
        /// <summary>
        ///     Tests adding revisions.
        /// </summary>
        [Test]
        public void TestRevisions () {
            LogFile logFile = new LogFile(settings.GetCvsRoot());
            
            LogRevision logRevision1 = new LogRevision();
            logRevision1.Revision = "1.1";
            logFile.AddRevision(logRevision1);
            
            LogRevision logRevision2 = new LogRevision();
            logRevision2.Revision = "1.2";
            logFile.AddRevision(logRevision2);
            
            LogRevision logRevision3 = new LogRevision();
            logRevision3.Revision = "1.3";
            logFile.AddRevision(logRevision3);

            Assert.AreEqual(3, logFile.Count);
            
            // Test indexer
            Assert.AreEqual("1.1", logFile[0].Revision);
            Assert.AreEqual("1.2", logFile[1].Revision);
            Assert.AreEqual("1.3", logFile[2].Revision);
            
            // Test foreach
            int nIndex = 0;
            foreach (LogRevision logRevision in logFile) {
                Assert.IsTrue(nIndex <= 2);
                if (nIndex == 0) {
                    Assert.AreEqual("1.1", logRevision.Revision);
                } else if (nIndex == 1) {
                    Assert.AreEqual("1.2", logRevision.Revision);
                } else if (nIndex == 2) {
                    Assert.AreEqual("1.3", logRevision.Revision);
                }
                
                nIndex++;
            }
        }
    }
}
