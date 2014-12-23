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
using System.Configuration;
using System.Text;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Tests.Config;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Extension.LogReporter {

    /// <summary>
    ///     Test the LogReportCommand class
    /// </summary>
    [TestFixture]
    public class LogReportCommandTest {
        private ILog LOGGER =
            LogManager.GetLogger (typeof(LogReportCommandTest));

//        private SharpCvsLibTestsConfig settings = 
//            SharpCvsLibTestsConfig.GetInstance();

    
        /// <summary>
        ///     Tests against the Sharpcvslib-test-repository.
        /// </summary>
        [Test]
        public void Test () {
            
            SharpCvsLibTestsConfig settings;
            string section = SharpCvsLibTestsConfigHandler.APP_CONFIG_SECTION;
//System.Console.WriteLine("section={0}", section);
            settings = (SharpCvsLibTestsConfig)ConfigurationSettings.GetConfig(section);

//System.Console.WriteLine("target-directory={0}", settings.TargetDirectory);
//System.Console.WriteLine("password={0}", settings.ValidPassword);
            string moduleName = settings.Module;
            string workingDir = settings.TargetDirectory;
            string password = settings.ValidPassword;
            bool foundTestFile1 = false;
            bool foundTestFile2 = false;
            LogRevision logRevision;
            LogSymbolicName symbolicName;
            
            LogReportCommand logCommand = new LogReportCommand(moduleName, workingDir);
    // 
    //    logCommand.SetLastNDays(7);
    //    // or logCommand.StartDate = new DateTime(...);
    //    // and/or logCommand.EndDate = new DateTime(...);
    //    
            LogReport logReport = logCommand.Run(password);
            
            Assert.AreEqual(16, logReport.Count);
            foreach (LogFile logFile in logReport)
            {
                if (logFile.WorkingFnm.EndsWith("test-file.txt"))
                {
                    Assert.IsTrue(!foundTestFile1);
                    foundTestFile1 = true;

                    Assert.AreEqual("/cvsroot/sharpcvslib-test/sharpcvslib-test-repository/test-file.txt,v", logFile.RepositoryFnm);
                    Assert.AreEqual("test-file.txt", logFile.WorkingFnm);
                    Assert.AreEqual("", logFile.Description);
                    
                    // check the revisions
                    Assert.AreEqual(3, logFile.Count);
                    // most recent version will be first
                    logRevision = logFile[0];
                    Assert.AreEqual("1.3", logRevision.Revision);
                    CheckDate(2003, 9, 14, 1, 8, 21, logRevision.Timestamp);
                    Assert.AreEqual("claytonharbour", logRevision.Author);
                    Assert.AreEqual("Exp", logRevision.State);
                    Assert.AreEqual(3, logRevision.LinesAdded);
                    Assert.AreEqual(1, logRevision.LinesDeleted);
                    Assert.AreEqual("*** empty log message ***", logRevision.Comment);
                    
                    logRevision = logFile[1];
                    Assert.AreEqual("1.2", logRevision.Revision);
                    CheckDate(2003, 9, 14, 1, 7, 15, logRevision.Timestamp);
                    Assert.AreEqual("claytonharbour", logRevision.Author);
                    Assert.AreEqual("Exp", logRevision.State);
                    Assert.AreEqual(3, logRevision.LinesAdded);
                    Assert.AreEqual(1, logRevision.LinesDeleted);
                    Assert.AreEqual("Added line.", logRevision.Comment);
                   
                    logRevision = logFile[2];
                    Assert.AreEqual("1.1", logRevision.Revision);
                    CheckDate(2003, 9, 14, 1, 5, 51, logRevision.Timestamp);
                    Assert.AreEqual("claytonharbour", logRevision.Author);
                    Assert.AreEqual("Exp", logRevision.State);
                    Assert.AreEqual(0, logRevision.LinesAdded);
                    Assert.AreEqual(0, logRevision.LinesDeleted);
                    Assert.AreEqual("Various changes for sticky tag support.  Looked at implementing a message event handling system for request/ responses to output server messages (similar to tortoise).", logRevision.Comment);
                    
                    // check the symbolic names
                    // check the revisions
                    Assert.AreEqual(3, logFile.SymbolicNames.Count);
                    symbolicName = logFile.SymbolicNames[0];
                    Assert.AreEqual("V0_3", symbolicName.Name);
                    Assert.AreEqual("1.3", symbolicName.Revision);
                    
                    symbolicName = logFile.SymbolicNames[1];
                    Assert.AreEqual("V0_2", symbolicName.Name);
                    Assert.AreEqual("1.2", symbolicName.Revision);
                    
                    symbolicName = logFile.SymbolicNames[2];
                    Assert.AreEqual("V0_1", symbolicName.Name);
                    Assert.AreEqual("1.1", symbolicName.Revision);
               }
                if (logFile.WorkingFnm.EndsWith("test-file-2.txt"))
                {
                    Assert.IsTrue(!foundTestFile2);
                    foundTestFile2 = true;

                    Assert.AreEqual("/cvsroot/sharpcvslib-test/sharpcvslib-test-repository/src/test-file-2.txt,v", logFile.RepositoryFnm);
                    Assert.AreEqual("src/test-file-2.txt", logFile.WorkingFnm);
                    Assert.AreEqual("", logFile.Description);
                    
                    Assert.AreEqual(1, logFile.Count);
                    // most recent version will be first
                    logRevision = logFile[0];
                    Assert.AreEqual("1.1", logRevision.Revision);
                    CheckDate(2003, 9, 14, 15, 57, 48, logRevision.Timestamp);
                    Assert.AreEqual("claytonharbour", logRevision.Author);
                    Assert.AreEqual("Exp", logRevision.State);
                    Assert.AreEqual(0, logRevision.LinesAdded);
                    Assert.AreEqual(0, logRevision.LinesDeleted);
                    Assert.AreEqual("*** empty log message ***", logRevision.Comment);
                }
            }
            
            Assert.IsTrue(foundTestFile1);
            Assert.IsTrue(foundTestFile2);
    //            ...
    //	        foreach (LogRevision logRevision in logFile)
    //	        {
    //    	        ...
    //	        }
    //	   }
        }
        
        private void CheckDate(int year, int month, int day, int hour, int minute, int second, DateTime timestamp)
        {
            Assert.AreEqual(year, timestamp.Year);
            Assert.AreEqual(month, timestamp.Month);
            Assert.AreEqual(day, timestamp.Day);
            Assert.AreEqual(hour, timestamp.Hour);
            Assert.AreEqual(minute, timestamp.Minute);
            Assert.AreEqual(second, timestamp.Second);
        }
    }
}
