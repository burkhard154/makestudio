#region "Copyright"
// XmlLogCommand.cs
// Copyright (C) 2004 Clayton Harbour
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
#endregion

using System;
using System.Collections;
using System.IO;
using System.Text;
using System.Xml.Xsl;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Requests;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Extension.ChangeLogReport;

using log4net;

namespace ICSharpCode.SharpCvsLib.Commands {

    /// <summary>
    /// The XmlLogCommand produces an xml representation of the changes that have occurred in the 
    /// cvs repository.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class XmlLogCommand : ICommand {
        private ILog LOGGER =
            LogManager.GetLogger (typeof (XmlLogCommand));
        
        private string xmlFilename = 
            Path.Combine(Path.GetTempPath(), "sharpcvslib-xmllogcommand.xml");
        private string xslFilename;

        private CvsChangeLog cvsChangeLog;

        /// <summary>
        /// <see cref="CvsChangeLog.StartDate"/>.
        /// </summary>
        public DateTime StartDate {
            get {return cvsChangeLog.StartDate;}
            set {
//                System.Console.WriteLine(String.Format("Setting start date: {0}.", value));
                this.cvsChangeLog.StartDate = value;}
        }

        /// <summary>
        /// <see cref="CvsChangeLog.EndDate"/>.
        /// </summary>
        public DateTime EndDate {
            get {return cvsChangeLog.EndDate;}
            set {
//                System.Console.WriteLine(String.Format("Setting end date: {0}.", value));
                this.cvsChangeLog.EndDate = value;}
        }

        /// <summary>
        /// <see cref="CvsChangeLog.SetLastNDays"/>.
        /// </summary>
        public int LastNDays {
            set {this.cvsChangeLog.SetLastNDays(value);}
        }

        /// <summary>
        /// <see cref="CvsChangeLog.AddNameMapping"/>
        /// </summary>
        /// <param name="id"></param>
        /// <param name="fullName"></param>
        public void AddNameMapping(string id, string fullName) {
            this.cvsChangeLog.AddNameMapping(id, fullName);
        }
        
        /// <summary>
        /// The name of the xml file.
        /// </summary>
        public string XmlFilename {
            get {return this.xmlFilename;}
            set {this.xmlFilename = value;}
        }

        /// <summary>
        /// The style sheet to apply to the xml file.  
        /// </summary>
        public string XslFilename {
            get {return this.xslFilename;}
            set {this.xslFilename = value;} 
        }

        private string htmlFilename;
        /// <summary>
        /// The name of the html file created after the xsl transform.
        /// </summary>
        public string HtmlFilename {
            get {
                if (null == this.htmlFilename) {
                    this.htmlFilename = Path.GetFileNameWithoutExtension(this.XmlFilename) + ".html";
                }
                return this.htmlFilename;}
            set {this.htmlFilename = value;}
        }
             

        /// <summary>
        /// Creates a new instance of the xml log command.
        /// </summary>
        /// <param name="workingDirectory">Directory on the local file system that represents a 
        /// working "sandbox" of the remote cvs repository.</param>
        /// <param name="module">Module to report on.</param>
        public XmlLogCommand(WorkingDirectory workingDirectory, string module) {
            this.cvsChangeLog = new CvsChangeLog(workingDirectory, module);
        }

        private ICommandConnection connection;

        /// <summary>
        /// Produce the xml log report.
        /// </summary>
        /// <param name="connection"></param>
        public void Execute(ICommandConnection connection) {
            this.connection = connection;
            if (null == this.XmlFilename || DateTime.MinValue == this.StartDate ||
                DateTime.MinValue == this.EndDate) {
                throw new ArgumentException (
                    String.Format("Xml filename ( {0} ), start date ( {1} ) and end date ( {2} ) must be provided.",
                    this.XmlFilename, this.StartDate, this.EndDate));
            }

            if (!Path.IsPathRooted(this.XmlFilename)) {
                this.XmlFilename = Path.Combine(System.Environment.CurrentDirectory, this.XmlFilename);
            }

            DirectoryInfo dirInfo = 
                new DirectoryInfo(Path.GetDirectoryName(this.XmlFilename));
            if (!dirInfo.Exists) {
                dirInfo.Create();
            }
            this.cvsChangeLog.Run(this.XmlFilename, connection);

            if (null != this.XslFilename) {
                XslTransform t = new XslTransform();
                try {
                    t.Load(this.XslFilename);
                    t.Transform(this.XmlFilename, this.HtmlFilename);
                } catch (Exception e) {
                    LOGGER.Error(e);
                }
            }
        }
    }
}