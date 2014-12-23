#region "Copyright"
//
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
//    <author>Clayton Harbour</author>
#endregion
using System;
using System.Globalization;
using System.Collections;
using System.IO;
using System.Text;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Console.Parser;
using ICSharpCode.SharpCvsLib.FileSystem;

using log4net;

namespace ICSharpCode.SharpCvsLib.Console.Parser {

    /// <summary>
    /// Produce an xml log report.
    /// </summary>
    public class XmlLogCommandParser : AbstractCommandParser, ICommandParser {
        private string moduleName;
        private string ModuleName {
            get {return this.moduleName;}
            set {
                //                System.Console.WriteLine(String.Format("Module name: {0}.", value));
                this.moduleName = value;}
        }

        private string localDirectory;

        private const string OPT_DATE = "-D";
        private const string OPT_DAYS = "-Ds";
        private const string OPT_OUTPUT_XML_FILENAME = "-oxml";
        private const string OPT_OUTPUT_HTML_FILENAME = "-ohtml";
        private const string OPT_OUTPUT_XSL_FILENAME = "-xsl";
        // future enhancement to specify xsl arguments...not today though :-).
        //private const string OPT_OUTPUT_XSL_ARGS = "-xslargs";
        private const string OPT_NAME_MAP = "-nm";
        private const string OPT_PW = "-pw";

        private DateTime startDate;
        private DateTime StartDate {
            get {return this.startDate;}
            set {this.startDate = value;}
        }

        private DateTime endDate;
        private DateTime EndDate {
            get {return this.endDate;}
            set {this.endDate = value;}
        }

        private int lastNDays;
        private int LastNDays {
            get {return this.lastNDays;}
            set {this.lastNDays = value;}
        }

        private string xmlFilename;
        private string XmlFilename {
            get {return this.xmlFilename;}
            set {
                //                System.Console.WriteLine(String.Format("Setting XmlFilename: {0}.", 
                //                    value));
                this.xmlFilename = value;}
        }

        private string htmlFilename;
        private string HtmlFilename {
            get {return this.htmlFilename;}
            set {this.htmlFilename = value;}
        }

        private string xslFilename;
        private string XslFilename {
            get {return this.xslFilename;}
            set {
                //                System.Console.WriteLine(String.Format("Setting XslFilename: {0}.",
                //                    value));
                this.xslFilename = value;
            }
        }

        private string password;
        private string Password {
            get {return this.password;}
            set {this.password = value;}
        }

        /// <summary>
        /// Create a new instance of the xml log command parser.
        /// </summary>
        public XmlLogCommandParser () {

        }

        /// <summary>
        /// Produce an xml log report.
        /// </summary>
        public XmlLogCommandParser(CvsRoot cvsroot, string[] args) {
            //            System.Console.WriteLine(String.Format("Number of arguments: {0}.", args.Length));
            this.CvsRoot = cvsroot;
            this.ParseOptions();
        }

        /// <summary>
        /// Create a new instance of the <see cref="XmlLogCommandParser"/>.
        /// </summary>
        /// <returns></returns>
        public static ICommandParser GetInstance() {
            return GetInstance(typeof(XmlLogCommandParser));
        }

        /// <summary>
        /// Name of the command being parsed.
        /// </summary>
        public override string CommandName {
            get {return "xml";}
        }

        /// <summary>
        /// Description of the command.
        /// </summary>
        public override string CommandDescription {
            get {return "Produces an xml formatted report on the history of the files";}
        }

        /// <summary>
        /// Nicknames for the add command.
        /// </summary>
        public override ICollection Nicks {
            get {
                commandNicks.Clear();
                return commandNicks;
            }
        }

        /// <summary>
        /// The add command is implemented in the library and commandline parser.
        /// </summary>
        public override bool Implemented {
            get {return true;}
        }

        /// <summary>
        /// Create a new <see cref="ICSharpCode.SharpCvsLib.Commands.XmlLogCommand"/>.
        /// </summary>
        /// <returns></returns>
        public override ICommand CreateCommand () {
            ICSharpCode.SharpCvsLib.Commands.XmlLogCommand xmlLogCommand;

            try {
                // create CvsRoot object parameter
                if (localDirectory == null || localDirectory.Length == 0) {
                    localDirectory = Environment.CurrentDirectory;
                }
                this.CurrentWorkingDirectory = new WorkingDirectory(this.CvsRoot,
                    localDirectory, this.moduleName);
                xmlLogCommand = 
                    new ICSharpCode.SharpCvsLib.Commands.XmlLogCommand(this.CurrentWorkingDirectory, 
                    this.CurrentWorkingDirectory.ModuleName);
                xmlLogCommand.StartDate = this.StartDate;
                xmlLogCommand.EndDate = this.EndDate;
                xmlLogCommand.XmlFilename = this.XmlFilename;
                xmlLogCommand.XslFilename = this.XslFilename;
                xmlLogCommand.HtmlFilename = this.HtmlFilename;
            }
            catch (Exception e) {
                LOGGER.Error (e);
                throw e;
            }
            return xmlLogCommand;        
        }
 
        /// <summary>
        /// Parse the command line options.
        /// </summary>
        /// <exception cref="CommandLineParseException">If the command line options cannot be
        /// parsed or are invalid.</exception>
        public override void ParseOptions () {
            base.ParseOptions();

            if (Args.Length > 1) {
                this.moduleName = Args[Args.Length - 1];
            }
            int i = 0;
            while (i < Args.Length - 1) {
                string arg = Args[i];
                switch (arg) {
                    case OPT_DATE: 
                        i++;
                        arg = Args[i];
                        DateTime date = Util.DateParser.ParseCvsDate(arg);

                        if (DateTime.MinValue == this.StartDate) {
                            this.StartDate = date;
                        } else if (this.StartDate > date) {
                            this.EndDate = this.StartDate;
                            this.StartDate = date;
                        }
                        else {
                            this.EndDate = date;
                        }
                        break;
                    case OPT_DAYS:
                        i++;
                        arg = Args[i];
                        int numDays = Convert.ToInt32(arg);
                        this.LastNDays = numDays;
                        break;
                    case OPT_OUTPUT_XML_FILENAME:
                        i++;
                        arg = Args[i];
                        this.XmlFilename = arg;
                        if (null == this.HtmlFilename) {
                            this.HtmlFilename = Path.GetFileNameWithoutExtension(this.XmlFilename) + ".html";
                        }
                        break;
                    case OPT_OUTPUT_HTML_FILENAME:
                        i++;
                        arg = Args[i];
                        this.HtmlFilename = arg;
                        break;
                    case OPT_OUTPUT_XSL_FILENAME:
                        i++;
                        arg = Args[i];
                        this.XslFilename = arg;
                        break;
                    case OPT_PW:
                        i++;
                        arg = Args[i];
                        this.Password = arg;
                        break;
                    default:
                        throw new CommandLineParseException(String.Format("Unknown option: {0}.",
                            arg));
                }
                i++;
            }
        }

        /// <summary>
        /// Output the command usage and arguements.
        /// </summary>
        public override string Usage {
            get {
                string usage = 
@"Usage: cvs xml [-D [startDate]] [-D [endDate]] [-oxml [source filename]] [-xsl [style sheet]]
        -D      Used to specify the start and end dates.  The first -D pair is the start date (required).
        -oxml   Name of the file xml change information will be output to (required).
        -xsl    Name of the xsl transformation to apply to the xml log information.
        -ohtml  Name of the html file the transform will create, if left empty the file will be
                    have the same name as the xml 'source' file, with a html extension.
(Specify the --help global option for a list of other help options)";

                return usage;
            }
        }
    }
}