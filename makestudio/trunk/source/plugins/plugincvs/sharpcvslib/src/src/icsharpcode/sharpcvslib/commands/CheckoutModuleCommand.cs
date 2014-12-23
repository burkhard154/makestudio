#region "Copyright"
// CheckoutModuleCommand.cs
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

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Requests;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.FileSystem;

using log4net;

namespace ICSharpCode.SharpCvsLib.Commands {

    /// <summary>
    /// Checkout module command
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class CheckoutModuleCommand : ICommand {
        private WorkingDirectory workingDirectory;

        private ILog LOGGER = LogManager.GetLogger (typeof (CheckoutModuleCommand));
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="workingDirectory"></param>
        public CheckoutModuleCommand(WorkingDirectory workingDirectory) {
            this.workingDirectory    = workingDirectory;

            this.Revision = this.workingDirectory.Revision;
            this.OverrideDirectory = this.workingDirectory.OverrideDirectory;
            if (this.workingDirectory.HasDate) {
                this.Date = this.workingDirectory.Date;
            }
            this.Module = this.workingDirectory.ModuleName;
        }

        private string _revision;
        public string Revision {
            get {return this._revision;}
            set {this._revision = value;}
        }

        private string _overrideDirectory;
        public string OverrideDirectory {
            get {return this._overrideDirectory;}
            set {this._overrideDirectory = value;}
        }

        private bool _hasDate = false;
        private DateTime _date;
        public DateTime Date {
            get {return this._date;}
            set {
                this._hasDate = true;
                this._date = value;
            }
        }

        protected string DateAsString {
            get {
                string dateAsString = "";
                string dateFormat = "dd MMM yyyy";

                if (this._hasDate) {
                    dateAsString = this._date.ToString(dateFormat);
                }
                return dateAsString;
            }
        }

        private string _module;
        public string Module {
            get {return this._module;}
            set {this._module = value;}
        }

        /// <summary>
        /// Execute checkout module command.
        /// </summary>
        /// <param name="connection">Server connection</param>
        public void Execute(ICommandConnection connection) {
            workingDirectory.Clear();

            //connection.SubmitRequest(new CaseRequest());
            connection.SubmitRequest(new ArgumentRequest(this.Module));

            connection.SubmitRequest(new DirectoryRequest(".",
                                    workingDirectory.CvsRoot.CvsRepository +
                                    "/" + workingDirectory.ModuleName));

            connection.SubmitRequest(new ExpandModulesRequest());

            connection.SubmitRequest(
                new ArgumentRequest(ArgumentRequest.Options.MODULE_NAME));

            if (null != this.Revision) {
                connection.SubmitRequest (new ArgumentRequest (ArgumentRequest.Options.REVISION));
                connection.SubmitRequest(new ArgumentRequest(this.Revision));
            }
            if (this._hasDate) {
                connection.SubmitRequest (new ArgumentRequest (ArgumentRequest.Options.DATE));
                connection.SubmitRequest(new ArgumentRequest(this.DateAsString));
            }
            if (null != this.OverrideDirectory) {
                connection.SubmitRequest (
                    new ArgumentRequest (ArgumentRequest.Options.OVERRIDE_DIRECTORY));
                connection.SubmitRequest (
                    new ArgumentRequest (this.OverrideDirectory));
            }

            connection.SubmitRequest(new ArgumentRequest(this.Module));

            connection.SubmitRequest(new DirectoryRequest(".",
                                    workingDirectory.CvsRoot.CvsRepository +
                                    "/" + this.Module));

            connection.SubmitRequest(new CheckoutRequest());
            Manager manager = new Manager (connection.Repository.WorkingPath);
        }
    }
}
