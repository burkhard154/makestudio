#region "Copyright"
// Copyright (C) 2003 Clayton Harbour
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
using System.Text;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Messages {

    /// <summary>
    ///     Used to handle a cvs server message.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class UpdateMessage : IMessage {
        /// <summary>
        ///     Specify that this action is an update to the cvs file.
        /// </summary>
        public const String ACTION = "U";
        private String module;
        private String repository;
        private String filename;

        private readonly ILog LOGGER =
            LogManager.GetLogger (typeof (UpdateMessage));

        /// <summary>
        ///     Constructor.
        /// </summary>
        public UpdateMessage () {

        }

        /// <summary>
        ///     The module component of the message.
        /// </summary>
        public String Module {
            get {return this.module;}
            set {
                this.module = value.Replace("/", String.Empty);}
        }

        /// <summary>
        ///     The repository or relative path component of the message.
        /// </summary>
        public String Repository {
            get {return this.repository;}
            set {this.repository = value;}
        }

        /// <summary>
        ///     The filename component of the message.
        /// </summary>
        public String Filename {
            get {return this.filename;}
            set {this.filename = value.Replace("/", String.Empty);}
        }

        /// <summary>
        ///     Format the variables entered into an update message.
        /// </summary>
        public String Message {
            get {
                string tempRepos = this.Repository;
                if (tempRepos.Length >= 0 &&
                    tempRepos.EndsWith("/")) {
                    tempRepos = tempRepos.Substring(0, tempRepos.Length - 1);
                }
                return String.Format("{0} {1}/{2}/{3}",
                    ACTION, this.Module, tempRepos, this.Filename);
            }
        }
    }
}
