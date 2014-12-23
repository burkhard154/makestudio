#region "Copyright"
//
// Copyright (C) 2003 Steve Kenzell
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
//    <credit>Credit to Dick Grune, Vrije Universiteit, Amsterdam, for writing
//    the shell-script CVS system that this is based on.  In addition credit
//    to Brian Berliner and Jeff Polk for their work on the cvsnt port of
//    this work. </credit>
//    <author>Steve Kenzell</author>
//    <author>Clayton Harbour</author>
#endregion

using System;
using System.Collections;

namespace ICSharpCode.SharpCvsLib.Console.Parser {

    /// <summary>Encapsulates the properties for a command.</summary>
    public class Command {

        private String first;
        private ArrayList nicks;
        private string description;
        private bool implemented;

        /// <summary>
        /// Primary name for the command, the name to be used first.
        /// </summary>
        public String First {
            get {return this.first;}
        }

        /// <summary>
        /// Nickname/ synonym for the command.
        /// </summary>
        public String Nick1 {
            get {
                if (null != this.nicks && this.nicks.Count > 0) {
                    return (string)this.nicks[0];
                }
                return String.Empty;
            }
        }

        /// <summary>
        /// Nickname/ synonym for the command
        /// </summary>
        public String Nick2 {
            get {
                if (null != this.nicks && this.nicks.Count > 1) {
                    return (string)this.nicks[1];
                }
                return String.Empty;            
            }
        }

        /// <summary>
        /// Common name for the command.
        /// </summary>
        public string CommandName {
            get {return this.first;}
        }

        /// <summary>
        /// Description of the command.
        /// </summary>
        public string Description {
            get {return this.description;}
            set {this.description = value;}
        }

        /// <summary>
        /// <code>true</code> if the command is implemented, <code>false</code> otherwise.
        /// </summary>
        public bool Implemented {
            get {return this.implemented;}
            set {this.implemented = value;}
        }

        /// <summary>Create a new command.</summary>
        /// <param name="first">Primary command name.</param>
        /// <param name="nick1">First alternate name for the command.</param>
        /// <param name="nick2">Second alternate name for the command.</param>
        public Command (string first, string nick1, string nick2) { 
            this.first = first;
            this.nicks = new ArrayList();
            this.nicks.Add(nick1);
            this.nicks.Add(nick2);
        }

        /// <summary>
        /// Create a new instance of the command object.
        /// </summary>
        /// <param name="commandName">Name of the command.</param>
        /// <param name="description">Description of the command.</param>
        /// <param name="nicks">Nicknames for the command.</param>
        public Command (string commandName, string description, ICollection nicks) {
            this.first = commandName;
            this.description = description;
            this.nicks = new ArrayList(nicks);
        }
    }
}
