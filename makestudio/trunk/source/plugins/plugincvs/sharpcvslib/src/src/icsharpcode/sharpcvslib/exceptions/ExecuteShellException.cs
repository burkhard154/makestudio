#region "Copyright"
// CantExecuteShellException.cs
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

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Exceptions {

    /// <summary>
    /// The shell command is used to execute a binary that will handle secure 
    ///     connections.  The shell exception occurs when there is a problem
    ///     using this binary.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class ExecuteShellException : Exception {
        private string shell;

        /// <summary>
        /// The shell command.
        /// </summary>
        public string Shell {
            get {return shell;}
        }

        /// <summary>
        /// Constructor, takes the shell command to help trace the
        ///     source of the exception.
        /// </summary>
        /// <param name="shell"></param>
        public ExecuteShellException(string shell) : base(shell) {
            this.shell = shell;
        }

        /// <summary>
        /// Create a new shell execute exception object.
        /// </summary>
        /// <param name="message"></param>
        /// <param name="exception"></param>
        public ExecuteShellException (string message, Exception exception)
            : base(message, exception) {

        }
    }
}
