#region "Copyright"
//
// Copyright (C) 2004   Clayton Harbour
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
    /// Interface for all command line parser commands.
    /// </summary>
    public interface ICommandParser {
        /// <summary>
        /// The current working directory.
        /// </summary>
        WorkingDirectory CurrentWorkingDirectory {get;set;}

        /// <summary>
        /// The cvs root to use for the command.
        /// </summary>
        CvsRoot CvsRoot {get;set;}

        /// <summary>
        /// The common name of the command this object represents.
        /// </summary>
        string CommandName {get;}

        /// <summary>
        /// Brief description of what the command does, corresponds to the text displayed on the
        /// <code>cvs --help-commands</code> menu.
        /// </summary>
        string CommandDescription {get;}

        /// <summary>
        /// Alternate names that the command is referenced by.
        /// </summary>
        ICollection Nicks {get;}

        /// <summary>
        /// Indicates if the command is implemented currently in the command line client and/ or the
        /// sharpcvs library.  <code>true</code> if it is implemented, otherwise <code>false</code>.
        /// 
        /// NOTE: This will eventually go away.
        /// </summary>
        bool Implemented {get;}

        /// <summary>
        /// Create the command object that will be used to act on the repository.
        /// </summary>
        /// <returns>The command object that will be used to act on the
        ///     repository.</returns>
        /// <exception cref="Exception">TODO: Make a more specific exception</exception>
        /// <exception cref="NotImplementedException">If the command argument
        ///     is not implemented currently.  TODO: Implement the argument.</exception>
        ICommand CreateCommand ();

        /// <summary>
        /// The commandline arguments that the command parser is to parse.
        /// </summary>
        string[] Args {get;set;}
 
        /// <summary>
        /// Parse the command line options/ arguments and populate the command
        ///     object with the arguments.
        /// </summary>
        void ParseOptions ();

        /// <summary>
        /// Show the command usage message.
        /// </summary>
        string Usage {get;}

    }
}