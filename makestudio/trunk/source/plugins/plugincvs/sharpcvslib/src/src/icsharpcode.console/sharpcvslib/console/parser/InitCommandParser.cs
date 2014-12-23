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
//    <author>Steve Kenzell</author>
//    <author>Clayton Harbour</author>
#endregion

using System;
using System.Collections;
using System.Globalization;
using System.Text;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Console.Parser;

using log4net;

namespace ICSharpCode.SharpCvsLib.Console.Parser {

    /// <summary>
    /// Initialize the cvs repository.
    /// </summary>
    public class InitCommandParser : AbstractCommandParser {
        private CvsRoot cvsRoot;

        /// <summary>
        /// Default constructor.
        /// </summary>
        public InitCommandParser () {

        }

        /// <summary>
        /// Initialize a cvs repository.
        /// </summary>
        /// <param name="cvsroot">User information</param>
        public InitCommandParser(string cvsroot) : 
            this(new CvsRoot(cvsroot)){
        }

        /// <summary>
        /// Initialize a cvs repository
        /// </summary>
        /// <param name="cvsroot">User Information</param>
        public InitCommandParser(CvsRoot cvsroot) {
            this.cvsRoot = cvsroot;
        }

        /// <summary>
        /// Create a new instance of the <see cref="InitCommandParser"/>.
        /// </summary>
        /// <returns></returns>
        public static ICommandParser GetInstance() {
            return GetInstance(typeof(InitCommandParser));
        }

        /// <summary>
        /// Name of the command being parsed.
        /// </summary>
        public override string CommandName {
            get {return "init";}
        }

        /// <summary>
        /// Nicknames for the add command.
        /// </summary>
        public override ICollection Nicks {
            get {
                if (commandNicks.Count == 0) {
                    commandNicks.Add("init");
                }

                return commandNicks;
            }
        }

        /// <summary>
        /// Description of the command.
        /// </summary>
        public override string CommandDescription {
            get {return "Create a CVS repository if it doesn't exist";}
        }

        /// <summary>
        /// Create the command object that will be used to act on the repository.
        /// </summary>
        /// <returns>The command object that will be used to act on the
        ///     repository.</returns>
        /// <exception cref="Exception">TODO: Make a more specific exception</exception>
        /// <exception cref="NotImplementedException">If the command argument
        ///     is not implemented currently.  TODO: Implement the argument.</exception>
        public override ICommand CreateCommand () {
            ICSharpCode.SharpCvsLib.Commands.InitCommand initCommand;
            try {
                CurrentWorkingDirectory = new WorkingDirectory( this.cvsRoot,
                    null, null);
                // Create new InitCommand object
                initCommand = new ICSharpCode.SharpCvsLib.Commands.InitCommand( this.cvsRoot );
            }
            catch (Exception e) {
                LOGGER.Error (e);
                throw e;
            }
            return initCommand;
        }

        /// <summary>
        /// Output the command usage and arguements.
        /// </summary>
        public override string Usage {
            get {
                string usage = 
@"Usage: cvs init
(Specify the --help global option for a list of other help options)";

                return usage;
            }
        }
    }
}