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
//    <credit>Credit to Dick Grune, Vrije Universiteit, Amsterdam, for writing
//    the shell-script CVS system that this is based on.  In addition credit
//    to Brian Berliner and Jeff Polk for their work on the cvsnt port of
//    this work. </credit>
//
//    <author>Clayton Harbour</author>
#endregion


using System;
using System.Collections;
using System.Reflection;

using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.FileSystem;

using log4net;

namespace ICSharpCode.SharpCvsLib.Console.Parser {
	/// <summary>
	/// Summary description for CommandFactory.
	/// </summary>
    public class CommandParserFactory {

        private bool showUsage;
        /// <summary>
        /// <code>true</code> if the help/usage information should be displayed for this command.
        /// </summary>
        public bool ShowUsage {
            get {return this.showUsage;}
            set {this.showUsage = value;}
        }

        private string command;
        private string[] args;
        private CvsRoot cvsRoot;
        private WorkingDirectory workingDirectory;
        private static SortedList allCommands;
        private static SortedList availableCommands;

        private static readonly ILog LOGGER = LogManager.GetLogger(typeof(CommandParserFactory));

        /// <summary>
        /// Creates a new instance of the command parser.
        /// </summary>
        /// <param name="command"></param>
        /// <param name="args"></param>
        /// <param name="cvsRoot"></param>
        /// <param name="workingDirectory"></param>
        public CommandParserFactory(string command, string[] args,
            CvsRoot cvsRoot, WorkingDirectory workingDirectory){
            this.command = command;
            this.args = GetArgsAfterCommandName(args);
            this.cvsRoot = cvsRoot;
            this.workingDirectory = workingDirectory;
        }

        private static SortedList commandParsers;
        private static SortedList CommandParsers {
            get {
                if (null == commandParsers) {
                    commandParsers = new SortedList();
                }
                if (commandParsers.Count == 0) {
                    Assembly cvsLibAssembly = Assembly.GetAssembly(typeof(Usage));

                    if (null == cvsLibAssembly) {
                        throw new Exception("Unable to load #cvslib assembly.");
                    }

                    Type[] types = cvsLibAssembly.GetTypes();
                    foreach(Type type in types) {
                        if (type.IsClass && type.GetInterface("ICommandParser") != null && !type.IsAbstract) {
                            try {
                                ICommandParser commandParser = (ICommandParser)Activator.CreateInstance(type);
                                commandParsers.Add(commandParser.CommandName, commandParser);
                            } catch (System.MissingMethodException) {
                                LOGGER.Warn(String.Format("Command {0} does not provide a parameterless constructor, skipping.",
                                    type.FullName));
                            }
                        }
                    }
                }
                return commandParsers;
            }
        }

        /// <summary>
        /// Gets a list of available commands.  Available commands are cvs commands that are currently
        /// implemented in #cvslib and have an associated command parser in the commandline client.
        /// </summary>
        public static SortedList AvailableCommands {
            get {
                if (null == availableCommands) {
                    availableCommands = new SortedList();
                    foreach (ICommandParser commandParser in CommandParsers.Values) {
                        Command command = new Command(commandParser.CommandName, commandParser.CommandDescription, commandParser.Nicks);
                        command.Implemented = commandParser.Implemented;
                        availableCommands.Add(command.CommandName, command);                        
                    }
                }
                return availableCommands;

            }
        }

        private static Command GetCom (string commandName, string commandDescription, string[] args) {
            ArrayList nicks = new ArrayList();
            foreach (string arg in args) {
                nicks.Add(arg);
            }
            return new Command(commandName, commandDescription, nicks);
        }

        private static Command GetCom (string commandName, string commandDescription, ArrayList list) {
            return new Command(commandName, commandDescription, list);
        }

        /// <summary>
        /// All commands that are available for the most common cvs clients.
        /// </summary>
        public static SortedList AllCommands {
            get {
                if (null == allCommands) {
                    allCommands = new SortedList();

                    allCommands.Add("add", GetCom("add", "Add a new file/directory to the repository", new string[]{"ad", "new"}));
                    allCommands.Add("admin", GetCom("admin", "Administration front end for rcs", new string[]{"adm", "rcs"}));
                    allCommands.Add("annotate", GetCom("annotate", "Show last revision where each line was modified", new string[]{"ann"}));
                    allCommands.Add("chac1", GetCom("chac1", "Change the Access Control List for a directory", new string[] {"setacl", "setperm"}));
                    allCommands.Add("checkout", GetCom("checkout", "Checkout sources for editing", new string[]{"co", "get"}));
                    allCommands.Add("chown", GetCom("chown", "Change the owner of a directory", new ArrayList()));
                    allCommands.Add("commit", GetCom("commit", "Check files into the repository", new string[]{"ci", "com"}));
                    allCommands.Add("diff", GetCom("diff", "Show differences between revisions", new string[]{"di", "dif"}));
                    allCommands.Add("edit", GetCom("edit", "Get ready to edit a watched file", new ArrayList()));
                    allCommands.Add("editors", GetCom("editors", "See who is editing a watched file", new ArrayList()));
                    allCommands.Add("export", GetCom("export", "Export sources from CVS, similar to checkout", new string[]{"exp", "ex"}));
                    allCommands.Add("history", GetCom("history", "Show repository access history", new string[]{"hi", "his"}));
                    allCommands.Add("import", GetCom("import", "Import sources into CVS, using vendor branches", new string[]{"im", "imp"}));
                    allCommands.Add("init", GetCom("init", "Create a CVS repository if it doesn't exist", new ArrayList()));
                    allCommands.Add("info", GetCom("info", "Display information about supported protocols", new string[]{"inf"}));
                    allCommands.Add("log", GetCom("log", "Print out history information for files", new string[]{"lo"}));
                    //#ifdef CLIENT_SUPPORT
                    allCommands.Add("login", GetCom("login", "Prompt for password for authenticating server", new string[]{"logon", "lgn"}));
                    allCommands.Add("logout", GetCom("logout", "Removes entry in .cvspass for remote repository", new ArrayList()));
                    //#endif /* CLIENT_SUPPORT */
                    allCommands.Add("ls", GetCom("ls", "List files in the repository", new string[]{"dir", "list"}));
                    allCommands.Add("lsacl", GetCom("lsacl", "List the directories Access Control List", new string[]{"lsattr", "listperm"}));
                    allCommands.Add("passwd", GetCom("passwd", "Set the user's password (Admin: Administer users)", new string[]{"password", "setpass"}));
                    //#if defined(SERVER_SUPPORT)
                    allCommands.Add("authserver", GetCom("authserver", "Authentication server mode", new ArrayList()));
                    //#endif
                    allCommands.Add("rannotate", GetCom("rannotate", "Show last revision where each line of module was modified", new string[]{"rann", "ra"}));
                    allCommands.Add("rdiff", GetCom("rdiff", "Create 'patch' format diffs between releases", new string[]{"patch", "pa"}));
                    allCommands.Add("release", GetCom("release", "Indicate that a Module is no longer in use", new string[]{"re", "rel"}));
                    allCommands.Add("remove", GetCom("remove", "Remove an entry from the repository", new string[]{"rm", "delete"}));
                    allCommands.Add("cvs_rename", GetCom("cvs_rename", "Rename a file in the repository", new string[]{"ren", "move"}));
                    allCommands.Add("rlog", GetCom("rlog", "Print out history information for a module", new string[]{"rl"}));
                    allCommands.Add("rtag", GetCom("rtag", "Add a symbolic tag to a module", new string[]{"rt", "rfreeze"}));
                    //#if defined(SERVER_SUPPORT)
                    allCommands.Add("server", GetCom("server", "Server mode", new ArrayList()));
                    //#endif
                    allCommands.Add("status", GetCom("status", "Display status information on checked out files", new string[]{"st", "cvs_stat"}));
                    allCommands.Add("tag", GetCom("tag", "Add a symbolic tag to checked out version of files", new string[]{"ta", "freeze"}));
                    allCommands.Add("unedit", GetCom("unedit", "Undo an edit command", new ArrayList()));
                    allCommands.Add("update", GetCom ("update", "Bring work tree in sync with repository", new string[]{"up", "upd"}));
                    allCommands.Add("version", GetCom("version", "Show current CVS version(s)", new string[]{"ve", "ver"}));
                    allCommands.Add("watch", GetCom("watch", "Set watches", new ArrayList()));
                    allCommands.Add("watchers", GetCom("watchers", "See who is watching a file", new ArrayList()));
                    allCommands.Add("xml", GetCom("xml", "Create an xml report containing the history information for a module", new ArrayList()));

                    foreach (Command command in AvailableCommands.Values) {
                        allCommands[command.CommandName] = command;
                    }
                }  
                return allCommands;
            }
        }

        private string[] GetArgsAfterCommandName (string[] args) {
            ArrayList subArgs = new ArrayList();
            bool add = false;
            foreach (string argument in args) {
                if (add) {
                    subArgs.Add(argument);
                }
                if (argument.Equals("xml")) {
                    add = true;
                }
            }

            return (string[])subArgs.ToArray(typeof(String));
        }

        /// <summary>
        /// Create a new instance of the command parser that matches the command name specified
        /// in the constructor.
        /// </summary>
        /// <returns>A new instance of the specified command parser that implements the 
        /// <see cref="ICommandParser"/> interface.</returns>
        public ICommandParser GetCommandParser () {
            ICommandParser parser = (ICommandParser)CommandParsers[command];

            if (null == parser) {
                foreach (ICommandParser tp in AvailableCommands.Values) {
                    foreach (string nick in tp.Nicks) { 
                        if (nick.Equals(command)) {
                            parser = tp;
                        }
                    }
                    if (null != parser) {
                        break;
                    }
                }
            }

            if (null == parser) {
                System.Console.WriteLine(String.Format("Command {0} not implemented.", command));
                System.Environment.Exit(-1);
            }

            parser.Args = this.args;
            parser.CvsRoot = this.cvsRoot;
            parser.CurrentWorkingDirectory = this.workingDirectory;

            try {
                parser.ParseOptions();
            } catch (CommandLineParseException e) {
                string msg = 
                    String.Format("{0}{1}{2}",
                    e.Message, Environment.NewLine, parser.Usage);
                System.Console.WriteLine(msg);
            }
            
            return parser;
        }
	}
}
