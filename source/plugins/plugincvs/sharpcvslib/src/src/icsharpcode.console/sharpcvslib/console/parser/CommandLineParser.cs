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
using System.IO;
using System.Text;
using System.Text.RegularExpressions;

using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.FileSystem;

using ICSharpCode.SharpCvsLib.Console.Commands;

using log4net;

namespace ICSharpCode.SharpCvsLib.Console.Parser {

    /// <summary>
    ///     Parse the command line parameters and create a new Console
    ///     command object for the current parameters passed in.
    /// </summary>
    public class CommandLineParser {
        private readonly ILog LOGGER = LogManager.GetLogger(typeof (CommandLineParser));

        private String[] arguments;

        private CvsRoot cvsRoot;
        private string commandTxt;
        private string options;
        private string repository;
        private string singleOptions;
        private string files;

        private bool verbose = false;

        private const string REGEX_LOG_LEVEL = @"[-]*log:[\s]*(debug|info|warn|error)";
        private const string REGEX_VERBOSE = @"[-]*(verbose)";

        private const String ENV_CVS_ROOT = "CVS_ROOT";

        private WorkingDirectory currentWorkingDirectory;
        /// <summary>
        /// Get the current working directory, parsed from the command line.
        /// </summary>
        public WorkingDirectory CurrentWorkingDirectory {
            get {return this.currentWorkingDirectory;}
        }

            /// <summary>
            /// Value of the cvsroot to use as a string.  This will be passed
            ///     into the CvsRoot object which will know how to parse it.
            /// </summary>
            public CvsRoot CvsRoot {
            get {return this.cvsRoot;}
        }

        /// <summary>
        /// The text value of the command that will be executed.  This should be
        ///     translated into one of the public API command objects.
        /// </summary>
        public String Command {
            get {return this.commandTxt;}
        }
        /// <summary>
        /// Value of the repository to use as a string.  This will be passed
        ///     into the RemoveCommand object which will know which files to get.
        /// </summary>
        public String Files {
            get {return this.files;}
        }

        /// <summary>
        /// Option to pass into the command.
        ///
        /// TODO: There may need to be an options collection to handle options,
        ///     either that or handle them as an attribute of the individual commands...
        /// </summary>
        public String Options {
            get {return this.options;}
        }

        /// <summary>
        /// Value of the repository to use as a string.  This will be passed
        ///     into the CheckoutCommand object which will know which files to get.
        /// </summary>
        public String Repository {
            get {return this.repository;}
        }

        /// <summary>
        /// Return the commandline string collection as a single string.
        /// </summary>
        public string CommandLine {
            get {
                StringBuilder msg = new StringBuilder ();
                foreach (string arg in this.arguments) {
                    msg.Append(String.Format("{0} ", arg));
                }
                return msg.ToString();;

            }
        }

        /// <summary>
        /// Set the global logging level if the appropriate commandline switch is passed in.
        /// </summary>
        /// <param name="commandline"></param>
        public void SetLogLevel (string commandline) {
            Regex regex = new Regex(REGEX_LOG_LEVEL);
            Match match = regex.Match(commandline);
            if (match.Groups.Count > 0) {
                string newLevelString = match.Groups[1].Value;

                if (null != newLevelString && newLevelString.Length != 0) {
                    log4net.Core.LevelMap map = log4net.LogManager.GetRepository().LevelMap;
                    log4net.Core.Level newLevel = map[newLevelString];
                    log4net.LogManager.GetRepository().Threshold = newLevel;
                }
            }
            // hack to remove the logging level parameter so the rest of the parsing goes correctly
            this.RemoveArg("log:");
        }

        /// <summary>
        /// Hack to remove arguments that cannot be processed by the current command line parser
        /// implementation.
        /// </summary>
        /// <param name="argRemove"></param>
        private void RemoveArg (string argRemove) {
            ArrayList newArguments = new ArrayList();
            foreach (string arg in this.arguments) {
                if (arg.IndexOf(argRemove) < 0) {
                    newArguments.Add(arg);
                }
            }
            this.arguments = (string[])newArguments.ToArray(typeof(string));
        }

        private string password;
        /// <summary>
        /// The password passed in on the commandline, or null if none.
        /// </summary>
        public string Password {
            get {return this.password;}
        }

        private void SetPassword(string commandLine) {
            string thePassword;
            if (null != this.arguments && this.arguments.Length > 0) {
                Regex regex = new Regex(LoginCommand.REGEX_PASSWORD);
                Match match = regex.Match(commandLine);
                string pwd = match.Groups[1].Value;
                thePassword = pwd;
            } else {
                thePassword = String.Empty;
            }
            this.RemoveArg("pwd:");
            this.password = thePassword;
        }

        private void SetVerbose(string commandLine) {
            Regex regex = new Regex(REGEX_VERBOSE);
            Match match = regex.Match(commandLine);
            if (match.Groups.Count > 0 && match.Groups[1].Value == "verbose") {
                this.verbose = true;
            }

            this.RemoveArg("verbose");
        }

        /// <summary>
        /// <code>true</code> if the server response and requests should be sent to the appropriate
        /// logger (usually standard out); otherwise <code>false</code>.
        /// </summary>
        public bool Verbose {
            get {return this.verbose;}
        }

        /// <summary>Create a new instance of the command line parser and
        ///     initialize the arguments object.</summary>
        /// <param name="args">A collection of strings that represent the command
        ///     line arguments sent into the program.</param>
        public CommandLineParser (String[] args) {
            this.arguments = args;

            // TODO: Remove this hack when add method to set options.
            this.options = String.Empty;

            this.SetLogLevel(this.CommandLine);
            this.SetPassword(this.CommandLine);
            this.SetVerbose(this.CommandLine);

        }

        /// <summary>
        /// Parse the command line options.  There are two (2) general sweeps
        ///     at parsing the command line.  The first sweep looks for command line
        ///     help switches, denoted by -- parameters.     
        /// </summary>
        /// <returns>A command object from the library which will be used to 
        ///     access the repsository.</returns>
        /// <exception cref="CommandLineParseException">If there is a problem
        ///     parsing the command line arguments (i.e. if invalid arguments
        ///     are entered.</exception>
        public ICommand Execute () {
            if (LOGGER.IsDebugEnabled) {
                StringBuilder msg = new StringBuilder ();
                msg.Append("\n Command line arguments:");
                foreach (String argument in this.arguments) {
                    msg.Append("\n\t argument=[").Append(argument).Append("]");
                }
                LOGGER.Debug(msg);
            }

            bool isHelp = this.ParseHelp (this.arguments);

            if (isHelp) {
                return null;
            }

            int startIndex = 0;
            // TODO: Remove = null when all other code paths return a value,
            //      this was just put in so it would compile.
            ICommand command = null;
            if (arguments.Length < 1) {
                System.Console.WriteLine (Usage.General);
            }

            if (arguments.Length > 0 && 
                (arguments[0] == "-d")) {
                this.cvsRoot = new CvsRoot(this.arguments[1]);
                startIndex = 2;                
            } else if (arguments.Length > 0 && 
                (arguments[0].Length > 2) && 
                arguments[0].Substring(0, 2) == "-d") {
                this.cvsRoot = new CvsRoot(this.arguments[0].Substring(2).Trim());
                startIndex = 1;
            }

            for (int i = startIndex; i < arguments.Length; i++) {
                if (LOGGER.IsDebugEnabled) {
                    StringBuilder msg = new StringBuilder ();
                    msg.Append("arguments[").Append(i).Append("]=[").Append(arguments[i]).Append("]");
                    LOGGER.Debug(msg);
                }
                LOGGER.Debug("Before we grab the arguments.");
                string commandString = arguments[i].Trim();
                CommandParserFactory factory;
                ICommandParser parser;
                switch (commandString) {
                    case "add":
                    case "ad":
                    case "new":
                        // no single options for the Add command
                        this.commandTxt = arguments[i];
                        i++;
                        // get rest of arguments which is options on the commit command.
                        while (arguments.Length > i && arguments[i].IndexOf("-", 0, 1) >= 0) {
                            // Get options with second parameters?
                            if (arguments[i].IndexOfAny( singleOptions.ToCharArray(), 1, 1) >= 0) {
                                for ( int cnt=1; cnt < arguments[i].Length; cnt++ ) {
                                    this.options = this.options + "-" + arguments[i][cnt] + " "; // No
                                }
                            }
                            else {
                                this.options = this.options + arguments[i++];       // Yes
                                this.options = this.options + arguments[i] + " ";
                            }
                            i++;
                        }
                        if (arguments.Length > i) {
                            // Safely grab the module, if not specified then
                            //  pass null into the repository...the cvs command
                            //  line for cvsnt/ cvs seems to bomb out when
                            //  it sends to the server
                            this.repository = arguments[i];
                        } 
                        else {
                            this.repository = String.Empty;
                        }
                        AddCommandParser addCommand = 
                            new AddCommandParser(this.CvsRoot, repository, options);
                        command = addCommand.CreateCommand ();
                        this.currentWorkingDirectory = 
                            addCommand.CurrentWorkingDirectory;
                        break;
                    case "commit":
                    case "ci":
                    case "com":
                        singleOptions = "DRcfln";
                        this.commandTxt = arguments[i];
                        i++;
                        // get rest of arguments which is options on the commit command.
                        while (arguments.Length > i && arguments[i].IndexOf("-", 0, 1) >= 0) {
                            LOGGER.Debug("Parsing arguments.  Argument[" + i + "]=[" + arguments[i]);
                            // Get options with second parameters?
                            if (arguments[i].IndexOfAny( singleOptions.ToCharArray(), 1, 1) >= 0) {
                                for ( int cnt=1; cnt < arguments[i].Length; cnt++ ) {
                                    this.options = this.options + "-" + arguments[i][cnt] + " "; // No
                                }
                            }
                            else {
                                this.options = this.options + arguments[i++];       // Yes
                                this.options = this.options + arguments[i] + " ";
                            }
                            i++;
                        }
                        if (arguments.Length > i) {
                            // Safely grab the module, if not specified then
                            //  pass null into the repository...the cvs command
                            //  line for cvsnt/ cvs seems to bomb out when
                            //  it sends to the server
                            this.repository = arguments[i];
                        } 
                        else {
                            this.repository = String.Empty;
                        }
                        CommitCommandParser commitCommand = 
                            new CommitCommandParser(this.CvsRoot, repository, options);
                        command = commitCommand.CreateCommand ();
                        this.currentWorkingDirectory = 
                            commitCommand.CurrentWorkingDirectory;
                        break;
                    case "checkout":
                    case "co":
                    case "get":
                        singleOptions = "ANPRcflnps";
                        this.commandTxt = arguments[i];
                        i++;
                        // get rest of arguments which is options on the checkout command.
                        while (arguments.Length > i && arguments[i].Trim().IndexOf("-") == 0){
                            // Get options with second parameters?
                            if (arguments[i].Trim().IndexOfAny( singleOptions.ToCharArray(), 1, 1) >= 0){
                                for ( int cnt=1; cnt < arguments[i].Length; cnt++ ){
                                    this.options = this.options + "-" + arguments[i][cnt] + " "; // No
                                }
                            }
                            else{
                                this.options = this.options + arguments[i++];       // Yes
                                this.options = this.options + arguments[i] + " ";
                            }
                            i++;
                        }
                        if (arguments.Length > i){
                            // Safely grab the module, if not specified then
                            //  pass null into the repository...the cvs command
                            //  line for cvsnt/ cvs seems to bomb out when
                            //  it sends to the server
                            this.repository = arguments[i];
                        } else {
                            this.repository = String.Empty;
                        }
                        CheckoutCommandParser checkoutCommand = 
                            new CheckoutCommandParser(this.CvsRoot, this.Repository, options);
                        command = checkoutCommand.CreateCommand ();
                        this.currentWorkingDirectory = 
                            checkoutCommand.CurrentWorkingDirectory;
                        break;
                    case "import":
                    case "imp":
                    case "im":
                        i++;
                        string [] tempArgs = new string[arguments.Length - i];
                        Array.Copy(arguments, i, tempArgs, 0, arguments.Length - i);
                        ImportCommandParser importCommand = 
                            new ImportCommandParser(this.CvsRoot, tempArgs);
                        command = importCommand.CreateCommand();
                        this.currentWorkingDirectory =
                            importCommand.CurrentWorkingDirectory;
                        i = arguments.Length;
                        break;
                    case "init":
                        this.commandTxt = arguments[i];
                        InitCommandParser initCommand = new InitCommandParser(this.CvsRoot);
                        command = initCommand.CreateCommand ();
                        this.currentWorkingDirectory = initCommand.CurrentWorkingDirectory;
                        break;
                    case "log":
                    case "lo":
                        this.commandTxt = arguments[i++];
                        string[] logArgs = new string[arguments.Length - i];
                        Array.Copy(arguments, i, logArgs, 0, arguments.Length - i);
                        LogCommandParser logCommandParser = 
                            new LogCommandParser(this.CvsRoot, logArgs);
                        command = logCommandParser.CreateCommand();
                        this.currentWorkingDirectory = logCommandParser.CurrentWorkingDirectory;
                        i = arguments.Length;
                        break;
                    case "login":
                    case "logon":
                    case "lgn":
                        // login to server
                        this.commandTxt = arguments[i];
                        LoginCommand loginCommand = 
                            new LoginCommand(this.CvsRoot, this.currentWorkingDirectory);
                        loginCommand.Args = arguments;
                        this.currentWorkingDirectory = loginCommand.CurrentWorkingDirectory;
                        command = loginCommand;
                        break;
                    case "dir":
                    case "list":
                    case "ls":
                        factory = 
                            new CommandParserFactory("ls", arguments, 
                            this.cvsRoot, this.currentWorkingDirectory);

                        parser = factory.GetCommandParser();
                        i = arguments.Length;
                        command = parser.CreateCommand();
                        this.currentWorkingDirectory = 
                            parser.CurrentWorkingDirectory;
                        break;
                    case "passwd":
                    case "password":
                    case "setpass":
                        this.commandTxt = arguments[i];
                        break;
                    case "remove":
                    case "delete":
                    case "rm":
                        singleOptions = "Rfl";
                        this.commandTxt = arguments[i];
                        i++;
                        // get rest of arguments which is options on the update command.
                        while (arguments.Length > i && arguments[i].IndexOf("-", 0, 1) >= 0) {
                            // Get options with second parameters?
                            if (arguments[i].IndexOfAny( singleOptions.ToCharArray(), 1, 1) >= 0) {
                                for ( int cnt=1; cnt < arguments[i].Length; cnt++ ) {
                                    this.options = this.options + "-" + arguments[i][cnt] + " "; // No
                                }
                            } 
                            else {
                                this.options = this.options + arguments[i];       // Yes
                                this.options = this.options + arguments[i] + " ";
                            }
                            i++;
                        }
                        if (arguments.Length > i) {
                            // Safely grab the module, if not specified then
                            //  pass null into the repository...the cvs command
                            //  line for cvsnt/ cvs seems to bomb out when
                            //  it sends to the server
                            this.files = arguments[i++];
                        } 
                        else {
                            this.files = String.Empty;
                        }
                        RemoveCommandParser removeCommand = 
                            new RemoveCommandParser(this.CvsRoot, files, options);
                        command = removeCommand.CreateCommand ();
                        this.currentWorkingDirectory = 
                            removeCommand.CurrentWorkingDirectory;
                        break;
                    case "rt":
                    case "rtag":
                    case "rtfreeze":
                        singleOptions = "abBdfFlMnR";
                        this.commandTxt = arguments[i++];
                        // get rest of arguments which is options on the rtag command.
                        while (arguments.Length > i && arguments[i].IndexOf("-", 0, 1) >= 0) {
                            // Get options with second parameters?
                            if (arguments[i].IndexOfAny( singleOptions.ToCharArray(), 1, 1) >= 0) {
                                for ( int cnt=1; cnt < arguments[i].Length; cnt++ ) {
                                    this.options = this.options + "-" + arguments[i][cnt] + " "; // No
                                }
                            } 
                            else {
                                this.options = this.options + arguments[i];       // Yes
                                this.options = this.options + arguments[i] + " ";
                            }
                            i++;
                        }
                        if (arguments.Length > i) {
                            // Safely grab the module, if not specified then
                            //  pass null into the repository...the cvs command
                            //  line for cvsnt/ cvs seems to bomb out when
                            //  it sends to the server
                            this.repository = arguments[i++];
                        } 
                        else {
                            this.repository = String.Empty;
                        }
                        RTagCommandParser rtagCommand = 
                            new RTagCommandParser(this.CvsRoot, repository, options);
                        command = rtagCommand.CreateCommand ();
                        this.currentWorkingDirectory = 
                            rtagCommand.CurrentWorkingDirectory;
                        break;
                    case "st":
                    case "stat":
                    case "status":
                        string[] commandArgs = new string[arguments.Length - i];
                        Array.Copy(arguments, i, commandArgs, 0, arguments.Length - i);
                        factory = 
                            new CommandParserFactory("status", commandArgs, 
                            this.cvsRoot, this.currentWorkingDirectory);

                        parser = factory.GetCommandParser();
                        i = arguments.Length;
                        command = parser.CreateCommand();
                        this.currentWorkingDirectory = 
                            parser.CurrentWorkingDirectory;
                        break;
                    case "up":
                    case "upd":
                    case "update":
                        singleOptions = "ACPRbdfmp";
                        this.commandTxt = arguments[i++];
                            // get rest of arguments which is options on the update command.
                        while (arguments.Length > i && arguments[i].IndexOf("-", 0, 1) >= 0) {
                            // Get options with second parameters?
                            if (arguments[i].IndexOfAny( singleOptions.ToCharArray(), 1, 1) >= 0) {
                                for ( int cnt=1; cnt < arguments[i].Length; cnt++ ) {
                                    this.options = this.options + "-" + arguments[i][cnt] + " "; // No
                                }
                            } else {
                                this.options = this.options + arguments[i];       // Yes
                                this.options = this.options + arguments[i] + " ";
                            }
                            i++;
                        }
                        if (arguments.Length > i) {
                            // Safely grab the module, if not specified then
                            //  pass null into the repository...the cvs command
                            //  line for cvsnt/ cvs seems to bomb out when
                            //  it sends to the server
                            this.repository = arguments[i++];
                        } 
                        else {
                            this.repository = String.Empty;
                        }
                        UpdateCommandParser updateCommand = 
                            new UpdateCommandParser(this.CvsRoot, repository, options);
                        command = updateCommand.CreateCommand ();
                        this.currentWorkingDirectory = 
                            updateCommand.CurrentWorkingDirectory;
                        break;
                    case "xml":
                        factory = 
                            new CommandParserFactory("xml", arguments, 
                            this.cvsRoot, this.currentWorkingDirectory);

                        // TODO: Move this outside of case statement when all commands use same pattern
                        parser = factory.GetCommandParser();
                        i = arguments.Length;
                        command = parser.CreateCommand();
                        this.currentWorkingDirectory = 
                            parser.CurrentWorkingDirectory;

                        break;
                    default:
                        StringBuilder msg = new StringBuilder ();
                        msg.Append("Unknown command entered.  ");
                        msg.Append("command=[").Append(arguments[i]).Append("]");
                        throw new CommandLineParseException(msg.ToString());
                    }
                }
            return command;
        }

        /// <summary>
        /// <p>
        /// Parse the command line arguments to determine if there are any help
        ///     requests.  If there are help requests then return true, this can
        ///     then be used to direct the flow of the application to not evaulate
        ///     any other commands.
        /// </p>
        /// <p>     
        ///     Also looks at all -- command line arguments, this will be assumed 
        ///     to have been attempts at help but were malformed.
        /// </p>
        /// </summary>
        /// <param name="args"></param>
        /// <returns></returns>
        /// <example>
        /// <br/>        Parses help commands such as:
        /// <br/>           cvs --help
        /// <br/>           cvs --help-options
        /// <br/>           cvs --help-commands
        /// <br/>           cvs --help-synonyms
        /// <br/>           
        /// </example>
        private bool ParseHelp (String[] args) {
            if (args.Length < 1) {
                System.Console.WriteLine(Usage.General);
                return true;
            }
            for (int i = 0; i < arguments.Length; i++) {
                switch (arguments[i]) {
                    case "--help":
                        if (i+1 < arguments.Length) {
                            i++;
                            string command = arguments[i];
                            CommandParserFactory factory = 
                                new CommandParserFactory(command, arguments, this.cvsRoot, 
                                this.currentWorkingDirectory);
                            ICommandParser commandParser = factory.GetCommandParser ();
                            System.Console.WriteLine(commandParser.Usage);
                            return true;
                        } else {
                            System.Console.WriteLine(Usage.General);
                        }
                        return true;
                    case "--help-options":
                        System.Console.WriteLine(Usage.Options);
                        return true;
                    case "--help-commands":
                        System.Console.WriteLine(Usage.Commands);
                        return true;
                    case "--help-synonyms":
                        System.Console.WriteLine(Usage.Synonyms);
			            return true;
		            case "--version":
		                System.Console.WriteLine(Usage.Version);
                        return true;
                }
                if (arguments[i].IndexOf("--") > -1) {
                    System.Console.WriteLine(Usage.General);
                    return true;
                }
            }
            return false;
        }

    }
}
