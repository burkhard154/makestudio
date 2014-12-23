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
using System.Text;
using System.Reflection;

using log4net;

namespace ICSharpCode.SharpCvsLib.Console.Parser {

/// <summary>
/// Contains the usage message for the command line interface.
/// </summary>
public class Usage {
	private static String version;
	private static String titleInfo;
	private static String copyrightInfo;
	private static String companyInfo;
	private static String description;

	private static ILog LOGGER = LogManager.GetLogger(typeof(Usage));

    /// <summary>Private constructor so the class is never instantiated.</summary>
    private Usage () {
        // should never get called.
    }

    /// <summary>Displays default/ general help message.</summary>
    public static String General {
        get {
            return 
@"Usage: cvs [cvs-options] command [command-options-and-arguments]
  where cvs-options are -q, -n, etc.
    (specify --help-options for a list of options)
  where command is add, admin, etc.
    (specify --help-commands for a list of commands
    or --help-synonyms for a list of command synonyms)
  where command-options-and-arguments depend on the specific command
    (specify -H followed by a command name for command-specific help)
  Specify --help to receive this message

The Concurrent Versions System (CVS) is a tool for version control.
For CVS updates and additional information, see
    the #CvsLib home page at http://sharpcvslib.sourceforge.net/ or
    the CVS home page at http://www.cvshome.org/ or
    Pascal Molli's CVS site at http://www.loria.fr/~molli/cvs-index.html
    the CVSNT home page at http://www.cvsnt.org/

Thanks for using the command line tool.";
        }
    }

    /// <summary>Displays usage message for commands.</summary>
    public static String Commands {
        get {
            StringBuilder commandMenu = new StringBuilder();
            commandMenu.Append("CVS commands are:").Append(Environment.NewLine);

            SortedList commands = CommandParserFactory.AllCommands;

            //int FIRST_COLUMN = 8;
            //int SECOND_COLUMN = 12;
            foreach (Command command in commands.Values) {
                commandMenu.Append(String.Format("        {0,-12}{1}",
                    command.CommandName, command.Description));

                if (!command.Implemented) {
                    commandMenu.Append (" (NOT IMPLEMENTED)");
                }
                commandMenu.Append(Environment.NewLine);
            }
            commandMenu.Append("(Specify the --help option for a list of other help options)");

            return commandMenu.ToString();
        }
    }

    /// <summary>Displays usage message for options.</summary>
    public static String Options {
        get {
            return
@"CVS global options (specified before the command name) are:
    -D prefix       Adds a prefix to CVSROOT.
    -H              Displays usage information for command.
    -Q              Cause CVS to be really quiet.
    -q              Cause CVS to be somewhat quiet.
    -r              Make checked-out files read-only.
    -w              Make checked-out files read-write (default).
    -l              Turn history logging off.
    -n              Do not execute anything that will change the disk.
    -t              Show trace of program execution (repeat for more verbosity) -- try with -n.
    -v              CVS version and copyright.
    -T tmpdir       Use 'tmpdir' for temporary files.
    -e editor       Use 'editor' for editing log information.
    -d CVS_root     Overrides $CVSROOT as the root of the CVS tree.
    -f              Do not use the ~/.cvsrc file."
//#ifdef CLIENT_SUPPORT
+ @"
    -z #            Use compression level '#' for net traffic.
    -x              Encrypt all net traffic (fail if not encrypted).
    -y              Encrypt all net traffic (if supported by protocol).
    -a              Authenticate all net traffic."
//#endif
+ @"
    -s VAR=VAL      Set CVS user variable.
    -log:[level]    Sets the logging level.  Levels can be one of [debug|info|warn|error].
    -verbose        Outputs request and response messages.

    --version       CVS version and copyright.
    --encrypt       Encrypt all net traffic (if supported by protocol).
    --authenticate  Authenticate all net traffic (if supported by protocol).
(Specify the --help option for a list of other help options)

Thanks for using the command line tool.";
        }
    }

    /// <summary>Displays commands synonyms.</summary>
    public static string Synonyms {
        get {
            SortedList commands = CommandParserFactory.AllCommands;
            StringBuilder msg = new StringBuilder ();
            msg.Append ("CVS command synonyms are:").Append(Environment.NewLine);
            // loop through commands for synonyms
            foreach(Command command in commands.Values) {
                if (command.Nick1 != null && command.Nick1.Length != 0) {
                    string syn_output = String.Format("        {0,-11}  {1} {2}",
                                                      command.First, command.Nick1, command.Nick2);
                    msg.Append (syn_output);
                    if (!command.Implemented) {
                        msg.Append (" (NOT IMPLEMENTED)");
                    }
                    msg.Append (Environment.NewLine);
                }
            }
            msg.Append ("(Specify the --help option for a list of other help options)");
            msg.Append(Environment.NewLine);
            return msg.ToString ();
        }
    }
    
    private static readonly String currentVersion = Usage.GetVersion();
    
    private static String GetVersion () {
		if (null == version) {
			try {
				Assembly a = typeof(Usage).Assembly;
				version = a.GetName().Version.ToString();
			} catch (Exception e) {
				version = "Unable to retrieve version information.";
				LOGGER.Error(version, e);
			}
		}
        return version;
    }

	private static String GetTitleInfo () {
		if (null == titleInfo) {
			try {
				titleInfo = ((System.Reflection.AssemblyTitleAttribute)
					System.Reflection.Assembly.GetExecutingAssembly().GetCustomAttributes(typeof(System.Reflection.AssemblyTitleAttribute), false)[0]).Title; 
			} catch (Exception e) {
				titleInfo = "Unable to retrieve AssemblyTitleAttribute.";
				LOGGER.Error(titleInfo, e);
			}
		}
		return titleInfo;
	}

	private static String GetCopyrightInfo () {
		if (null == copyrightInfo) {
			try {
				copyrightInfo = ((System.Reflection.AssemblyCopyrightAttribute)
					System.Reflection.Assembly.GetExecutingAssembly().GetCustomAttributes(typeof(System.Reflection.AssemblyCopyrightAttribute), false)[0]).Copyright; 
			} catch (Exception e) {
				copyrightInfo = "Unable to retrieve AssemblyCopyrightAttribute.";
				LOGGER.Error (copyrightInfo, e);
			}
		}
		return copyrightInfo;

	}

	private static String GetDescription () {
		if (null == description) {
			try {
				description = ((System.Reflection.AssemblyDescriptionAttribute)
					System.Reflection.Assembly.GetExecutingAssembly().GetCustomAttributes(typeof(System.Reflection.AssemblyDescriptionAttribute), false)[0]).Description; 
			} catch (Exception e) {
				description = "Unable to retieve AssemblyDescriptionAttribute.";
				LOGGER.Error(description, e);
			}
		}
		return description;
	}

	private static String GetCompanyInfo () {
		if (null == companyInfo) {
			try {
				companyInfo = ((System.Reflection.AssemblyCompanyAttribute)
					System.Reflection.Assembly.GetExecutingAssembly().GetCustomAttributes(typeof(System.Reflection.AssemblyCompanyAttribute), false)[0]).Company; 
			} catch (Exception e) {
				companyInfo = "Unable to retrieve AssemblyCompanyAttribute.";
				LOGGER.Error(companyInfo, e);
			}
		}
		return companyInfo;
	}
    
    /// <summary>Gets a string that contains information about the program and version.</summary>
    public static String Version {
        get{
		
		String programInfo = @"
{0} - {1}
  Build  : {2}
  Runtime: {3}; {4}

Copyright (c) {5}

Specify the --help option for further information about CVS
  or see {6}";
			object[] args = {GetTitleInfo(), 
								GetVersion(), 
								GetDescription(),
								Environment.OSVersion.Platform.ToString(),
								Environment.OSVersion.Version.ToString(), 
								GetCopyrightInfo(),
								GetCompanyInfo()
								};
            return String.Format (programInfo, args);
        }
    }    

}

}
