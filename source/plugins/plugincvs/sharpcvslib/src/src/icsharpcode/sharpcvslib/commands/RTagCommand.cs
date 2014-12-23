#region "Copyright"
//
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

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Requests;

namespace ICSharpCode.SharpCvsLib.Commands {
	/// <summary>
	/// Recursively tags a given module.
	/// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003, 2005")]
	public class RTagCommand : ICommand {
        private WorkingDirectory workingDirectory;

        private ILog LOGGER = LogManager.GetLogger (typeof (RTagCommand));

        private class Options {
            public const String CLEAR_TAG = "-a";
            public const String CREATE_BRANCH = "-b";
            public const String DELETE_TAG = "-d";
        }

        /// <summary>
        /// Perform an rtag against the repository.  The rtag command has a number
        ///     of request options that may be specified.  These are translated into
        ///     boolean values where possible to calling classes.  A summary of 
        ///     available command options is listed below and is listed in the private
        ///     options classes.
        ///    
        ///         -D date
        ///             Tag the most recent revision no later than date. 
        ///         -f
        ///             Only useful with the `-D date' or `-r tag' flags. 
        ///             If no matching revision is found, use the most recent 
        ///             revision (instead of ignoring the file). 
        ///         -F
        ///             Overwrite an existing tag of the same name on a different 
        ///             revision. 
        ///         -l
        ///             Local; run only in current working directory. 
        ///         -n
        ///             Do not run any tag program that was specified with the `-t' 
        ///             flag inside the `modules' file. (see section The modules 
        ///             file). 
        ///         -R
        ///             Tag directories recursively. This is on by default. 
        ///         -r tag
        ///             Only tag those files that contain tag. This can be used to 
        ///             rename a tag: tag only the files identified by the old tag, 
        ///             then delete the old tag, leaving the new tag on exactly the 
        ///             same files as the old tag. 
        ///
        ///         In addition to the above common options, these options are 
        ///         available:
        ///
        ///         -a
        ///             Use the `-a' option to have rtag look in the `Attic' 
        ///             (see section The attic) for removed files that contain the 
        ///             specified tag. The tag is removed from these files, which 
        ///             makes it convenient to re-use a symbolic tag as development 
        ///             continues (and files get removed from the up-coming 
        ///             distribution). 
        ///         -b
        ///             Make the tag a branch tag. See section Branching and merging. 
        ///         -d
        ///             Delete the tag instead of creating it. In general, tags 
        ///             (often the symbolic names of software distributions) should 
        ///             not be removed, but the `-d' option is available as a means 
        ///             to remove completely obsolete symbolic names if necessary 
        ///             (as might be the case for an Alpha release, or if you 
        ///             mistagged a module).  
        ///             
        /// </summary>
        /// <param name="workingDirectory">The working directory which contains
        ///     the repository information.</param>
		public RTagCommand(WorkingDirectory workingDirectory) {
            this.workingDirectory = workingDirectory;
		}

        private bool deleteTag = false;
        /// <summary>
        /// <code>true</code> if the given tag should be deleted; defaults to 
        ///     <code>false</code>.
        /// </summary>
        public bool DeleteTag {
            get {return this.deleteTag;}
            set {this.deleteTag = value;}
        }

        private String tagName;
        /// <summary>
        /// The name of the tag.
        /// </summary>
        public String TagName {
            get {return this.tagName;}
            set {this.tagName = value;}
        }

        /// <summary>
        /// Execute checkout module command.
        /// </summary>
        /// <param name="connection">Server connection</param>
        public void Execute(ICommandConnection connection) {
            connection.SubmitRequest(new CaseRequest());

            if (this.deleteTag) {
                connection.SubmitRequest(new ArgumentRequest(Options.DELETE_TAG));
            }

            connection.SubmitRequest(new ArgumentRequest(TagName));
            connection.SubmitRequest(new ArgumentRequest(workingDirectory.ModuleName));

            connection.SubmitRequest(new RTagRequest());
        }

	}
}
