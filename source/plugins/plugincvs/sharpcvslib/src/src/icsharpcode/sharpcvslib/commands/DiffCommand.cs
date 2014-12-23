#region "Copyright"
// DiffCommand.cs
// Copyright (C) 2002 Mike Krueger
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
using ICSharpCode.SharpCvsLib.Requests;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.FileSystem;

namespace ICSharpCode.SharpCvsLib.Commands {

    /// <summary>
    /// Diff command
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2002")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class DiffCommand : ICommand {
        private WorkingDirectory workingdirectory;
        private string directory;
        private Entry entry;

        private bool isIgnoringCase          = false;
        private bool isIgnoringAllWhitespace = false;
        private bool isIgnoringBlankLines    = false;
        private bool isIgnoringSpaceChange   = false;

        /// <summary>
        /// Indicator for ignore case
        /// </summary>
        public bool IsIgnoringCase {
            get {
                return isIgnoringCase;
            }
            set {
                isIgnoringCase = value;
            }
        }

        /// <summary>
        /// Indicator for ignoring whitespece
        /// </summary>
        public bool IsIgnoringAllWhitespace {
            get {
                return isIgnoringAllWhitespace;
            }
            set {
                isIgnoringAllWhitespace = value;
            }
        }

        /// <summary>
        /// Indicator for ignoring blank lines
        /// </summary>
        public bool IsIgnoringBlankLines {
            get {
                return isIgnoringBlankLines;
            }
            set {
                isIgnoringBlankLines = value;
            }
        }

        /// <summary>
        /// Indicator for ignoring space change.
        /// </summary>
        public bool IsIgnoringSpaceChange {
            get {
                return isIgnoringSpaceChange;
            }
            set {
                isIgnoringSpaceChange = value;
            }
        }

        /// <summary>
        /// Diff command
        /// </summary>
        /// <param name="workingdirectory"></param>
        /// <param name="directory"></param>
        /// <param name="entry"></param>
        public DiffCommand(WorkingDirectory workingdirectory, string directory, Entry entry)
        {
            this.workingdirectory    = workingdirectory;
            this.directory = directory;
            this.entry = entry;
        }

        /// <summary>
        /// Execute the diff command.
        /// </summary>
        /// <param name="connection"></param>
        public void Execute(ICommandConnection connection)
        {
            connection.SubmitRequest(new DirectoryRequest(".",
                                    workingdirectory.CvsRoot.CvsRepository +
                                    directory));

            if (IsIgnoringCase) {
                connection.SubmitRequest(new ArgumentRequest("-i"));
            }
            if (IsIgnoringAllWhitespace) {
                connection.SubmitRequest(new ArgumentRequest("-w"));
            }
            if (IsIgnoringBlankLines) {
                connection.SubmitRequest(new ArgumentRequest("-B"));
            }
            if (IsIgnoringSpaceChange) {
                connection.SubmitRequest(new ArgumentRequest("-b"));
            }

            connection.SubmitRequest(new ArgumentRequest(entry.Name));
            connection.SubmitRequest(new DiffRequest());
        }
    }
}
