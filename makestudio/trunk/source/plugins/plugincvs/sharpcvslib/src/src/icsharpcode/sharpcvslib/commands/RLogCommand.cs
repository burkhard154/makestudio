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
#endregion

using System;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Requests;

namespace ICSharpCode.SharpCvsLib.Commands {
	/// <summary>
	/// Recursively create a log of each file in the given module.
    /// 
    /// Usage: cvs rlog [-lRhtNbT] [-r[revisions]] [-d dates] [-s states]
    ///     [-w[logins]] [files...]
    ///     -l      Local directory only, no recursion.
    ///     -R      Only print name of RCS file.
    ///     -h      Only print header.
    ///     -t      Only print header and descriptive text.
    ///     -T      Use local time not GMT.
    ///     -S      Supress header information when no revisions are selected.
    ///     -N      Do not list tags.
    ///     -b      Only list revisions on the default branch.
    ///     -r[revisions]   Specify revision(s)s to list.
    ///         rev1:rev2   Between rev1 and rev2, including rev1 and rev2.
    ///         rev1::rev2  Between rev1 and rev2, excluding rev1 and rev2.
    ///         rev1:::rev2 Between rev1 and rev2, excluding rev1.
    ///         rev:        rev and following revisions on the same branch.
    ///         rev::       After rev on the same branch.
    ///         :rev        rev and previous revisions on the same branch.
    ///         ::rev       Before rev on the same branch.
    ///         rev         Just rev.
    ///         branch      All revisions on the branch.
    ///         branch.     The last revision on the branch.
    ///     -d dates        Specify dates (D1&lt;D2 for range, D for latest before).
    ///     -s states       Only list revisions with specified states.
    ///     -w[logins]      Only list revisions checked in by specified logins.
    ///
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
	public class RLogCommand : LogCommand, ILogCommand {
        private WorkingDirectory workingDirectory;

        private ILog LOGGER = LogManager.GetLogger (typeof (RLogCommand));


        /// <summary>
        /// 
        /// </summary>
        /// <param name="workingDirectory"></param>
        /// <param name="module"></param>
		public RLogCommand(WorkingDirectory workingDirectory, string module) : base(workingDirectory, null, null) {
            this.workingDirectory = workingDirectory;
		}

        /// <summary>
        /// Execute checkout module command.
        /// </summary>
        /// <param name="connection">Server connection</param>
        public new void Execute(ICommandConnection connection) {
            string relativeDirectory = this.workingDirectory.ModuleName;

            // Note: don't use Path.Combine() as the separator must be "/"
            string repositoryDir = workingDirectory.CvsRoot.CvsRepository + "/" + relativeDirectory;
            connection.SubmitRequest(new DirectoryRequest(relativeDirectory, repositoryDir));

            if (DefaultBranch) {
                connection.SubmitRequest(new ArgumentRequest("-b"));
            }
            if (HeaderAndDescOnly) {
                connection.SubmitRequest(new ArgumentRequest("-t"));
            }
            if (HeaderOnly) {
                connection.SubmitRequest(new ArgumentRequest("-h"));
            }
            if (NoTags) {
                connection.SubmitRequest(new ArgumentRequest("-N"));
            }
        
            // add any date arguments
            foreach (object o in base.DateArgs) {
                string dateArg = (string)o;
                connection.SubmitRequest(new ArgumentRequest("-d"));
                connection.SubmitRequest(new ArgumentRequest(dateArg));
            }

            connection.SubmitRequest(new ArgumentRequest(workingDirectory.ModuleName));

            connection.SubmitRequest(new RLogRequest());
        }

	}
}
