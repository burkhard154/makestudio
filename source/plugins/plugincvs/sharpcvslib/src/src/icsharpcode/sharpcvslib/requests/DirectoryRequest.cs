#region "Copyright"
// DirectoryRequest.cs
// Copyright (C) 2001 Mike Krueger
// comments are taken from CVS Client/Server reference manual which
// comes with the cvs client (www.cvshome.org)
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
// As a special exception, if you link this library with other files to
// produce an executable, this library does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why the
// executable file might be covered by the GNU General Public License.
#endregion

using System;
using System.Text;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Requests {

    /// <summary>
    /// Additional data: repository \n. Response expected: no.
    /// Tell the server what directory to use. The repository should be a directory
    /// name from a previous server response. Note that this both gives a default
    /// for Entry and Modified and also for ci and the other commands; normal usage
    /// is to send Directory for each directory in which there will be an Entry or
    /// Modified, and then a final Directory for the original directory, then the
    /// command. The local-directory is relative to the top level at which the
    /// command is occurring (i.e. the last Directory which is sent before the
    /// command); to indicate that top level, `.' should be sent for
    /// local-directory.
    /// </summary>
    /// <example>
    /// <p>
    /// Here is an example of where a client gets repository and local-directory.
    /// Suppose that there is a module defined by moddir 1dir
    /// </p>
    /// <p>
    /// That is, one can check out moddir and it will take 1dir in the repository
    /// and check it out to moddir in the working directory. Then an initial
    /// check out could proceed like this:
    /// </p>
    /// <code>
    /// C: Root /home/kingdon/zwork/cvsroot
    /// . . .
    /// C: Argument moddir
    /// C: Directory .
    /// C: /home/kingdon/zwork/cvsroot
    /// C: co
    /// S: Clear-sticky moddir/
    /// S: /home/kingdon/zwork/cvsroot/1dir/
    /// . . .
    /// S: ok
    /// </code>
    /// <p>
    /// In this example the response shown is Clear-sticky, but it could be
    /// another response instead. Note that it returns two pathnames. The
    /// first one, `moddir/', indicates the working directory to check out
    /// into. The second one, ending in `1dir/', indicates the directory to
    /// pass back to the server in a subsequent Directory request. For example,
    /// a subsequent update request might look like:
    /// </p>
    /// <code>
    /// C: Directory moddir
    /// C: /home/kingdon/zwork/cvsroot/1dir
    /// . . .
    /// C: update
    /// </code>
    /// <p>
    /// For a given local-directory, the repository will be the same
    /// for each of the responses, so one can use the repository from
    /// whichever response is most convenient. Typically a client will
    /// store the repository along with the sources for each local-directory,
    /// use that same setting whenever operating on that local-directory,
    /// and not update the setting as long as the local-directory exists.
    /// A client is free to rename a local-directory at any time (for example,
    /// in response to an explicit user request). While it is true that the
    /// server supplies a local-directory to the client, as noted above, this
    /// is only the default place to put the directory. Of course, the various
    ///  Directory requests for a single command (for example, update or ci
    /// request) should name a particular directory with the same local-directory.
    /// Each Directory request specifies a brand-new local-directory and
    /// repository; that is, local-directory and repository are never relative
    /// to paths specified in any previous Directory request.
    /// </p>
    /// </example>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class DirectoryRequest : AbstractRequest {
        private ILog LOGGER = LogManager.GetLogger (typeof (DirectoryRequest));
        private string localdir;
        private string repository;

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="localdir"></param>
        /// <param name="repository"></param>
        public DirectoryRequest(string localdir, string repository) {
            if (null == localdir || string.Empty == localdir) {
                throw new ArgumentException("Unable to process a null directory, use '.'");
            }
            if (null == repository || string.Empty == repository) {
                throw new ArgumentException("Unable to process a null repository, use repository path (i.e. [/cvsroot/sharpcvslib])");
            }
            this.localdir   = localdir;
            this.repository = repository;
        }

        /// <summary>
        /// Specifies the local and repository directories to
        ///     use for a request.
        /// </summary>
        public override string RequestString {
            get {
                return "Directory " + localdir + "\n" + repository + "\n";
            }
        }

        /// <summary>
        /// <code>false</code>, a response is not expected.
        /// </summary>
        public override bool IsResponseExpected {
            get {
                return false;
            }
        }
    }
}
