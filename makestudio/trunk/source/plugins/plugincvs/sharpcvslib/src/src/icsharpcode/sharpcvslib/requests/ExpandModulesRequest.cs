#region "Copyright"
// ExpandModulesRequest.cs
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

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Requests {

    /// <summary>
    /// Response expected: yes.
    /// Expand the modules which are specified in the arguments. Returns the data in
    /// Module-expansion responses. Note that the server can assume that this is checkout or export,
    /// not rtag or rdiff; the latter do not access the working directory and thus have no need to
    /// expand modules on the client side. Expand may not be the best word for what this request does.
    /// It does not necessarily tell you all the files contained in a module, for example.
    /// Basically it is a way of telling you which working directories the server needs to know about
    /// in order to handle a checkout of the specified modules. For example, suppose that the server has
    /// a module defined by aliasmodule -a 1dir
    ///
    /// That is, one can check out aliasmodule and it will take 1dir in the repository and check it out
    /// to 1dir in the working directory. Now suppose the client already has this module checked out and
    /// is planning on using the co request to update it. Without using expand-modules, the client would
    /// have two bad choices: it could either send information about all working directories under the
    /// current directory, which could be unnecessarily slow, or it could be ignorant of the fact that
    /// aliasmodule stands for 1dir, and neglect to send information for 1dir, which would lead to
    /// incorrect operation. With expand-modules, the client would first ask for the module to be expanded:
    /// C: Root /home/kingdon/zwork/cvsroot
    /// . . .
    /// C: Argument aliasmodule
    /// C: Directory .
    /// C: /home/kingdon/zwork/cvsroot
    /// C: expand-modules
    /// S: Module-expansion 1dir
    /// S: ok
    ///
    /// and then it knows to check the `1dir' directory and send requests such as Entry and Modified for
    /// the files in that directory.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class ExpandModulesRequest : AbstractRequest {
        /// <summary>
        /// Request the server expands the modules before performing a
        ///     checkout or update.
        /// </summary>
        public override string RequestString {
            get {
                return "expand-modules\n";
            }
        }

        /// <summary>
        /// <code>true</code>, a response is exptected.
        /// </summary>
        public override bool IsResponseExpected {
            get {
                return true;
            }
        }
    }
}
