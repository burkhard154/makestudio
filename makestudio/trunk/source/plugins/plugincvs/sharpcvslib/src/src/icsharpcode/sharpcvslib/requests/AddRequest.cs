#region "Copyright"
// AddRequest.cs
// Copyright (C) 2001 Mike Krueger
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
    /// Response expected: yes. Add a file or directory. This uses any previous Argument, Directory,
    /// Entry, or Modified requests, if they have been sent. The last Directory sent specifies the
    /// working directory at the time of the operation. To add a directory, send the directory to be
    /// added using Directory and Argument requests. For example:
    /// C: Root /u/cvsroot
    /// . . .
    /// C: Argument nsdir
    /// C: Directory nsdir
    /// C: /u/cvsroot/1dir/nsdir
    /// C: Directory .
    /// C: /u/cvsroot/1dir
    /// C: add
    /// S: M Directory /u/cvsroot/1dir/nsdir added to the repository
    /// S: ok
    ///
    /// You will notice that the server does not signal to the client in any particular way that the
    /// directory has been successfully added. The client is supposed to just assume that the directory
    /// has been added and update its records accordingly. Note also that adding a directory is immediate;
    /// it does not wait until a ci request as files do. To add a file, send the file to be added using a
    /// Modified request. For example:
    /// C: Argument nfile
    /// C: Directory .
    /// C: /u/cvsroot/1dir
    /// C: Modified nfile
    /// C: u=rw,g=r,o=r
    /// C: 6
    /// C: hello
    /// C: add
    /// S: E cvs server: scheduling file `nfile' for addition
    /// S: Mode u=rw,g=r,o=r
    /// S: Checked-in ./
    /// S: /u/cvsroot/1dir/nfile
    /// S: /nfile/0///
    /// S: E cvs server: use 'cvs commit' to add this file permanently
    /// S: ok
    ///
    /// Note that the file has not been added to the repository; the only effect of a successful add
    /// request, for a file, is to supply the client with a new entries line containing `0' to indicate an
    /// added file. In fact, the client probably could perform this operation without contacting the server,
    /// although using add does cause the server to perform a few more checks. The client sends a subsequent
    /// ci to actually add the file to the repository. Another quirk of the add request is that with CVS 1.9
    /// and older, a pathname specified in an Argument request cannot contain `/'. There is no good reason
    /// for this restriction, and in fact more recent CVS servers don't have it. But the way to interoperate
    /// with the older servers is to ensure that all Directory requests for add (except those used to
    /// add directories, as described above), use `.' for local-directory. Specifying another string for
    /// local-directory may not get an error, but it will get you strange Checked-in responses from the buggy
    /// servers.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2004-2005")]
    public class AddRequest : AbstractRequest {
        /// <summary>
        /// The string to send to the cvs server.
        /// </summary>
        public override string RequestString {
            get {return "add\n";}
        }

        /// <summary>
        /// <code>true</code>, a response is expected for this
        ///     command.
        /// </summary>
        public override bool IsResponseExpected {
            get {return true;}
        }
    }
}
