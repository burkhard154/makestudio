#region "Copyright"
// ArgumentRequest.cs
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

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Requests {

    /// <summary>
    /// Response expected: no.
    /// Save argument for use in a subsequent command. Arguments accumulate until
    /// an argument-using command is given, at which point they are forgotten.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class ArgumentRequest : AbstractRequest {
        private string arg;

        /// <summary>The options that are available as
        /// arguments to the cvs server.</summary>
        public class Options {
            /// <summary>The cvs command used to specify a specific revision
            ///     is requested.</summary>
            public const String REVISION = "-r";

            /// <summary>The cvs argument used to specify a revision
            ///     by date.</summary>
            public const String DATE = "-D";

            /// <summary>Cvs command to specify that the name of a cvs
            /// module is comming.</summary>
            public const String MODULE_NAME = "-N";

            /// <summary>Cvs argument to specify that the local directory
            /// will be different than the module directory.</summary>
            public const String OVERRIDE_DIRECTORY = "-d";

            /// <summary>
            /// Send in a dash request.
            /// TODO: Figure out what the dash request actually does.
            /// </summary>
            public const String DASH = "--";
        }

        /// <summary>
        /// An argument to use with the cvs command.
        /// </summary>
        /// <param name="arg">The argument to send to the server.</param>
        public ArgumentRequest(string arg) {
            this.arg = arg;
        }

        /// <summary>
        /// Argument.
        /// </summary>
        public override string RequestString {
            get {
                return "Argument " + arg + "\n";
            }
        }

        /// <summary>
        /// <code>false</code>, response is not expected.
        /// </summary>
        public override bool IsResponseExpected {
            get {
                return false;
            }
        }
    }
}
