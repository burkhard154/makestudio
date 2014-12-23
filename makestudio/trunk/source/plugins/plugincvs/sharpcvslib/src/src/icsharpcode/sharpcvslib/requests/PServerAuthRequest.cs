#region "Copyright"
// PServerAuthRequest.cs
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

using System;
using System.Collections;
using System.IO;
using System.Text;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Misc;

using log4net;

namespace ICSharpCode.SharpCvsLib.Requests {

    /// <summary>
    /// this isn't an official request, this is the authorization for the
    /// pserver protocol.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class PServerAuthRequest : AbstractRequest {
        private const String VAR_HOME = "HOME";
        private const String CVSPASS = ".cvspass";
        private string cvsroot;
        private string userName;
        private string password;

        private readonly ILog LOGGER =
            LogManager.GetLogger (typeof (PServerAuthRequest));

        /// <summary>
        /// The cvs root.
        /// </summary>
        public String CvsRoot {
            get {return this.cvsroot;}
        }

        /// <summary>
        /// The name of the user that is logging in.
        /// </summary>
        public String UserName {
            get {return this.userName;}
        }

        /// <summary>
        /// Get the password for the user.
        /// </summary>
        public String Password {
            get {
                if (null == this.password || this.password.Length == 0) {
                    String home = Environment.GetEnvironmentVariable(VAR_HOME);
                    if (null != home) {
                        String varHomePath = 
                            Path.Combine(home, CVSPASS);

                        try {
                            StreamReader reader = 
                                new StreamReader(varHomePath);
                            ArrayList passwords = new ArrayList ();
                            String passLine = reader.ReadLine();
                            while (passLine.Length != 0) {
                                LOGGER.Debug("passLine=[" + passLine + "]");
                                String passFileCvsRoot = passLine.Substring(0, passLine.IndexOf(" "));
                                String passFilePassword = passLine.Substring(passLine.IndexOf(" "), passLine.Length);

                                if (null != passFileCvsRoot && 
                                    passFileCvsRoot.Length != 0 &&
                                    this.cvsroot.Equals (passFileCvsRoot)) {
                                    this.password = passFilePassword;
                                    break;
                                }
                                passLine = reader.ReadLine();
                            }
                        } catch (IOException e) {
                            LOGGER.Error(e);
                        }

                        StringBuilder msg = new StringBuilder ();
                        msg.Append("Password was null, looking up password from ");
                        msg.Append(VAR_HOME).Append("=[").Append(varHomePath).Append("]");
                        msg.Append("Password=[").Append(password).Append("]");
                        LOGGER.Debug(msg);
                    }
                }
                LOGGER.Debug("Returning password=[" + this.password + "]");
                return this.password;
            }
        }

        /// <summary>
        /// Creates a new instance of the pserver authentication request with a null 
        ///     password.
        /// </summary>
        /// <param name="cvsroot">A cvsroot line that locates the repository and
        ///     the server.</param>
        /// <param name="username">The name of the user that is logging in.</param>
        public PServerAuthRequest (string cvsroot, string username) : this (cvsroot, username, null) {
        }

        /// <summary>
        /// Create a new pserver authentication object.  Populate the cvsroot, username
        ///     and password variables that will be sent to the server.  When the password
        ///     is populated it is encrypted with the password scrambler.
        /// </summary>
        /// <param name="cvsroot">A cvsroot line that locates the repository and
        ///     the server.</param>
        /// <param name="userName">The name of the user that is logging in.</param>
        /// <param name="password">A password for the user.</param>
        public PServerAuthRequest(string cvsroot, string userName, string password)
        {
            this.cvsroot  = cvsroot;
            this.userName = userName;
            this.password = password;
            this.password = PasswordScrambler.Scramble(this.password);
        }

        /// <summary>
        /// Authorization for the pserver protocol.
        /// </summary>
        public override string RequestString {
            get {
                return "BEGIN AUTH REQUEST\n" +
                    CvsRoot + "\n" +
                    UserName + "\n" +
                    Password + "\n" +
                    "END AUTH REQUEST\n";
            }
        }

        /// <summary>
        /// <code>false</code>, a response is not expected.
        /// </summary>
        public override bool IsResponseExpected {
            get {return false;}
        }
    }
}
