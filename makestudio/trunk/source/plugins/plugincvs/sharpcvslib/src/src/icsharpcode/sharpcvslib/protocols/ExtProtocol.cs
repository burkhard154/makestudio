#region "Copyright"
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
//  <author>Mike Krueger</author>
//  <author>Clayton Harbour</author>
//
#endregion

using System;
using System.IO;
using System.Text;
using System.Diagnostics;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Config;
using ICSharpCode.SharpCvsLib.Exceptions;
using ICSharpCode.SharpCvsLib.Streams;

using log4net;

namespace ICSharpCode.SharpCvsLib.Protocols {
	/// <summary>
	/// Handle connect and authentication for the pserver protocol.
	/// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
	[Protocol("ext")]
	public class ExtProtocol : AbstractProtocol {
        private const string VERSION_ONE = "-1";
        private const string VERSION_TWO = "-2";

        private readonly ILog LOGGER =
            LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);

        private Process p = null;

        /// <summary>
        /// Create a new instance of the ext (ssh) protocol.
        /// </summary>
		public ExtProtocol() {
		}

        /// <summary>
        /// Connect to the cvs server using the ext method.
        /// </summary>
        public override void Connect() {
            this.HandleExtAuthentication();
        }

        /// <summary>
        /// Disconnect from the cvs server.
        /// </summary>
        public override void Disconnect() {
            if (p != null && !p.HasExited) {
                p.Kill();
                p.WaitForExit();
                p = null;
            }
        }


        private void HandleExtAuthentication () {
            ProcessStartInfo startInfo =
                this.GetProcessInfo(this.Config.Shell, VERSION_ONE);

            try {
                p = new Process();

                p.StartInfo = startInfo;
                p.Exited += new EventHandler(this.ExitShellEvent);

                LOGGER.Info(string.Format("{0} {1}",
                    p.StartInfo.FileName, p.StartInfo.Arguments));

                p.Start();
            } catch (Exception) {
                try {
                    p.StartInfo = this.GetProcessInfo(this.Config.Shell, VERSION_TWO);
                    p.Start();
                } catch (Exception e) {
                    throw new ExecuteShellException(
                        string.Format("{0} {1}",
                        this.Config.Shell, p.StartInfo.Arguments), e);
                }
            }
            BufferedStream errstream = new BufferedStream(p.StandardError.BaseStream);
            StreamWriter streamWriter  = p.StandardInput;
            StreamReader streamReader = p.StandardOutput;

            SetInputStream(new CvsStream (streamReader.BaseStream));
            SetOutputStream(new CvsStream (streamWriter.BaseStream));
        }

        private ProcessStartInfo GetProcessInfo (string program, string version) {
            string tProgram = Path.GetFileNameWithoutExtension(program);
            ProcessStartInfo startInfo;
            switch (tProgram) {
                case "plink": {
                    startInfo = this.GetPlinkProcessInfo(version);
                    break;
                }
                case "ssh": {
                    startInfo = this.GetSshProcessInfo(version);
                    break;
                }
                default:
                    throw new ArgumentException(string.Format("Unknown ssh program specified ( {0} )",
                        this.Config.Shell));
            }
            startInfo.RedirectStandardError  = true;
            startInfo.RedirectStandardInput  = true;
            startInfo.RedirectStandardOutput = true;
            startInfo.UseShellExecute        = false;

            return startInfo;
        }

        private ProcessStartInfo GetPlinkProcessInfo (string version) {
            string args = string.Format("{0} {1} {2} {3} {4}",
                version, "-l", this.Repository.CvsRoot.User, 
                this.Repository.CvsRoot.Host, "\"cvs server\"");

            ProcessStartInfo startInfo =
                new ProcessStartInfo("plink.exe", args.ToString ());

            startInfo.RedirectStandardError  = true;
            startInfo.RedirectStandardInput  = true;
            startInfo.RedirectStandardOutput = true;
            startInfo.UseShellExecute        = false;

            return startInfo;
        }

        private ProcessStartInfo GetSshProcessInfo (string version) {
            string args = string.Format("{0} {1} {2} {3} {4} {5}",
                version.ToString(), "-l", this.Repository.CvsRoot.User, "-q", 
                this.Repository.CvsRoot.Host, "\"cvs server\"");

            ProcessStartInfo startInfo =
                new ProcessStartInfo("ssh.exe", args);

            startInfo.RedirectStandardError  = true;
            startInfo.RedirectStandardInput  = true;
            startInfo.RedirectStandardOutput = true;
            startInfo.UseShellExecute        = false;

            return startInfo;
        }

        private void ExitShellEvent(object sender, EventArgs e) {
            if (LOGGER.IsDebugEnabled) {
                LOGGER.Debug("Process EXITED");
            }

            if (p.ExitCode != 0) {
                throw new AuthenticationException();
            }
        }

	}
}
