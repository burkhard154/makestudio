#region "Copyright"
// ConsoleWriter.cs
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
//    <author>Clayton Harbour</author>
#endregion

using System;
using System.Text;
using System.Text.RegularExpressions;

using ICSharpCode.SharpCvsLib.Messages;

namespace ICSharpCode.SharpCvsLib.Console
{
	/// <summary>
	/// Provides a common interface for writing to the console.
	/// </summary>
	public class ConsoleWriter {
        //private bool debug = false;

        private const string REGEX_TEXT = @"text[\s]*([\w]*)";
        private const string REGEX_FNAME = @"fname[\s]*([\w]+[/\w]*[.\w]*)";

        private static Regex TextRegex = new Regex(REGEX_TEXT, RegexOptions.Multiline);
        private static Regex FNameRegex = new Regex(REGEX_FNAME, RegexOptions.Multiline);

        private const string DEFAULT_PREFIX = "";
        private const bool DEFAULT_USE_PREFIX = true;


        private bool usePrefix = DEFAULT_USE_PREFIX;

        private DateTime startTime;
        private DateTime stopTime;

        /// <summary>
        /// <code>true</code> if a prefix should be appended to the message; <code>false</code> otherwise.
        /// </summary>
        /// <value>Default value is <code>true</code>.</value>
        public bool UsePrefix {
            get {return this.usePrefix;}
            set {this.usePrefix = value;}
        }

        /// <summary>
        /// Create a new instance of the console writer.
        /// </summary>
		public ConsoleWriter() {
		}

        /// <summary>
        /// Write the message, without an ending line return.
        /// </summary>
        /// <param name="message"></param>
        public void Write(string message) {
            System.Console.Write(message);
        }

        /// <summary>
        /// Write the message using the default prefix.
        /// </summary>
        /// <param name="message"></param>
        public void WriteLine(string message)  {
            this.WriteLine(message, DEFAULT_PREFIX);
        }

        /// <summary>
        /// Write the message with the given prefix in front.
        /// </summary>
        /// <param name="message"></param>
        /// <param name="prefix"></param>
        public void WriteLine(string message, string prefix) {
            string formattedMessage = null;
            if (UsePrefix && null != prefix && prefix.Length != 0) {
                formattedMessage = String.Format("[{0}]: {1}", prefix, message);
            } else {
                formattedMessage = String.Format("{0}", message);
            }
            System.Console.WriteLine(formattedMessage);
        }

        /// <summary>
        /// Write the given message to the console and include a carriage return at line end.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void WriteLine(object sender, MessageEventArgs e) {
            this.WriteLine(e.Message, e.Prefix);
        }

        /// <summary>
        /// Send a beep to the console and the write the error message.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void WriteError(object sender, MessageEventArgs e) {
            //System.Console.WriteLine( "\a" );
            this.WriteLine(e.Message, e.Prefix);
        }

        /// <summary>
        /// Event that occurs at the start of a process.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void StartProcess(object sender, ProcessEventArgs e) {
            this.startTime = e.Date;
        }

        /// <summary>
        /// Event that occurs at the end of a process.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void StopProcess(object sender, ProcessEventArgs e) {
            this.stopTime = e.Date;
            TimeSpan elapsedTime = this.stopTime.Subtract(this.startTime);
            this.WriteLine(String.Format("\nProcessing time: {0}:{1}:{2}:{3}.",
                elapsedTime.Hours, elapsedTime.Minutes, elapsedTime.Seconds,
                elapsedTime.Milliseconds));

        }
	}
}
