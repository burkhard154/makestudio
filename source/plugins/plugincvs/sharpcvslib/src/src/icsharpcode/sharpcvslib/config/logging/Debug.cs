#region "Copyright"
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
using System.Xml;
using System.Xml.Serialization;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Config.Logging {
    /// <summary>
    /// Configuration settings for the sharpcvslib debug log.  These are used
    ///     to configure the message delegates on the CvsStream class.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class Debug {

        /// <summary>
        /// Specifies the default file that is used to log request data to if debug
        ///     logging is enabled.
        /// </summary>
        public const String DEFAULT_REQUEST_FILE = "in.txt";
        /// <summary>
        /// Specifies the default file that responses should be logged t if debug
        ///     logging is enabled.
        /// </summary>
        public const String DEFAULT_RESPONSE_FILE = "out.txt";
        /// <summary>
        /// Indicates whether the stack trace will be logged with the request/ response
        ///     message.
        /// </summary>
        public const bool DEFAULT_LOG_STACK_TRACE = false;
        /// <summary>
        /// Indicates whether debug logging will be enabled by default or not.
        /// </summary>
        public const bool DEFAULT_ENABLED = true;

        private bool enabled;
        private String requestFile;
        private String responseFile;
        private bool logStackTrace;

        /// <summary>
        /// Constructor.
        /// </summary>
        public Debug () {
            this.enabled = DEFAULT_ENABLED;
            this.requestFile = DEFAULT_REQUEST_FILE;
            this.responseFile = DEFAULT_RESPONSE_FILE;
            this.logStackTrace = DEFAULT_LOG_STACK_TRACE;
        }

        /// <summary>
        /// <code>true</code> if the debug log is enabled,
        ///     <code>false</code> otherwise.  If this is false nothing
        ///     will be logged to the request and response files.
        ///     
        ///     This is set to <code>DEFAULT_ENABLED</code>
        /// </summary>
        [XmlElement ("enabled", typeof (bool))]
        public bool Enabled {
            get {return this.enabled;}
            set {this.enabled = value;}
        }

        /// <summary>
        /// Configure the name of the file that requests to the cvs
        ///     server are logged to.
        ///     
        ///     This is set to <code>DEFAULT_REQUEST_FILE</code> by default.
        /// </summary>
        [XmlElement ("request-file", typeof (String))]
        public String RequestFile {
            get {return this.requestFile;}
            set {this.requestFile = value;}
        }

        /// <summary>
        /// Configure the name of the file that responses from the cvs server
        ///     are logged to.
        ///     
        ///     This is set to <code>DEFAULT_RESPONSE_FILE</code> by default.
        /// </summary>
        [XmlElement ("response-file", typeof (String))]
        public String ResponseFile {
            get {return this.responseFile;}
            set {this.responseFile = value;}
        }

        /// <summary>
        /// Indicate whether a stack trace should be logged in the in/ out log as
        ///     well as the message to the server.  This will give a better indication
        ///     of what is sending the message and driving the application behavior.
        ///     
        ///     This is set to <code>DEFAULT_LOG_STACK_TRACE</code> by default.
        /// </summary>
        [XmlElement ("log-stack-trace", typeof(bool))]
        public bool LogStackTrace {
            get {return this.logStackTrace;}
            set {this.logStackTrace = value;}
        }

        /// <summary>
        /// Return a human readable representation of the object.
        /// </summary>
        /// <returns>A human readable representation of the object.</returns>
        public override String ToString () {
            ICSharpCode.SharpCvsLib.Util.ToStringFormatter formatter =
                new ICSharpCode.SharpCvsLib.Util.ToStringFormatter("Debug");
            formatter.AddProperty("Enabled", this.Enabled);
            formatter.AddProperty("RequestFile", this.RequestFile);
            formatter.AddProperty("ResponseFile", this.ResponseFile);
            formatter.AddProperty("LogStackTrace", this.LogStackTrace);
            return formatter.ToString();
        }
    }
}
