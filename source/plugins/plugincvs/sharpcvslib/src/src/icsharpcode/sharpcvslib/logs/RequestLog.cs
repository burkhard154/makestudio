#region "Copyright"
//
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

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Config;

using log4net;

namespace ICSharpCode.SharpCvsLib.Logs {
    /// <summary>
    /// Utility class for debugging cvs server responses.  Logging outputs are
    ///     attempted in the following order:
    ///     <ol>
    ///         <li>log4net properties for Type in application config file.</li>
    ///         <li>cvs.out file in the working folder of the assembly.</li>
    ///         <li>console message</li>
    ///     </ol>
    /// A failure at any level causes the program to attempt to log to another
    ///     level.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class RequestLog {
        private SharpCvsLibConfig settings;
        private readonly ILog LOGGER = LogManager.GetLogger (typeof (RequestLog));
        /// <summary>
        /// Constructor.
        ///
        /// // TODO: Write a more useful description.
        /// </summary>
        public RequestLog () {
            this.settings = SharpCvsLibConfig.GetInstance();
        }

        /// <summary>
        /// Log the message.
        ///
        /// // TODO: Write a more useful description.
        /// </summary>
        /// <param name="message">A message to output to the log.</param>
        public void Log (String message) {
            if (null != message || message.Length != 0) {
                StringBuilder msg = new StringBuilder ();
                msg.Append(message);
                if (this.settings.Log.DebugLog.LogStackTrace) {
                    msg.Append("\n Stack Trace:");
                    msg.Append(Environment.StackTrace);
                }
                LOGGER.Debug(msg);
            }
        }
    }
}
