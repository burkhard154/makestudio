#region "Copyright"
// WorkingDirectory.cs
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
#endregion

using System;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Misc {

    /// <summary>
    ///     The trace utility class logs a stack trace to the log4net file
    ///         if DEBUG logging is enabled.  This is used as an alternative
    ///         to the .net trace mechanism.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class TraceUtil {

        private static readonly ILog LOGGER =
            LogManager.GetLogger (typeof (TraceUtil));

        /// <summary>
        ///     Create a new instance of the trace util.
        /// </summary>
        public TraceUtil () {
        }

        /// <summary>
        ///     Log a debug message with a stack trace.
        /// </summary>
        /// <param name="message">The message to log with the stack
        ///     trace.</param>
        public static void Debug (String message) {
            if (LOGGER.IsDebugEnabled) {
                System.Diagnostics.StackTrace trace =
                    new System.Diagnostics.StackTrace ();

                String msg = "";
                if (null != message) {
                    msg = msg + "message=[" + message + "]";
                }
                msg = msg + "stackTrace=[" + trace.ToString () + "]";
                LOGGER.Debug (msg);
            }
        }

        /// <summary>
        ///     Log stack trace.
        /// </summary>
        public static void Debug () {
            TraceUtil.Debug (null);
        }
    }
}
