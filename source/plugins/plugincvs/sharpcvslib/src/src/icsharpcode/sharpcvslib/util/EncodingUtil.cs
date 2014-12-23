#region "Copyright"
// EncodingUtil.cs
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
using System.Text;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Config;

using log4net;

namespace ICSharpCode.SharpCvsLib.Util {
    /// <summary>
    /// Wraps the encoding class so that there is a consistant strategy throughout
    ///     the application.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class EncodingUtil {
        private static readonly ILog LOGGER = LogManager.GetLogger(typeof(EncodingUtil));
        /// <summary>
        /// Private constructor, all methods are static.
        /// </summary>
        private EncodingUtil () {

        }

        /// <summary>
        /// The default encoding to use for the application.
        /// </summary>
        public static Encoding DEFAULT_ENCODING {
            get {return SharpCvsLibConfig.DefaultEncoding;}
        }

        /// <summary>
        /// Converts the byte stream to a Unicode array of charaters and then reads
        ///     the specified number of characters from the initial offset point.
        /// </summary>
        /// <param name="array">A byte array that contains the data to convert.</param>
        /// <param name="offset">A point to start reading from the array.</param>
        /// <param name="count">The number of bytes to read from the array.</param>
        public static String GetString (byte[] array, int offset, int count) {
            Encoding encoding = DEFAULT_ENCODING;
            return encoding.GetString (array, offset, count);
        }

        /// <summary>
        /// Converts the byte value to a String using the default encoding.
        /// </summary>
        /// <param name="val">A byte value to convert to a String.</param>
        public static String GetString (byte val) {
            Encoding encoding = DEFAULT_ENCODING;
            byte[] array = {val};
            return encoding.GetString (array);
        }

        /// <summary>
        /// Get the string value of the integer by converting it back to a byte
        ///     first.
        /// </summary>
        /// <param name="val">A byte value that has been cast to an integer
        ///     by the Stream.ReadByte() method.</param>
        public static String GetString (int val) {
            try {
                if (val > 0) {
                    return EncodingUtil.GetString (System.Convert.ToByte(val));
                } else {
                    return val.ToString();
                }
            } catch (OverflowException e) {
                StringBuilder msg = new StringBuilder ();
                msg.Append ("val=[").Append(val).Append("]");
                LOGGER.Error(msg, e);
                throw e;
            }
        }
    }
}
