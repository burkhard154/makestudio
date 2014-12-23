#region "Copyright"
// CVSServerConnection.cs
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
//    Author:     Clayton Harbour
//
#endregion

using System;
using System.Globalization;

using log4net;
using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Util {
    /// <summary>
    ///     Utility class to parse a string value into a date.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class DateParser {

        /// <summary>
        ///     Date format for the <code>RFC1123</code> specification.
        /// </summary>
        public const String RFC1123 =
            "dd MMM yyyy HH':'mm':'ss '-0000'";
        /// <summary>
        ///     The date format used by cvsnt.
        /// </summary>
        public const String CVSNT1 =
            "ddd MMM  d HH':'mm':'ss yyyy";    // single 'd' to support both single & double digit dates
        //	        "ddd MMM dd HH':'mm':'ss yyyy";
        //	        "ddd MMM dd HH:mm:ss yyyy";
        public const String CVS_SINGLE_DAY =
            "ddd MMM  d HH':'mm':'ss yyyy";    // single 'd' to support both single & double digit dates
        public const String CVS_DOUBLE_DAY =
            "ddd MMM dd HH':'mm':'ss yyyy";    // single 'd' to support both single & double digit dates



        /// <summary>
        ///     Private constructor because all accessor methods are going to
        ///         be static public.
        /// </summary>
        private DateParser () {
        }

        /// <summary>
        ///     Parse the date string using a number of different potential
        ///         cvs date formats.
        /// </summary>
        public static DateTime ParseCvsDate (String date) {
            DateTime dateTime = DateTime.Now;

            if (date != null && date.Length > 0)  {
                try {
                    dateTime = DateParser.ParseRFC1123 (date);
                } catch (FormatException) {
                    try {
                        dateTime = DateParser.ParseRFC1123WithZero (date);
                    } catch (FormatException) {
                        try {
                            dateTime = DateParser.ParseCvsNT1 (date);
                        } catch (FormatException) {
                            try {
                                dateTime = DateTime.Parse (date);
                            } catch (FormatException) {
                                dateTime = DateTime.Now;
                            }
                        }
                    }
                }
            }

            return dateTime;
        }

        /// <summary>
        ///     Create a cvs date string given the date time.
        /// </summary>
        /// <param name="date">The date to convert to a string.</param>
        /// <returns>The date as a cvs formatted date string.</returns>
        public static String GetCvsDateString (DateTime date) {
            string dateString;
            if (date.Day < 10) {
                dateString = date.ToString(DateParser.CVS_SINGLE_DAY,
                    DateTimeFormatInfo.InvariantInfo);
            } else {
                dateString = date.ToString(DateParser.CVS_DOUBLE_DAY,
                    DateTimeFormatInfo.InvariantInfo);
            }
            return dateString;
        }

        /// <summary>
        ///     Apply the correct UTC offset to the given time.  This is done
        ///         to correct a bug in the .net framework.
        /// </summary>
        /// <param name="timeStamp">The timestamp to be corrected.</param>
        /// <returns>The corrected timestamp for the file.</returns>
        public static DateTime GetCorrectedTimeStamp (DateTime timeStamp) {
            return timeStamp.Add (System.TimeZone.CurrentTimeZone.GetUtcOffset (timeStamp));
        }


        private static DateTime ParseRFC1123 (String date) {
            return DateTime.ParseExact(date,
                                    RFC1123,
                                    DateTimeFormatInfo.InvariantInfo);
        }

        private static DateTime ParseRFC1123WithZero (String date) {
            return DateTime.ParseExact("0" + date,
                                    RFC1123,
                                    DateTimeFormatInfo.InvariantInfo);
        }

        private static DateTime ParseCvsNT1 (String date) {
            // These dates sometimes contain space padded 'day of month' rather than
            // zero padded which results in a double space.  Hence the AllowWhiteSpaces.
            return DateTime.ParseExact(date,
                                    CVSNT1,
                                    DateTimeFormatInfo.InvariantInfo,
                                    DateTimeStyles.AllowWhiteSpaces);
        }

    }
}
