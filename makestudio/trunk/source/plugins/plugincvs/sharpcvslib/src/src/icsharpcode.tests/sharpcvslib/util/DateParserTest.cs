#region Copyright
// DateParser.cs
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
//
#endregion

using System;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Util {
    /// <summary>
    ///     Test the date parsing utiltiy.
    /// </summary>
    [TestFixture]
    public class DateParserTest {
        private readonly ILog LOGGER = LogManager.GetLogger (typeof (DateParserTest));

        /// <summary>
        ///     Constructor.
        /// </summary>
        public DateParserTest () {
        }

        /// <summary>
        ///     Test that the date is either parsed with a list of good dates.
        /// </summary>
        [Test]
        public void ParseCvsDateGood () {
/*            DateTime date1 = DateParser.ParseCvsDate ("14/09/2003 3:57:48 PM");
            LOGGER.Info(date1.Year);
            AssertDateEquals (date1, 2003, 09, 14, 15, 57, 48);
            DateTime date2 = DateParser.ParseCvsDate ("14/09/2003 1:05:51 AM");
            LOGGER.Info(date2.Year);
            AssertDateEquals (date2, 2003, 09, 14, 1, 05, 51); */
            DateTime date3 = DateParser.ParseCvsDate ("03 Jan 2003 04:07:36 -0000");
            AssertDateEquals (date3, 2003, 1, 3, 4, 7, 36);

            // These dates are in the format found in real Entries file
            DateTime date4 = DateParser.ParseCvsDate ("Thu Jun 12 06:14:16 2003");
            AssertDateEquals (date4, 2003, 6, 12, 6, 14, 16);
            // Following format found in an Entry file using Tortoise as cvs client
            // This is known to cause a problem for revision 1.3 of DateParser
            DateTime date5 = DateParser.ParseCvsDate ("Thu Jun  5 06:14:16 2003");
            AssertDateEquals (date5, 2003, 6, 5, 6, 14, 16);

            // These two entries were not found, but are obvious alternative
            // formats for date5
            DateTime date6 = DateParser.ParseCvsDate ("Thu Jun 5 06:14:16 2003");
            AssertDateEquals (date6, 2003, 6, 5, 6, 14, 16);
            DateTime date7 = DateParser.ParseCvsDate ("Thu Jun 05 06:14:16 2003");
            AssertDateEquals (date7, 2003, 6, 5, 6, 14, 16);
        }

        /// <summary>
        ///     Test that the parser DOES NOT throw an exception with a bad date.
        /// </summary>
        [Test]
        public void ParseCvsDateBad () {
            DateTime date1 = DateParser.ParseCvsDate ("Result of merge");
            DateTime now = DateTime.Now;

            // Since we probably don't have the same now just be happy
            //    with the same day
            AssertDateEquals (date1, now.Year, now.Month, now.Day);
        }

        private void AssertDateEquals (DateTime date,
                                    int year,
                                    int month,
                                    int day) {
            Assert.AreEqual (year, date.Year);
            Assert.AreEqual (month, date.Month);
            Assert.AreEqual (day, date.Day);
        }
        private void AssertDateEquals (DateTime date,
                                    int year,
                                    int month,
                                    int day,
                                    int hour,
                                    int minute,
                                    int second) {
            Assert.AreEqual (year, date.Year);
            Assert.AreEqual (month, date.Month);
            Assert.AreEqual (day, date.Day);
            Assert.AreEqual (hour, date.Hour);
            Assert.AreEqual (minute, date.Minute);
            Assert.AreEqual (second, date.Second);
        }
    }
}
