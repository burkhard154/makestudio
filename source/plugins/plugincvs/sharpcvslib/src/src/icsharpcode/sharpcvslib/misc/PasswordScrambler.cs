#region "Copyright"
// PasswordScrambler.cs
// Implementation of the unsecure cvs password scrambeling method.
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
#endregion

using System;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Misc {

    /// <summary>
    /// SCRAMBLE and DESCRAMBLE work like this:
    ///
    /// scramble(STR) returns SCRM, a scrambled copy of STR.  SCRM[0] is a
    /// single letter indicating the scrambling method.  As of this
    /// writing, the only legal method is 'A', but check the code for more
    /// up-to-date information.
    ///
    /// descramble(SCRM) returns STR.
    /// descramble() uses SCRM[0] to determine which method of unscrambling
    /// to use.  If it does not recognize the method, it throws an exception.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class PasswordScrambler
    {
        private static readonly ILog LOGGER =
            LogManager.GetLogger (typeof (PasswordScrambler));
        /// <summary>
        /// Map characters to each other randomly and symmetrically, A &lt;--&gt; B.
        ///
        /// We divide the ASCII character set into 3 domains: control chars (0
        /// thru 31), printing chars (32 through 126), and "meta"-chars (127
        /// through 255).  The control chars map _to_ themselves, the printing
        /// chars map _among_ themselves, and the meta chars map _among_
        /// themselves.  Why is this thus?
        ///
        /// No character in any of these domains maps to a character in another
        /// domain, because I'm not sure what characters are legal in
        /// passwords, or what tools people are likely to use to cut and paste
        /// them.  It seems prudent not to introduce control or meta chars,
        /// unless the user introduced them first.  And having the control
        /// chars all map to themselves insures that newline and
        /// carriage-return are safely handled.
        /// </summary>
        private readonly static byte[] shifts = new byte[] {
                                                    0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                                                    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                                                    114,120, 53, 79, 96,109, 72,108, 70, 64, 76, 67,116, 74, 68, 87,
                                                    111, 52, 75,119, 49, 34, 82, 81, 95, 65,112, 86,118,110,122,105,
                                                    41, 57, 83, 43, 46,102, 40, 89, 38,103, 45, 50, 42,123, 91, 35,
                                                    125, 55, 54, 66,124,126, 59, 47, 92, 71,115, 78, 88,107,106, 56,
                                                    36,121,117,104,101,100, 69, 73, 99, 63, 94, 93, 39, 37, 61, 48,
                                                    58,113, 32, 90, 44, 98, 60, 51, 33, 97, 62, 77, 84, 80, 85,223,
                                                    225,216,187,166,229,189,222,188,141,249,148,200,184,136,248,190,
                                                    199,170,181,204,138,232,218,183,255,234,220,247,213,203,226,193,
                                                    174,172,228,252,217,201,131,230,197,211,145,238,161,179,160,212,
                                                    207,221,254,173,202,146,224,151,140,196,205,130,135,133,143,246,
                                                    192,159,244,239,185,168,215,144,139,165,180,157,147,186,214,176,
                                                    227,231,219,169,175,156,206,198,129,164,150,210,154,177,134,127,
                                                    182,128,158,208,162,132,167,209,149,241,153,251,237,236,171,195,
                                                    243,233,253,240,194,250,191,155,142,137,245,235,163,242,178,152 };

        /// <returns>
        /// a scrambled version of text.
        /// </returns>
        public static string Scramble(string text)
        {
            string str = "A";

            if (LOGGER.IsDebugEnabled) {
                String msg = "text to scramble=[" + text + "]";
                LOGGER.Debug (msg);
            }
            if (text == null || text.Length == 0) {
                return str;
            }

            foreach (char ch in text) {
                str += (char)shifts[(byte)ch];
            }
            return str;
        }

        /// <summary>
        /// Unincrypts the specified text.
        /// </summary>
        /// <param name="text">a scrabmled text.</param>
        /// <returns>a descrambled version of text.</returns>
        /// <throws name="ArgumentException">
        ///     when text isn't scambled with a known method.
        /// </throws>
        public static string Descramble(string text)
        {
            if (text == null || text.Length == 0) {
                return 'A'.ToString();
            } else if (text[0] == 'A') {
                // currently the only recognized scrambeling method
                return Scramble(text.Substring(1)).Substring(1);
            } else {
                throw new ArgumentException("This isn't a scrambled text, or an unknown scrambeling method is used.");
            }
        }
    }
}
