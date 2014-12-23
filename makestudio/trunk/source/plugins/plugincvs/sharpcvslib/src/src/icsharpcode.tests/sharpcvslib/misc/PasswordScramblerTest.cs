#region "Copyright"
// Copyright (C) 2003 Gerald Evans
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
//    <author>Gerald Evans</author>
#endregion

using System;
using System.Text;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Misc;

using log4net;
using NUnit.Framework;

namespace ICSharpCode.SharpCvsLib.Misc {

/// <summary>
///     Test the PasswordScrambler.
/// </summary>
[TestFixture]
public class PasswordScramblerTest {
    // This list of character mappings extracted directly from
    // 'cvsclient.texi' in the cvs distribution.
    //
    // Note: The PasswordScrambler class also performs maping of characters
    // above 127, but I have not found any documentation on this and
    // 'cvsclient.texi' states that these characters shouldn't be used in
    // passwords anyway.
    private readonly char[,] charMapping = {
                                               { '!', (char)120 },
                                               { '"', (char)53 },
                                               { '%', (char)109 },
                                               { '&', (char)72 },
                                               { '\'', (char)108 },
                                               { '(', (char)70 },
                                               { ')', (char)64 },
                                               { '*', (char)76 },
                                               { '+', (char)67 },
                                               { ',', (char)116 },
                                               { '-', (char)74 },
                                               { '.', (char)68 },
                                               { '/', (char)87 },
                                               { '0', (char)111 },
                                               { '1', (char)52 },
                                               { '2', (char)75 },
                                               { '3', (char)119 },
                                               { '4', (char)49 },
                                               { '5', (char)34 },
                                               { '6', (char)82 },
                                               { '7', (char)81 },
                                               { '8', (char)95 },
                                               { '9', (char)65 },
                                               { ':', (char)112 },
                                               { ';', (char)86 },
                                               { '<', (char)118 },
                                               { '=', (char)110 },
                                               { '>', (char)122 },
                                               { '?', (char)105 },
                                               { 'A', (char)57 },
                                               { 'B', (char)83 },
                                               { 'C', (char)43 },
                                               { 'D', (char)46 },
                                               { 'E', (char)102 },
                                               { 'F', (char)40 },
                                               { 'G', (char)89 },
                                               { 'H', (char)38 },
                                               { 'I', (char)103 },
                                               { 'J', (char)45 },
                                               { 'K', (char)50 },
                                               { 'L', (char)42 },
                                               { 'M', (char)123 },
                                               { 'N', (char)91 },
                                               { 'O', (char)35 },
                                               { 'P', (char)125 },
                                               { 'Q', (char)55 },
                                               { 'R', (char)54 },
                                               { 'S', (char)66 },
                                               { 'T', (char)124 },
                                               { 'U', (char)126 },
                                               { 'V', (char)59 },
                                               { 'W', (char)47 },
                                               { 'X', (char)92 },
                                               { 'Y', (char)71 },
                                               { 'Z', (char)115 },
                                               { '_', (char)56 },
                                               { 'a', (char)121 },
                                               { 'b', (char)117 },
                                               { 'c', (char)104 },
                                               { 'd', (char)101 },
                                               { 'e', (char)100 },
                                               { 'f', (char)69 },
                                               { 'g', (char)73 },
                                               { 'h', (char)99 },
                                               { 'i', (char)63 },
                                               { 'j', (char)94 },
                                               { 'k', (char)93 },
                                               { 'l', (char)39 },
                                               { 'm', (char)37 },
                                               { 'n', (char)61 },
                                               { 'o', (char)48 },
                                               { 'p', (char)58 },
                                               { 'q', (char)113 },
                                               { 'r', (char)32 },
                                               { 's', (char)90 },
                                               { 't', (char)44 },
                                               { 'u', (char)98 },
                                               { 'v', (char)60 },
                                               { 'w', (char)51 },
                                               { 'x', (char)33 },
                                               { 'y', (char)97 },
                                               { 'z', (char)62 }
                                           };

    /// <summary>
    ///     Tests each character in turn to make sure it scrambles to what
    ///     is specified in 'cvsclient.texi'.
    /// </summary>
    [Test]
    public void IndividualCharScrambleTest () {
        int index;
        String input;
        String expectedOutput;
        String actualOutput;

        for (index = 0; index < charMapping.GetLength(0); index++)
        {
            input = new String(charMapping[index, 0], 1);
            expectedOutput = "A" + new String(charMapping[index, 1], 1);
            actualOutput = PasswordScrambler.Scramble(input);
            Assert.IsTrue(actualOutput.Equals(expectedOutput));

            // Just to convince myself that we have iterated over all chars specified in the charMapping
            if (index == 0)
            {
                Assert.IsTrue(input.Equals("!"));
            }
            else if (index == charMapping.GetLength (0) - 1)
            {
                Assert.IsTrue(input.Equals("z"));
            }
        }
    }

    /// <summary>
    ///     Creates a string containing all characters specified in 'cvsclient.texi'
    ///     and checks that this scrambles to the expected result.
    /// </summary>
    [Test]
    public void AllCharsScrambleTest () {
        int index;
        StringBuilder input;
        StringBuilder expectedOutput;
        String actualOutput;

        input = new StringBuilder();
        expectedOutput = new StringBuilder();

        expectedOutput.Append("A");

        for (index = 0; index < charMapping.GetLength (0); index++)
        {
            input.Append(charMapping[index, 0]);
            expectedOutput.Append(charMapping[index, 1]);
        }
        actualOutput = PasswordScrambler.Scramble(input.ToString());
        Assert.AreEqual(expectedOutput.ToString(), actualOutput);
    }

    /// <summary>
    ///     Checks that Descramble(Scramble(s)) returns the original string.
    ///     This test uses a string containing all possible characters in an
    ///     8 bit character set with the exception of '\0'.
    /// </summary>
    [Test]
    public void ScrambleDescrambleTest () {
        char ch;
        StringBuilder input;
        String scrambledOutput;
        String descrambledOutput;

        input = new StringBuilder();

        for (ch = (char)1; ch <= (char)0xFF; ch++)
        {
            input.Append(ch);
        }
        scrambledOutput = PasswordScrambler.Scramble(input.ToString());
        descrambledOutput = PasswordScrambler.Descramble(scrambledOutput);
        Assert.AreEqual(input.ToString(), descrambledOutput);
    }
}
}
