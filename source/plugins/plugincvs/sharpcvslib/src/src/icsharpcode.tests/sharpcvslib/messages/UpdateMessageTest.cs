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
//    Author        Clayton Harbour
//
#endregion

using System;
using System.Text;

using log4net;
using NUnit.Framework;

using ICSharpCode.SharpCvsLib.Messages;

namespace ICSharpCode.SharpCvsLib.Messages {

/// <summary>
///     Ensures the content of the update message are consistent with other
///         command line cvs clients.
/// </summary>
[TestFixture]
public class UpdateMessageTest {
    private const String MODULE = "Sharpcvslib";
    private const String REPOSITORY = "sharpcvslib";
    private const String FILENAME = "theFile.txt";

    /// <summary>
    ///     Test that the message output is formatted correctly if there
    ///         are no slashes on the end of the seperate values.
    /// </summary>
    [Test]
    public void MessageFormattedCorrectlyNoSlashes () {
        UpdateMessage message = new UpdateMessage ();
        message.Module = MODULE;
        message.Repository = REPOSITORY;
        message.Filename = FILENAME;

        String cvsMessage = "U " + MODULE + "/" + REPOSITORY +
                            "/" + FILENAME;
        Assert.AreEqual (cvsMessage, message.Message);
    }

    /// <summary>
    ///     Test that the message is formatted correctly if there are slashes
    ///         on the end of the input values.
    /// </summary>
    [Test]
    public void MessageFormattedCorrectlyWithSlashes () {
        UpdateMessage message = new UpdateMessage ();
        message.Module = MODULE + "/";
        message.Repository = REPOSITORY + "/";
        message.Filename = FILENAME + "/";

        String cvsMessage = "U " + MODULE + "/" + REPOSITORY +
                            "/" + FILENAME;

        System.Console.WriteLine ("made=[" + cvsMessage + "]");
        System.Console.WriteLine ("generated=[" + message.Message + "]");
        Assert.AreEqual (cvsMessage, message.Message);
    }
}
}
