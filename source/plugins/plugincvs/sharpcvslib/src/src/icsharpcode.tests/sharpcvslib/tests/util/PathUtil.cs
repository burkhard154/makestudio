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
//    <author>Clayton Harbour</author>
//
#endregion

using System;
using System.IO;

namespace ICSharpCode.SharpCvsLib.Tests.Util {
/// <summary>
/// Utility for encapsulating the various path manipulation, temp directory
///     creation for the unit tests.
/// </summary>
public class PathUtil {

    private static readonly String PROJECT_TEST_PATH =
        Path.Combine (Path.GetTempPath (), "sharpcvslib-tests");
    /// <summary>
    /// Private constructor, only static methods in this class.
    /// </summary>
    private PathUtil () {
    }

    /// <summary>
    /// Get a handle to a temporary file on the hard drive.
    ///     Use a guid value for the sub path.
    /// </summary>
    public static String GetTempPath () {
        return GetTempPath (null);
    }
    /// <summary>
    /// Get a handle to a temporary file on the hard drive.  Using the name
    ///     of the calling object to create the path, or if that is null
    ///     then use a guid value for the sub path.
    /// </summary>
    /// <param name="callingObject">A reference to the calling object,
    ///     if this is <code>null</code> then the string value of a
    ///     GUID is used.</param>
    public static String GetTempPath (object callingObject) {
        String subPath;
        try {
            subPath = callingObject.GetType ().Name;
        } catch (NullReferenceException) {
            subPath = Guid.NewGuid ().ToString ();
        }

        String fullPath = Path.Combine (PROJECT_TEST_PATH, subPath);
        if (!Directory.Exists (fullPath)) {
            Directory.CreateDirectory (fullPath);
        }
        return fullPath;
    }
}

}
