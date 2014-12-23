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
#endregion

using System;
using System.IO;

namespace ICSharpCode.SharpCvsLib.Tests.Config {

    /// <summary>
    ///     Holds constants used for test cases.
    /// </summary>
    public class TestConstants {
        /// <summary>
        ///     The local path for the test.
        /// </summary>
        public static readonly String LOCAL_PATH =
            Path.Combine (Path.GetTempPath (), "sharpcvslib-tests" + 
            Path.DirectorySeparatorChar);
        /// <summary>
        ///     Cvs root to use for test cases.
        /// </summary>
        public const String CVSROOT =
            ":pserver:anonymous@goliath.sporadicism.com:/cvsroot/sharpcvslib-test";
        /// <summary>
        ///     Project/ module to use.
        /// </summary>
        public const String MODULE = "sharpcvslib-test-repository";
        /// <summary>
        ///     The valid password for a login.
        /// </summary>
        public const String PASSWORD_VALID = "";
        /// <summary>
        ///     Cvs server response if password is invalid.
        /// </summary>
        public const String PASSWORD_INVALID = "I_HATE_YOU_TOO";
        /// <summary>
        ///     The file that will be checked after update/ checkout.
        /// </summary>
        public const String TARGET_FILE = "test-file.txt";
        /// <summary>
        ///     The directory that will be searched for after an update/ checkout.
        /// </summary>
        public const String TARGET_DIRECTORY = "src";

        /// <summary>
        ///     The override directory that will be used instead of the module
        ///         name.
        /// </summary>
        public const String OVERRIDE_DIRECTORY = "override_root";

        /// <summary>
        ///     Holds constants for the revision tests.
        /// </summary>
        public class Revision {
            /// <summary>The revision to fetch.</summary>
            public const String TAG_1 = "V0_1";

            /// <summary>The revision to fetch.</summary>
            public const String TAG_2 = "V0_2";

            /// <summary>Contents of the test file that should match the contents
            ///     at the time of the first revision tag.</summary>
            public const string CONTENT_1 = "Original File checkin.";

            /// <summary>Contents of the test file that should match the contents
            ///     at the time of the first revision tag.</summary>
            public const string CONTENT_2 = @"Original File checkin.

                                            Modified file, added line after initial tag.";

        }

    }
}
