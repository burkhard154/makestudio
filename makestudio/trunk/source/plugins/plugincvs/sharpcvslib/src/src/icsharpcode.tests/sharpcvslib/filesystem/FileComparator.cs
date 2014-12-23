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
//    Author:    Clayton Harbour
#endregion

using System;
using System.Collections;
using System.IO;

using log4net;

namespace ICSharpCode.SharpCvsLib.FileSystem {

    /// <summary>
    ///     Compare two files using the following algorithm:
    ///         1) Compare file sizes.
    ///         2) Compare file contents.
    /// </summary>
    public class FileComparator : IComparable {

        String filename;

        /// <summary>
        ///     The name of the file to use in the comparason.
        /// </summary>
        public String Filename {
            get {return this.filename;}
            set {this.filename = value;}
        }
        /// <summary>
        ///     Compare the specified file to the given.  Return
        ///         1 - if this object is greater than the given object.
        ///         0 - if this object is equal to the given object.
        ///        -1 - if this object is less than the given object.
        /// </summary>
        public int CompareTo (object obj) {
            if (obj is FileComparator) {
                FileComparator that = (FileComparator)obj;

                if (File.Exists (this.filename) && File.Exists (that.filename)) {
                    StreamReader thisReader = new StreamReader (this.filename);
                    StreamReader thatReader = new StreamReader (that.filename);

                    String thisContents = thisReader.ReadToEnd ();
                    String thatContents = thatReader.ReadToEnd ();

                    return thisContents.CompareTo (thatContents);
                } else {
                    return 0;
                }
            }
            return -1;
        }

        /// <summary>
        ///     Determine if this object and the object in question are equal.
        /// </summary>
        /// <param name="obj">The object to compare this object against.</param>
        /// <returns><code>true</code> if the objects are equal, or
        ///     <code>false</code> if the two objects are not equal.</returns>
        public override bool Equals (object obj) {
            if (obj is FileComparator) {
                FileComparator that = (FileComparator)obj;
                return this.filename == that.filename;
            }
            return false;
        }

        /// <summary>
        ///     Return the hash code of the file.
        /// </summary>
        /// <returns>The hash code of the file contents.</returns>
        public override int GetHashCode () {
            StreamReader reader = new StreamReader (this.filename);
            String fileContents = reader.ReadToEnd ();
            return fileContents.GetHashCode ();
        }
    }
}
