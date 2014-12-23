#region Copyright
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
//    Author: Clayton Harbour
//     claytonharbour@sporadicism.com
#endregion

using System;
using System.Collections;
using System.IO;
using System.Globalization;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.FileSystem {
    /// <summary>
    ///     Value object for the <code>Tag</code> cvs file.  The root file
    ///         holds the cvsroot string.  The cvsroot is a string value
    ///         which has the following information:
    ///             <ol>
    ///                 <li>protocol</li>
    ///                 <li>user@servername.domainname</li>
    ///                 <li>server repository directory</li>
    ///             </ol>
    ///         seperated by a colan(<code>:</code>).
    ///
    ///     eg)     :pserver:anonymous@linux.sporadicism.com:/home/cvs/src/
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class Tag : AbstractCvsFile, ICvsFile {

        /// <summary>
        ///     The name of the root file.
        /// </summary>
        public const String FILE_NAME = "Tag";

        /// <summary>
        ///     The name of the cvs file that the object represents.
        /// </summary>
        public override String Filename {
            get {return Tag.FILE_NAME;}
        }

        /// <summary>
        /// Create a new instance of the cvs object.
        /// </summary>
        /// <param name="cvsFile">The full path to the object being managed.</param>
        /// <param name="fileContents">The contents of the cvs management file.</param>
        public Tag (FileInfo cvsFile, String fileContents) : 
            base (cvsFile, fileContents) {
        }

        public Tag (string fullPath, string fileContents) : 
            this(new FileInfo(System.IO.Path.Combine(fullPath, "CVS")), fileContents) {

        }

        public static Tag Load (DirectoryInfo cvsDir) {
            if (cvsDir.Name != "CVS") {
                cvsDir = new DirectoryInfo(
                    System.IO.Path.Combine(cvsDir.FullName, "CVS"));
            }
            return Load (
                new FileInfo(
                System.IO.Path.Combine(cvsDir.FullName, Tag.FILE_NAME)));
        }

        /// <summary>
        /// Load the root file.
        /// </summary>
        /// <param name="tagFile"></param>
        /// <returns></returns>
        public static Tag Load (FileInfo tagFile) {
            if (tagFile.Name != Tag.FILE_NAME) {
                throw new ArgumentException(string.Format("Not a valid Tag file, {0}",
                    tagFile.FullName));
            }
            Manager manager = new Manager(tagFile.DirectoryName);
            return manager.FetchTag(tagFile.DirectoryName);
        }

        /// <summary>
        /// Parse the contents of the cvs file.
        /// </summary>
        /// <param name="line"></param>
        public override void Parse (String line) {
            if (line.IndexOf('T') == 0) {
                line = "N" + line.Substring (1);
            } else if (line.IndexOf('N') != 0) {
                line = "N" + line;
            }
            this.FileContents = line;
        }

        /// <summary>
        ///     Determine if the two objects are equal.
        /// </summary>
        public override bool Equals (object obj) {
            if (obj is Tag) {
                Tag that = (Tag)obj;
                if (that.GetHashCode ().Equals (this.GetHashCode ())) {
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        ///     Override the hashcode.  This is a combination of the entry
        ///         name and the path to the entry file.
        /// </summary>
        public override int GetHashCode () {
            return this.FileContents.GetHashCode ();
        }

        /// <summary>The type of file that this is.</summary>
        public Factory.FileType Type {get {return Factory.FileType.Tag;}}

        /// <summary>Indicates whether the file can contain multiple
        /// lines.</summary>
        /// <returns><code>true</code> if the file can contain multiple
        /// lines; <code>false</code> otherwise.</returns>
        public bool IsMultiLined {
            get {return false;}
        }

        /// <summary>
        /// Returns the full path to the CVS\Tag file on the local file system
        ///     that this object represents.
        /// </summary>
        /// <returns>The full path to the CVS\Tag file that this object 
        ///     represents.</returns>
        protected override String DeriveCvsFullPath () {
            throw new NotImplementedException("This will eventually return the full path to the repository.");
        }

    }
}
