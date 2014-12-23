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
using System.Collections;
using System.IO;
using System.Text;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.FileSystem {
    /// <summary>
    /// Takes a single file or a collection of files and creates a new
    ///     list of files based on the following rules:
    ///         1) If a single file is specified then a check is performed to
    ///             determine if the file exists.
    ///                 a) If the file does not exist then the NonExistingFiles
    ///                     collection is populated.
    ///                 b) If the file doe exist then the ExistingFile collection
    ///                     is populated.
    ///         2) If a collection of files is specified, WITHOUT a directory
    ///             then a non-recursive search is performed.  The ExistingFiles and
    ///             NonExistingFiles collections are populated.
    ///         3) If a collection of files is specified that contains a directory,
    ///             or a directory is specified then a recursive search is performed
    ///             to populate the ExistingFiles and NonExistingFiles collection.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class Probe {

        private ILog LOGGER = LogManager.GetLogger (typeof (Probe));
        private const String ALL = "*";
        private String start;
        private ArrayList files;

        private bool recursive = true;

        private bool isStartDirectory;

        private const String CVS_DIRECTORY = "CVS";

        /// <summary>
        /// The collection of files that have been identified below the given
        ///     starting path if Start is a directory, or if Start is a file then
        ///     this is the given file name.
        /// </summary>
        public ICollection Files {
            get {return this.files;}
        }


        /// <summary>
        /// Place to start the search.  By default the search will recurse through
        ///     the directory tree from here if this is a directory path.
        /// </summary>
        /// <exception name="ArgumentException">If the OriginalFiles collection
        ///     is non-null before an attempt is made to use this method.</exception>
        public String Start {
            set {
                String tempStart = value;
                if (Directory.Exists (tempStart)) {
                    this.isStartDirectory = true;
                } else if (File.Exists(tempStart)) {
                    this.isStartDirectory = false;
                } else {
                    StringBuilder msg = new StringBuilder ();
                    msg.Append ("Unknown starting position.");
                    msg.Append ("start=[").Append(tempStart).Append("]");
                    throw new ArgumentException(msg.ToString());
                }

                if (tempStart.EndsWith(CVS_DIRECTORY + Path.DirectorySeparatorChar)) {
                    this.start = tempStart.Remove(tempStart.Length - CVS_DIRECTORY.Length + 1, CVS_DIRECTORY.Length + 1);
                } else if (tempStart.EndsWith(CVS_DIRECTORY)) {
                    this.start = tempStart.Remove(tempStart.Length - CVS_DIRECTORY.Length, CVS_DIRECTORY.Length);
                } else {
                    this.start = tempStart;
                }
                if (LOGGER.IsDebugEnabled) {
                    StringBuilder msg = new StringBuilder();
                    msg.Append("start=[").Append(start).Append("]");
                    LOGGER.Debug (msg);
                }
            }

        }

        /// <summary>
        /// Initialize the existing and non-existing file collections.
        /// </summary>
        public Probe () {
            this.files = new ArrayList ();
        }

        /// <summary>
        /// Begin searching the list of files and categorizing them into existing
        ///     or non-existing.
        /// </summary>
        /// <exception>IllegalArgumentException if the or</exception>
        public void Execute () {
            if (this.isStartDirectory && this.recursive) {
                GetFiles(this.files, start, ALL);
            }
        }

        /// <summary>
        /// Perform a recursive search through the current directory specified.
        ///     Sort all files into existing or non-existing categories.
        /// </summary>
        private void GetFiles(ArrayList files, 
                                     String currentDirectory, 
                                     String searchPattern) {
            if (LOGGER.IsDebugEnabled) {
                StringBuilder msg = new StringBuilder ();
                msg.Append ("currentDirectory=[").Append(currentDirectory).Append("]");
                msg.Append ("currentDirectory.ToUpper().EndsWith (CVS_DIRECTORY.ToUpper())")
                    .Append(currentDirectory.ToUpper().EndsWith (CVS_DIRECTORY.ToUpper()))
                    .Append("]");
                LOGGER.Debug (msg);
            }
            if (!currentDirectory.ToUpper().EndsWith (CVS_DIRECTORY.ToUpper())) {
                String[] currentFiles = Directory.GetFiles (currentDirectory);
                foreach (String file in currentFiles) {
                    files.Add (file);
                }
                
                String[] currentDirectories = Directory.GetDirectories(currentDirectory);
                foreach (String directory in currentDirectories) {
                    GetFiles(files, directory, searchPattern);
                }
            }
        }
    }
}
