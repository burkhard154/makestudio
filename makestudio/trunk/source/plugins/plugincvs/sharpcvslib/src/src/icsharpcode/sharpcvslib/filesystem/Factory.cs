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
#endregion

using System;
using System.Collections;
using System.Text;
using System.IO;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.FileSystem {

    /// <summary>
    ///     Creates the cvs object necessary based on the filename.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class Factory {
        private readonly ILog LOGGER = LogManager.GetLogger(typeof(ICSharpCode.SharpCvsLib.FileSystem.Factory));
        /// <summary>
        ///     Type of cvs file.
        /// </summary>
        public enum FileType {
            /// <summary>
            ///     Root file type.
            /// </summary>
            Root,
            /// <summary>
            ///     Repository file type.
            /// </summary>
            Repository,
            /// <summary>
            ///     The entries file type.
            /// </summary>
            Entries,
            /// <summary>Used to specify specific repository revisions or
            /// sticky tags.</summary>
            Tag
        }
        /// <summary>
        ///     Constructor.
        /// </summary>
        public Factory () {

        }

        /// <summary>
        /// Create a cvs management object for the given path.  The path specified
        ///     should be the folder above the cvs directory.  The name of the file
        ///     and full path is then derived from the cvs line in the case of an
        ///     Entries line, or in the case of a single line cvs management file
        ///     (i.e. Root, Repository, etc.) the object being managed is the
        ///     entire directory.
        /// </summary>
        /// <param name="path">The path to the folder above the cvs directory.</param>
        /// <param name="fileName">The name of the cvs file that is being modified/
        ///     created.</param>
        /// <param name="line">The line to add to the file.</param>
        /// <returns>A new cvs file that contains properties for the different
        ///     elements in the line.</returns>
        /// <exception cref="UnsupportedFileTypeException">If the cvs filetype specified
        ///     is unknown.</exception>
        /// <example>
        ///     The following will produce an entries file 
        ///         (directory seperator character may be different):
        ///         
        /// <list type="table">
        ///     <term>path</term>
        ///     <description>c:/dev/sharpcvslib</description>
        ///     <term>fileName</term>
        ///     <description>Entries</description>
        ///     <term>line</term>
        ///     <description>/SharpCvsLib.build/1.1///</description>
        /// </list>
        /// 
        /// With the following information:
        /// <list type="table">
        ///     <term>FileContents</term>
        ///     <description>/SharpCvsLib.build/1.1///</description>
        ///     <term>FileName</term>
        ///     <description>Entries</description>
        ///     <term>FullPath</term>
        ///     <description>c:/dev/sharpcvslib/SharpCvsLib.build</description>
        ///     <term>IsMultiLined</term>
        ///     <description>true</description>
        ///     <term>Path</term>
        ///     <description>c:/dev/sharpcvslib/</description>
        /// </list>
        ///         
        ///     NOTE:
        ///     <ul>
        ///         <li>The path seperator may face the other way</li>
        ///         <li>There will be an ending path seperator after every directory,
        ///             as in the path.</li>
        ///     </ul>
        /// </example>
        public ICvsFile CreateCvsObject (String path, String fileName, String line) {
            FileType fileType = this.GetFileType(fileName);
            FileInfo cvsFile = new FileInfo(Path.Combine(PathTranslator.AppendCvs(path).FullName, fileName));
            return this.CreateCvsObject(cvsFile, line);
        }

        public ICvsFile CreateCvsObject (DirectoryInfo dir, string fileName, string line) {
            if (!dir.FullName.EndsWith(string.Format("{0}CVS", 
                Path.DirectorySeparatorChar))) {
                dir = new DirectoryInfo(Path.Combine(dir.FullName, "CVS"));
            }
            FileInfo cvsFile = new FileInfo(Path.Combine(dir.FullName, fileName));
            return this.CreateCvsObject(cvsFile, line);
        }

        /// <summary>
        /// Create a cvs file object given the full path and line of the file.
        /// </summary>
        /// <param name="file"></param>
        /// <param name="line"></param>
        /// <returns></returns>
        public ICvsFile CreateCvsObject (FileInfo file, string line) {
            ICvsFile entry;

            if (!System.Enum.IsDefined(typeof(FileType), file.Name)) {
                throw new UnsupportedFileTypeException(string.Format("Unknown cvs file type: {0}",
                    file.Name));
            }

            switch ((FileType)Enum.Parse(typeof(FileType), file.Name, true)) {
                case (FileType.Entries): {
                    entry = new Entry(file, line);
                    break;
                }
                case (FileType.Repository):{
                    entry = new Repository (file, line);
                    break;
                }
                case (FileType.Root):{
                    entry = new Root (file, line);
                    break;
                }
                case (FileType.Tag):{
                    entry = new Tag (file, line);
                    break;
                }
                default:{
                    StringBuilder msg = new StringBuilder();
                    msg.Append("Unknown file type specified.");
                    msg.Append("fileType=[").Append(file.Name).Append("]");
                    throw new UnsupportedFileTypeException (msg.ToString());
                }

            }
            return entry;
        }

        private string GetCvsFileName (string fullPath) {
            int cvsIndex = fullPath.LastIndexOf("CVS");
            if (cvsIndex == -1) {
                throw new Exception(string.Format("CVS directory does not exist in {0}.",
                    fullPath));
            }

            return
                fullPath.Substring(cvsIndex + 3);
        }

        private string GetPath(string fullPath) {
            int cvsIndex = fullPath.LastIndexOf("CVS");
            if (cvsIndex == -1) {
                throw new Exception(string.Format("CVS directory does not exist in {0}.",
                    fullPath));
            }

            return
                fullPath.Substring(0, cvsIndex);
        }

        /// <summary>
        ///     Get the name of the file given the file type.
        /// </summary>
        /// <param name="fileType">The type of the file.</param>
        /// <returns>The name of the cvs file.</returns>
        public String GetFilename (FileType fileType) {
            switch (fileType) {
                case (FileType.Entries):{
                    return Entry.FILE_NAME;
                }
                case (FileType.Repository):{
                    return Repository.FILE_NAME;
                }
                case (FileType.Root):{
                    return Root.FILE_NAME;
                }
                case (FileType.Tag):{
                    return Tag.FILE_NAME;
                }
                default:{
                    StringBuilder msg = new StringBuilder();
                    msg.Append("Unknown file type specified.");
                    msg.Append("fileType=[").Append(fileType.ToString()).Append("]");
                    throw new UnsupportedFileTypeException (msg.ToString());
                }
            }
        }

        /// <summary>
        /// Derive the file type from the name of the cvs file.
        /// </summary>
        /// <param name="name">The name of the cvs file.</param>
        /// <returns>The type of the file.</returns>
        public FileType GetFileType (String name) {
            switch (name) {
                case (Entry.FILE_NAME): {
                    return FileType.Entries;
                }
                case (Repository.FILE_NAME): {
                    return FileType.Repository;
                }
                case (Root.FILE_NAME): {
                    return FileType.Root;
                }
                case (Tag.FILE_NAME): {
                    return FileType.Tag;
                }
                default: {
                    StringBuilder msg = new StringBuilder();
                    msg.Append("Unknown file type specified.");
                    msg.Append("name=[").Append(name).Append("]");
                    throw new UnsupportedFileTypeException (msg.ToString());
                }
            }
        }
    }
}
