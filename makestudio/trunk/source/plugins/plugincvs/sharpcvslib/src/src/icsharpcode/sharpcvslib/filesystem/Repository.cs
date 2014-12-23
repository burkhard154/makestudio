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
    /// Information about the repository file.  This file is used to identify
    ///     the relative path (from the cvsroot) of the file in the cvs
    ///     repository.  Combined with the entry from the cvs entries file
    ///     this provides the relative path to the file on the cvs server.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class Repository : AbstractCvsFile, ICvsFile {
        private ILog LOGGER = LogManager.GetLogger (typeof (Repository));
        private String moduleName;

        /// <summary>
        ///     The name of the repository file.
        /// </summary>
        public const String FILE_NAME = "Repository";

        /// <summary>
        ///     Create a new repository object taking the path to the
        ///         folder above the CVS directory and the line to enter
        ///         into the repository file.
        ///
        ///     The repository file stores the relative path to the directory
        ///         from the server's perspective.
        /// </summary>
        /// <param name="cvsFile">The cvs management file.</param>
        /// <param name="line">The line to enter into the repository file.</param>
        public Repository (FileInfo cvsFile, String line) : base (cvsFile, line) {
        }

        /// <summary>
        ///     Create a new repository object taking the path to the
        ///         folder above the CVS directory and the line to enter
        ///         into the repository file.
        ///
        ///     The repository file stores the relative path to the directory
        ///         from the server's perspective.
        /// </summary>
        /// <param name="filePath">The path to the cvs management file.</param>
        /// <param name="line">The line to enter into the repository file.</param>
        [Obsolete ("Use constructor Repository(FileInfo string)")]
        public Repository (string filePath, string line) : 
            this (new FileInfo(System.IO.Path.Combine(filePath, "CVS")), line) {

        }

        /// <summary>
        ///     The name of this file should correspond to the name required
        ///         for a cvs repository.
        /// </summary>
        public override String Filename {
            get {return Repository.FILE_NAME;}
        }

        /// <summary>
        /// Return the module name from the repository object.  The cvs module name
        ///     is stored as the topmost directory in the Repository relative path.
        /// </summary>
        /// <example>
        ///     <p>In the repository path represented by:
        ///         <code>sharpcvslib/src/ICSharpCode</code>
        ///     The module name would be:
        ///         <code>sharpcvslib</code>
        ///     </p>
        /// </example>
        public String ModuleName {
            get {return this.moduleName;}
        }

        public static Repository Load (DirectoryInfo cvsDir) {
            if (cvsDir.Name != "CVS") {
                cvsDir = new DirectoryInfo(
                    System.IO.Path.Combine(cvsDir.FullName, "CVS"));
            }

            return Load(new FileInfo(System.IO.Path.Combine(
                cvsDir.FullName, Repository.FILE_NAME)));
        }

        public static Repository Load (FileInfo repositoryFile) {
            if (repositoryFile.Name != Repository.FILE_NAME) {
                throw new ArgumentException(string.Format("Not a Repository file.", repositoryFile));
            }

            if (!repositoryFile.Exists) {
                throw new CvsFileNotFoundException(
                    string.Format("File does not exist {0}.", repositoryFile.FullName));
            }
            string fileContents;

            using (StreamReader reader = new StreamReader(repositoryFile.FullName)) {
                fileContents = reader.ReadToEnd();
            }

            // strip off line return characters
            fileContents = fileContents.Replace(Environment.NewLine, "");

            return new Repository(repositoryFile, fileContents);
            
        }

        /// <summary>
        /// Format the string as a repository entry.  Remove any trailing
        ///     slashes from the line.
        /// </summary>
        /// <param name="line">The one line intry in the Repository file.</param>
        /// <example>
        ///     A CVS\Repository file that looks like:
        ///         <code>sharpcvslib/src/ICSharpCode</code>
        ///     would be parsed into the following components:
        ///     <ul>
        ///         <li>
        ///             moduleName: sharpcvslib
        ///         </li>
        ///         <li>fileContents: sharpcvslib/src/ICSharpCode</li>
        ///     </ul>
        /// </example>
        public override void Parse (String line) {
            int moduleLength = line.Length;
            if (line.IndexOf("/") > -1) {
                moduleLength = line.IndexOf("/");
            }
            this.moduleName = line.Substring(0, moduleLength);
            this.FileContents = line;
        }

        /// <summary>
        ///     Determines if the objects are equal based on the file contents
        ///
        /// </summary>
        /// <param name="obj">The object to compare.</param>
        public override bool Equals (object obj) {
            if (obj is Repository) {
                Repository that = (Repository)obj;
                if (that.GetHashCode ().Equals (this.GetHashCode ())) {
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        ///     Another object will be unique if id identifies the same
        ///         file contents as this file.
        /// </summary>
        public override int GetHashCode () {
            return this.FileContents.GetHashCode ();
        }

        /// <summary>The type of file that this is.</summary>
        public Factory.FileType Type {get {return Factory.FileType.Repository;}}

        /// <summary>Indicates whether the file can contain multiple
        /// lines.</summary>
        /// <returns><code>true</code> if the file can contain multiple
        /// lines; <code>false</code> otherwise.</returns>
        public bool IsMultiLined {
            get {return false;}
        }

        /// <summary>
        /// Returns the full path to the CVS\Root folder on the local filesystem
        ///     that this object represents.
        /// </summary>
        /// <returns>The full path to the CVS\Root file that this object
        ///     represents.</returns>
        protected override String DeriveCvsFullPath () {
            throw new NotImplementedException("This will eventually return the full path to the repository.");
        }

    }
}
