#region Copyright
// Entry.cs
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
using System.IO;
using System.Globalization;
using System.Text;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Util;

namespace ICSharpCode.SharpCvsLib.FileSystem {
    /// <summary>
    ///     Used to parse out the important parts of the orgainization path
    ///         response from the cvs server.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class PathTranslator {
        /// <summary>
        /// The name of the cvs management folder.
        /// </summary>
        public const String CVS = "CVS";

        private static readonly ILog LOGGER =
            LogManager.GetLogger (typeof (PathTranslator));

        private WorkingDirectory workingDirectory;
        private String repositoryPath;

        private DirectoryInfo baseDir;
        private string moduleFacade;

        private DirectoryInfo _currentDir;

        private CvsRoot cvsRoot;
        private String relativePath;
        private bool isDirectory;

        /// <summary>
        ///     The cvs root that defines the who, how and where of the server.
        /// </summary>
        public CvsRoot CvsRoot {
            get {return this.cvsRoot;}
        }

        /// <summary>
        /// The relative path to the file.  Usually this is the information that
        ///    has been sent down by the server and can be used along with
        ///    the current working directory information to find the file on the
        ///    local file system.
        /// </summary>
        public String RelativePath {
            get {return this.relativePath;}
        }

        /// <summary>
        /// The name of the file, without any path information.
        /// </summary>
        public String Filename {
            get {return this._currentDir.Name;}
        }

        /// <summary>
        ///  The path on the local file system.  This is constructed using the
        ///     local working directory and the relative path from the server.
        ///     This does not contain information about the filename.
        /// </summary>
        public String LocalPath {
            get {return baseDir.FullName;}
        }

        public DirectoryInfo BaseDir {
            get {return baseDir;}
        }

        /// <summary>
        /// Local path and filename.  Combines the local path and the name
        ///     of the file.
        /// </summary>
        [Obsolete ("Use CurrentDir")]
        public String LocalPathAndFilename {
            get {return this._currentDir.FullName;}
        }

        public DirectoryInfo CurrentDir {
            get {
                if (null == this._currentDir) {
                    this._currentDir = new DirectoryInfo(Environment.CurrentDirectory);
                }
                return this._currentDir;
            }
            set {this._currentDir = value;}
        }

        /// <summary>
        ///     Determines if path contains a reference to a file or a directory.
        /// </summary>
        public bool IsDirectory {
            get {return this.isDirectory;}
        }

        /// <summary>
        /// Uses the directory seperator character to determine if the platform is Windows.
        /// </summary>
        /// <value><code>true</code> if the directory seperator character is a '\'; otherwise
        ///     <code>false</code>.</value>
        public static bool IsWin32 {
            get {
                return Path.DirectorySeparatorChar.Equals('\\');
            }
        }

        /// <summary>
        /// Indicate if the platform filesystem is case sensitive or not.
        /// </summary>
        /// <value><code>true</code> if the OS is case sensitive; otherwise <code>false</code>.</value>
        public static bool IsCaseSensitive {
            get {
                return !IsWin32;
            }
        }

        /// <summary>
        ///     Constructor, parses out the orgainizational path response from
        ///         the cvs server.
        /// </summary>
        /// <param name="workingDirectory">Contains information about the working
        ///     repository.</param>
        /// <param name="repositoryPath">The relative path to the file served
        ///     down from the cvs server.</param>
        public PathTranslator (WorkingDirectory workingDirectory,
            String repositoryPath) {
            this.baseDir = new DirectoryInfo(workingDirectory.LocalDirectory);
            this.moduleFacade = workingDirectory.WorkingDirectoryName;

            this.repositoryPath = repositoryPath;
            this.workingDirectory = workingDirectory;
            this.cvsRoot = workingDirectory.CvsRoot;
            this.relativePath =
                this.GetRelativePath (repositoryPath);

            this._currentDir = 
                new DirectoryInfo(Path.Combine(Path.Combine(baseDir.FullName,
                this.workingDirectory.WorkingDirectoryName), this.relativePath));        
        }

        /// <summary>
        ///     Remove the repository name from the beginning of the path so that
        ///         the local path to the file can contain an override directory.
        ///         
        ///     The module name is appended in a different section.
        /// </summary>
        /// <param name="repositoryPath">The relative path to the repository directory
        ///     which is returned in the server response.</param>
        /// <returns>The value of the filename.</returns>
        private String GetRelativePath (String repositoryPath) {
            if (repositoryPath.EndsWith("/")) {
                this.isDirectory = true;
            }

            if (repositoryPath.StartsWith(this.cvsRoot.CvsRepository)) {
                repositoryPath = repositoryPath.Substring(this.cvsRoot.CvsRepository.Length + 1);
            }

            if (repositoryPath.StartsWith("/")) {
                repositoryPath = repositoryPath.Substring(1);
            }

            if (repositoryPath.StartsWith(this.moduleFacade)) {
                repositoryPath = repositoryPath.Substring(this.moduleFacade.Length + 1);
            } else if (repositoryPath.StartsWith(this.workingDirectory.ModuleName)) {
                repositoryPath = repositoryPath.Substring(this.workingDirectory.ModuleName.Length + 1);
            }

            return repositoryPath;
        }

        /// <summary>
        ///     Convert this object to a human readable string.
        /// </summary>
        /// <returns>A string representation of the object.</returns>
        public override String ToString () {
            ToStringFormatter formatter = new ToStringFormatter ("PathTranslator");
            formatter.AddProperty ("cvsRoot", cvsRoot);
            formatter.AddProperty ("repositoryPath=[", this.repositoryPath);
            formatter.AddProperty ("relativePath", relativePath);

            return formatter.ToString ();
        }

        /// <summary>
        ///     Convert the back slashes and front slashes in the path string to
        ///         the direction preferred by the os.
        /// </summary>
        /// <param name="path">The path to convert.</param>
        /// <returns>The os friendly path string.</returns>
        public static String ConvertToOSSpecificPath (String path) {
            // Make the path match the os' preference
            String osPath = path;
            osPath = osPath.Replace ("/", Path.DirectorySeparatorChar.ToString());
            osPath = osPath.Replace ("\\", Path.DirectorySeparatorChar.ToString());

            return osPath;
        }

        /// <summary>
        /// Determines if a valid path was entered.  A valid path will not contain
        ///     any cvs folder information.  If the full path contains information 
        ///     about a cvs folder then false is returned.
        /// </summary>
        /// <param name="fullPath">The full path to validate.</param>
        /// <returns>Returns <code>true</code> if the path does NOT contain a CVS
        ///     folder; otherwise returns <code>false</code></returns>
        private static bool ContainsCVS(String fullPath) {
            if (fullPath.ToUpper().EndsWith(Path.DirectorySeparatorChar + CVS.ToUpper())) {
                return true;
            } else {
                LOGGER.Debug("ends with: " + fullPath.ToUpper());
            }
            foreach (string fileType in System.Enum.GetNames(typeof(Factory.FileType))) {
                string cvsDir = string.Format("{0}{1}{2}{3}",
                    Path.DirectorySeparatorChar, CVS, Path.DirectorySeparatorChar, fileType);
                if (fullPath.ToUpper().EndsWith(cvsDir.ToUpper())) {
                    return true;
                }
            }
            return false;
        }

        public static bool IsCvsDir (DirectoryInfo dir) {
            return IsCvsDir(dir.FullName);
        }

        /// <summary>
        /// Determine if the full path specified is a cvs control directory or if it is just a 
        /// regular path on the hard drive.
        /// </summary>
        /// <param name="fullPath">Full path to check.</param>
        /// <returns><code>true</code> if the directory is a cvs control directory; 
        ///     otherwise <code>false</code>.</returns>
        public static bool IsCvsDir (string fullPath) {
            if (ContainsCVS(fullPath)) { // && HasControlFiles(fullPath)) {
                return true;
            } else {
                return false;
            }
        }

        private static bool HasControlFiles (string fullPath) {
            if (null == fullPath) {
                return false;
            }
            DirectoryInfo dirInfo = new DirectoryInfo(fullPath);
            if (!dirInfo.Exists) {
                return false;
            }
            foreach (FileInfo file in dirInfo.GetFiles()) {
                if ((ContainsCVS(dirInfo.Name) && Entry.FILE_NAME.IndexOf(file.Name) > -1) ||
                    (ContainsCVS(dirInfo.Name) && Root.FILE_NAME.IndexOf(file.Name) > -1) ||
                    (ContainsCVS(dirInfo.Name) && Repository.FILE_NAME.IndexOf(file.Name) > -1)) {
                    return true;
                }
            }
            return false;
        }

        private static bool IsSeperatorBeforeAndAfterCvs (String fullPath) {
            // Check for case where file is called CVSFoo
            try {
                int startOfCvs = fullPath.ToUpper().LastIndexOf(CVS);
                String cvsString = 
                    fullPath.ToUpper().Substring (startOfCvs - 1, CVS.Length + 2);
                
                if (cvsString.Equals(
                    Path.DirectorySeparatorChar + CVS + Path.DirectorySeparatorChar)) {
                    return true;
                }
            } catch (ArgumentOutOfRangeException) {
                return true;
            }

            return false;
        }

        /// <summary>
        /// Checks the end of a path string, if the path ends in CVS it does nothing, 
        /// otherwise it appends CVS to the full path.
        /// </summary>
        /// <param name="fs">Full path to a file/ directory that may or may
        ///     not end with CVS.</param>
        /// <returns>The full path with a CVS appended.</returns>
        public static FileSystemInfo AppendCvs(FileSystemInfo fs) {
            if (fs.FullName.EndsWith(string.Format("{0}{1}{2}", Path.DirectorySeparatorChar, CVS, Path.DirectorySeparatorChar)) ||
                fs.FullName.EndsWith(string.Format("{0}{1}", Path.DirectorySeparatorChar, CVS)) ||
                PathTranslator.IsCvsDir(fs.FullName)) {
                LOGGER.Warn(string.Format("Already ends with cvs directory: {0}",
                    fs.FullName));
                return fs;
            } 

            if (fs.GetType() == typeof(DirectoryInfo)) {
                return new DirectoryInfo(Path.Combine(fs.FullName, CVS));
            } else {
                return new FileInfo(Path.Combine(fs.FullName, CVS));
            }
        }

        public static FileSystemInfo AppendCvs(string fs) {
            FileSystemInfo _fs;
            if (File.Exists(fs) || 
                IsControlFile(fs)) {
                _fs = new FileInfo(fs);
            } else {
                _fs = new DirectoryInfo(fs);
            } 

            return AppendCvs(_fs);
        }

        /// <summary>
        /// Append the cvs directory to the file full path, then append the cvs management
        /// file to the full path.
        /// </summary>
        /// <param name="fullPath">Full path that may or may not contain a CVS 
        ///     directory.</param>
        /// <param name="managingFile">Name of the managing file, such as 
        ///     <see cref="Tag"/>, <see cref="Entry"/>.</param>
        /// <returns></returns>
        public static string AppendCvs(string fullPath, string managingFile) {
            return Path.Combine(AppendCvs(fullPath).FullName, managingFile);
        }

        private static bool IsControlFile (string fs) {
            FileInfo fileInfo = new FileInfo(fs);
            
            foreach (string file in System.Enum.GetNames(typeof(Factory.FileType))) {
                if (fileInfo.Name == file) {
                    return true;
                }
            }
            return false;
        }

    }
}
