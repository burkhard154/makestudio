#region "Copyright"
// WorkingDirectory.cs
// Copyright (C) 2001 Mike Krueger
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
//    Author:     Mike Krueger,
//                Clayton Harbour
#endregion

using System;
using System.Collections;
using System.Text;
using System.IO;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Util;

namespace ICSharpCode.SharpCvsLib.Misc {
    /// <summary>
    /// Encapsulates the information for the local working directory and
    ///     the cvs server module name.  Also contains the <code>CvsRoot</code>
    ///     object required to connect to the server.  The only thing you
    ///     have to add for this is the password.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class WorkingDirectory {
        private Manager manager;
        private readonly ILog LOGGER =
            LogManager.GetLogger (typeof (WorkingDirectory));
        CvsRoot cvsroot;
        private DirectoryInfo localDir;
        String  repositoryname;
        String revision;
        bool hasDate = false;    // DateTime is a value type so we can't use null to indicate it hasn't been set
        DateTime date = new DateTime();
        String overrideDirectory;

        private Folders folders = new Folders();

        private Folder[] foldersToUpdate;

        /// <summary>
        ///     Render the object as a human readable string.
        /// </summary>
        public override String ToString () {
            ToStringFormatter formatter =
                new ToStringFormatter ("WorkingDirectory");
            formatter.AddProperty ("cvsRoot", cvsroot);
            formatter.AddProperty ("localdirectory", localDir.FullName);
            formatter.AddProperty ("repositoryname", repositoryname);
            formatter.AddProperty ("revision", revision);
            formatter.AddProperty ("overrideDirectory", overrideDirectory);
            formatter.AddProperty ("WorkingDirectoryName", WorkingDirectoryName);
            formatter.AddProperty ("WorkingPath", WorkingPath);

            return formatter.ToString ();

        }

        /// <summary>
        /// The root directory on the local/ client machine that sources
        ///     will be checked out into.  This will be contain the Module
        ///     or Override directory.
        /// </summary>
        public string LocalDirectory {
            get {return this.localDir.FullName;}
        }

        public DirectoryInfo LocalDir {
            get {return this.localDir;}
        }

        /// <summary>
        /// The name of the module.
        /// </summary>
        public string ModuleName {
            get {return repositoryname;}
            set {repositoryname = value;}
        }

        /// <summary>
        /// The name of the working directory under the local root directory.
        ///     This directory is usually equal to the module name however there
        ///     is an option to override this directory and specify an alternative.
        /// </summary>
        public string WorkingDirectoryName {
            get {
                if (this.HasOverrideDirectory) {
                return this.OverrideDirectory;
            } else {
                return this.repositoryname;
            }
        }
    }

    /// <summary>
    ///     Specifies the current working path for the sources.  This is
    ///         a combination of the root/ sandbox directory and the module
    ///         or override directory.
    /// </summary>
    /// <exception cref="InvalidPathException">If the local directory and/ or the 
    ///     working directory name are null.</exception>
    public String WorkingPath {
        get {
            if (null != this.LocalDirectory) {
                string tempWorkingPath;
                if (null == this.WorkingDirectoryName || this.WorkingDirectoryName.Length == 0) {
                    tempWorkingPath = this.LocalDirectory;
                } else {
                    tempWorkingPath = Path.Combine(this.LocalDirectory, this.WorkingDirectoryName);
                }
                if (!tempWorkingPath.EndsWith(Path.DirectorySeparatorChar.ToString())) {
                    tempWorkingPath = tempWorkingPath + Path.DirectorySeparatorChar;
                }
                return tempWorkingPath;
            } else {
                StringBuilder msg = new StringBuilder ();
                msg.Append ("Unable to determine working path, you must specify ");
                msg.Append ("a local directory and a module/ override directory.");
                msg.Append ("\nLocalDirectory=[").Append (this.LocalDirectory).Append ("]");
                msg.Append ("\nWorkingDirectoryName=[").Append (this.WorkingDirectoryName).Append ("]");
                throw new InvalidPathException (msg.ToString ());
            }
        }
    }


        /// <summary>
        /// A list of the cvs folders on the local host.
        /// </summary>
        public Folders Folders {
            get {return folders;}
            set {this.folders = value;}
        }

        /// <summary>
        ///     The cvs folders that are to be updated/ manipulated when requests
        ///         are sent to the server.
        /// </summary>
        public Folder[] FoldersToUpdate {
            get {return this.foldersToUpdate;}
            set {this.foldersToUpdate = value;}
        }

        /// <summary>
        /// Object encapsulating information to connect to a cvs server.
        /// </summary>
        public CvsRoot CvsRoot {
            get {return cvsroot;}
            set {cvsroot = value;}
        }

        /// <summary>Used to specify the revision of the module
        /// requested.  This should correspond to a module tag.</summary>
        [Obsolete(@"Use the UpdateCommand2.Revision object to specify the checkout/ update revision.")]
        public String Revision {
            get {return this.revision;}
            set {
                if (HasDate) {
                    throw new ArgumentException("Cannot specify both revision and date");
                }
                this.revision = value;
            }
        }

        /// <summary>
        ///     Determine if a revision has been specified.
        /// </summary>
        /// <returns><code>true</code> if a specific revision has been
        /// specified and the <code>Revision</code> field is non-null;
        /// <code>false</code> otherwise.</returns>
        public bool HasRevision {
            get {return !(null == this.Revision);}
        }

        /// <summary>Specifies the most recent revision no later that the given date.</summary>
        [Obsolete(@"Use the UpdateCommand2.Date to specify the checkout/ update date.")]
        public DateTime Date {
            get {return this.date;}
            set {
                if (HasRevision) {
                    throw new ArgumentException("Cannot specify both date and revision");
                }
                this.date = value; 
                hasDate = true;
            }
        }
        
        /// <summary>
        ///     Determine if a date has been specified.
        /// </summary>
        /// <returns><code>true</code> if a date has been
        /// specified and <code>false</code> otherwise.</returns>
        [Obsolete(@"Use the UpdateCommand2.HasDate to check if the date property has been
            set.")]
        public bool HasDate {
            get {return hasDate;}
        }
        
        /// <summary>
        ///     Returns the data as a string as required by the cvs server.
        /// </summary>
        [Obsolete(@"Use the UpdateCommand2.GetDateAsString.")]
        public string GetDateAsString() {
            string dateAsString = "";
	        string dateFormat = "dd MMM yyyy";

            if (hasDate) {
                dateAsString = date.ToString(dateFormat);
            }
            return dateAsString;
        }

        /// <summary>
        ///     Specifies the local root directory that the module will be checked
        ///         out to.  This overrides the module name that is being checked
        ///         out which is by default the local root directory.
        /// </summary>
        public String OverrideDirectory {
            get {return this.overrideDirectory;}
            set {this.overrideDirectory = value;}
        }

        /// <summary>
        ///     Determine if a local working directory overrides the repository
        ///         working directory (i.e. the module directory).
        /// </summary>
        /// <returns><code>true</code> if a local working directory has been
        ///     specified; <code>false</code> otherwise.</returns>
        public bool HasOverrideDirectory {
            get {return !(null == this.OverrideDirectory) && 
                     !(this.OverrideDirectory.Length == 0);}
        }

        /// <summary>
        /// Public constructor.
        /// </summary>
        /// <param name="cvsroot">The cvs root string, contains information
        ///     about the connection and path on the cvs server.</param>
        /// <param name="localdirectory">The local base directory to check the
        ///     module out in.</param>
        /// <param name="moduleName">The name of the module.  This is
        ///     appended to the base localdirectory to check the sources out into.</param>
        public WorkingDirectory(    CvsRoot cvsroot,
                                    string localdirectory,
                                    string moduleName) {
            this.repositoryname = moduleName;
            this.cvsroot        = cvsroot;
            this.localDir = new DirectoryInfo(localdirectory);
            this.manager = new Manager(this.WorkingPath);
        }

        /// <summary>
        /// Clear folders collection.
        /// </summary>
        public void Clear()
        {
            folders = new Folders();
        }

        /// <summary>
        /// Add a new entry to the folders collection.
        /// </summary>
        /// <param name="folder"></param>
        /// <param name="entry"></param>
        public void AddEntry(string folder, Entry entry)
        {
            if (folders[folder] == null) {
                folders[folder] = new Folder();
            }
            ((Folder)folders[folder]).Entries.Add(entry.FullPath, entry);
        }

        /// <summary>
        /// Converting the local directory string to a remote/ *nix
        ///     string.
        /// </summary>
        /// <param name="directory">The directory path.</param>
        /// <returns></returns>
        public string ToRemotePath(string directory) {
            return directory.Substring(
                    this.localDir.FullName.Length).Replace(Path.DirectorySeparatorChar, '/');
        }

        /// <summary>
        /// Convert the directory name to a win32 directory name
        ///     with the appropriate slashes.
        ///
        /// TODO: Clean up this dirty bad boy and move the functionality
        ///     to the Manager
        /// </summary>
        /// <param name="orgPath">The directory path.</param>
        /// <returns></returns>
        [Obsolete ("Use the OrgPath to parse the org path string")]
        public string ToLocalPath(string orgPath) {
            string _localBasePath = this.localDir.FullName;

            string _orgPathWithoutRoot =
                orgPath.Substring (this.cvsroot.CvsRepository.Length + 1);

            String [] splitOrgPath = orgPath.Split ('/');
            String filename = splitOrgPath[splitOrgPath.Length - 1];
            string _orgPathWithoutFilename =
                _orgPathWithoutRoot.Replace (filename, "");
            string _localPath =
                Path.Combine (_localBasePath, _orgPathWithoutFilename);

            _localPath =
                _localPath.Replace ('/', Path.DirectorySeparatorChar);
            if (LOGGER.IsDebugEnabled) {
                String msg = "Converting server path and filename to local path and filename.  " +
                            "_localBasePath=[" + _localBasePath + "]" +
                            "_orgPathWithoutRoot=[" + _orgPathWithoutRoot + "]" +
                            "_orgPathWithoutFilename=[" + _orgPathWithoutFilename + "]" +
                            "localPath=[" + _localPath + "]";
                LOGGER.Debug (msg);
            }

            return _localPath;
        }

        /// <summary>
        /// Uses the assumption that ASCII 0 or ASCII 255 are only
        ///     found in non-text files to determine if the file
        ///     is a binary or a text file.
        /// Also the assumption is made that a non-text file will
        ///     have an ASCII 0 or ASCII 255 character.
        /// </summary>
        //        [Obsolete ("This is moving to the CvsFileManager class")]
        private bool IsBinary(string filename) {
            FileStream fs = File.OpenRead(filename);

            byte[] content = new byte[fs.Length];

            fs.Read(content, 0, (int)fs.Length);
            fs.Close();

            // assume that ascii 0 or
            // ascii 255 are only found in non text files.
            // and that all non text files contain 0 and 255
            foreach (byte b in content) {
                if (b == 0 || b == 255)
                    return true;
            }

            return false;
        }

        /// <summary>
        /// Recurses through all child directories starting with
        ///     base directory.  Add all the cvs entries found
        ///     to the folder collection.
        /// </summary>
        /// <param name="directory">The name of the directory.</param>
        //        [Obsolete ("This is moving to the CvsFileManager class")]
        public void AddEntriesIn(string directory) {
            if (LOGGER.IsDebugEnabled) {
                String msg = "Adding cvs entries to request updates.  " +
                            "directory=[" + directory + "]";
                LOGGER.Debug (msg);
            }
            ArrayList entryCollection =
                new ArrayList (this.manager.Fetch (directory, Factory.FileType.Entries));
            Entry[] entries = (Entry[])entryCollection.ToArray (typeof (Entry));
            // TODO: Remove this line -- Entry.RetrieveEntries(directory);
            if (entries != null && entries.Length > 0) {
                string cvsdir    = ToRemotePath(directory);
                if (File.Exists(Path.Combine (directory, Repository.FILE_NAME))) {
                    StreamReader sr =
                        File.OpenText(Path.Combine (directory, Repository.FILE_NAME));
                    string line = sr.ReadLine();
                    if (line != null && line.Length > 0) {
                        // TODO: Figure out what to do with this path seperator
                        cvsdir = "/" + line; // + "/" + cvsdir;
                    }
                    sr.Close();
                }
                foreach (Entry entry in entries) {
                    if (entry.IsDirectory && null != entry.Name) {
                        AddEntriesIn(Path.Combine (directory, entry.Name));
                    }

                    if (LOGGER.IsDebugEnabled) {
                        String msg = "Adding entry.  " +
                                    ";  cvsdir=[" + cvsdir + "]" +
                                    ";  entry=[" + entry + "]";
                        LOGGER.Debug (msg);
                    }
                    AddEntry(cvsdir, entry);
                }
            }
        }

        /// <summary>
        /// Read all the existing entries.
        /// </summary>
        //        [Obsolete ("This is moving to the CvsFileManager class")]
        public void ReadAllExistingEntries() {
            Clear();
            string wd =
                Path.Combine (this.localDir.FullName, this.ModuleName);
            AddEntriesIn(wd);
        }

        /// <summary>
        /// Determines if the path specified is inside of the working directory path.
        /// </summary>
        /// <param name="path">A path to check.</param>
        /// <returns>Returns <code>true</code> if the path to check is under the 
        ///     current working path; otherwise <code>false</code>.</returns>
        public bool IsInPath (string path) {
            return path.IndexOf(this.WorkingPath) >= 0;
        }

    }
}
