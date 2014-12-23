#region "Copyright"
//
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

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.FileSystem {
	/// <summary>
	/// Populates a folder or collection of folders given one file, a directory,
	///     a collection of files or a collection of directories.
	/// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
	public class Folders : DictionaryBase {
        private ILog LOGGER = LogManager.GetLogger (typeof (Folders));

        /// <summary>
        /// Return the collection of values for the dictionary.
        /// </summary>
        public ICollection Values {
            get {return this.Dictionary.Values;}
        }

        /// <summary>
        /// Create an instance of the folder manager.
        /// </summary>
		public Folders() {
		}

        /// <summary>
        /// The path to the folder on the filesystem.
        /// </summary>
        public Folder this[String path] {
            get { return ((Folder)(Dictionary[path])); }
            set { Dictionary[path] = value; }
        }

        /// <summary>
        /// Add the folder to the folders collection.
        /// <br/>
        /// </summary>
        /// <param name="folder">The folder to add to the collection.</param>
        /// <exception cref="ArgumentException">If the Folder.Path value is null.</exception>
        public void Add(Folder folder) {
            if (null == folder.Path) {
                throw new ArgumentException("Folder.Path cannot be null.");
            }   
            Dictionary.Add(folder.Path.FullName, folder);
        }   

        /// <summary>
        /// Add the given folder to the collection.  The folder key is the path to
        ///     the folder on the filesystem.
        /// </summary>
        /// <param name="path">The path to the folder on the filesystem.</param>
        /// <param name="folder">The folder object to add to the collection.</param>
        public void Add(String path, Folder folder) {
            if (null == folder.Path) {
                folder.Path = new DirectoryInfo(path);
            }
            Dictionary.Add(path, folder);
        }

        /// <summary>
        /// Remove the given folder from the collection.
        /// </summary>
        /// <param name="path">The path from the folder to remove from the 
        ///     folders collection.</param>
        public void Remove(String path) {
            Dictionary.Remove(path);
        }

        /// <summary>
        /// Determine if the folders collection contains the given folder.
        /// </summary>
        /// <param name="path">The path to the folder on the filesystem which 
        ///     acts as the key to the collection.</param>
        /// <returns><code>true</code> if the collection contains the folder;
        ///     otherwise <code>false</code>.</returns>
        public bool Contains(String path) {
            return Dictionary.Contains(path);
        }

        /// <summary>
        /// Recursively fill the folder object from the base directory.
        /// </summary>
        /// <param name="dir">Directory to start recursing through.</param>
        public void Fill(DirectoryInfo dir) {
            if (Folder.IsManaged(dir)) {
                this.Add(new Folder(dir));
            }
            foreach (DirectoryInfo subDir in dir.GetDirectories()) {
                this.Fill(subDir);
            }
        }

        /// <summary>
        /// Return a human readable string that represents the folders contained
        ///     in this dictionary object.
        /// </summary>
        /// <returns>A human readable string that represents this collection
        ///     of folders.</returns>
        public override String ToString () {
            ICSharpCode.SharpCvsLib.Util.ToStringFormatter formatter = new
                ICSharpCode.SharpCvsLib.Util.ToStringFormatter("Entries");
            foreach (DictionaryEntry entry in Dictionary) {
                formatter.AddProperty("Folder key", entry.Key);
                formatter.AddProperty("Folder value", entry.Value);
            }
            return formatter.ToString();
        }

	}
}
