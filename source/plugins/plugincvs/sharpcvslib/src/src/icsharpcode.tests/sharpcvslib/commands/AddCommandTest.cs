#region "Copyright"
// AddCommandTest.cs
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
// As a special exception, if you link this library with other files to
// produce an executable, this library does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why the
// executable file might be covered by the GNU General Public License.
//
//      <author>Clayton Harbour</author>
#endregion

using System;
using System.Collections;
using System.IO;

using ICSharpCode.SharpCvsLib;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Exceptions;
using ICSharpCode.SharpCvsLib.Tests;

using NUnit.Framework;

using log4net;

namespace ICSharpCode.SharpCvsLib.Commands {
	/// <summary>
	/// Summary description for AddCommandTest.
	/// </summary>
	[TestFixture]
	public class AddCommandTest : AbstractTest {
        private readonly ILog LOGGER  = LogManager.GetLogger(typeof(AddCommandTest));

        /// <summary>
        /// Add a file to the test cvs repository.
        /// </summary>
        [Test]
        public void AddFilesTest () {

            this.CheckoutTestModule();

            String modulePath = Path.Combine(this.GetTempPath(), 
                this.Settings.Config.Module);

            String[] files = Directory.GetFiles(modulePath, "*.txt");

            Assert.IsTrue(files.Length > 0);
            ArrayList copiedFiles = new ArrayList ();
            foreach (String file in files) {
                LOGGER.Debug("file=[" + file + "]");
                // Remove the .txt when everything works, giving me bugs...
                String newFileName = Guid.NewGuid().ToString() + ".txt";
                String fullPath = Path.Combine(modulePath, newFileName);
                File.Copy (file, fullPath);
                copiedFiles.Add(fullPath);
            }

            CvsRoot root = new CvsRoot (this.Settings.Config.Cvsroot);
            WorkingDirectory working =
                new WorkingDirectory (root,
                this.Settings.Config.LocalPath,
                this.Settings.Config.Module);

            CVSServerConnection connection = new CVSServerConnection ();
            Assert.IsNotNull (connection);

            AddCommand command = new AddCommand (working);
            //command.Folders = this.GetFolders(modulePath);
            command.Folders = this.GetFoldersToAdd(copiedFiles);

            Assert.IsTrue(command.Folders.Count > 0);
            LOGGER.Debug("folders count=[" + command.Folders.Count + "]");
            foreach (DictionaryEntry folderDic in command.Folders) {
                Folder folder = (Folder)folderDic.Value;
                LOGGER.Debug("folder=[" + folder.ToString() + "]");
                LOGGER.Debug("entries count=[" + folder.Entries.Count + "]");
            }
            Assert.IsNotNull (command);

            try {
                connection.Connect (working, this.Settings.Config.ValidPassword);
            } catch (AuthenticationException) {
                // should not get here.
                Assert.IsTrue (true);
            }

            command.Execute (connection);
            connection.Close();

            try {
                connection.Connect(working, this.Settings.Config.ValidPassword);
            } catch (AuthenticationException) {
                Assert.IsTrue(true);
            }
            CommitCommand2 commitCommand = new CommitCommand2(working);
            working.Folders = command.Folders;
            commitCommand.LogMessage = "AddCommandTest";
            commitCommand.Execute(connection);
            connection.Close ();

            Manager manager = new Manager(working.WorkingPath);
            Entries entries = manager.FetchEntries(Path.Combine(modulePath, "Entries"));
            foreach (String addedFile in copiedFiles) {
                Assert.IsTrue(entries.Contains(Path.Combine(modulePath, addedFile)));
            }

        }

        private Folders GetFoldersToAdd (ICollection filesAdded) {
            Folders folders = new Folders();
            Manager manager = new Manager(Path.Combine(this.Settings.LocalPath, this.Settings.Module));
            LOGGER.Debug("Number of files copied=[" + filesAdded.Count + "]");
            foreach (String file in filesAdded) {
                Folder folder;
                if (!folders.Contains(Path.GetDirectoryName(file))) {
                    folder = new Folder();
                    LOGGER.Debug("file=[" + file + "]");
                    LOGGER.Debug("file path=[" + Path.GetDirectoryName(file) + "]");
                    folder.Repository = 
                        manager.FetchRepository(Path.GetDirectoryName(file));
                    folder.Root = 
                        manager.FetchRoot(Path.GetDirectoryName(file));
                    folder.Tag = 
                        manager.FetchTag(Path.GetDirectoryName(file));
                    folders.Add(Path.GetDirectoryName(file), folder);
                } else {
                    folder = folders[Path.GetDirectoryName(file)];
                }
                if (!folder.Entries.Contains(file)) {
                    Entry entry = Entry.CreateEntry(file);
                    folder.Entries.Add (file, entry);
                } else {
                    folder.Entries[file] = Entry.CreateEntry(file);
                }
            }
            return folders;
        }

        private Folders GetFolders(String path) {
            String startPath = path;
            String[] files =
                Directory.GetFiles(startPath); 

            Manager manager = 
                new Manager(Path.Combine(this.Settings.LocalPath, this.Settings.Module));
            Folders folders = manager.GetFolders(files);
            foreach (String file in files) {
                LOGGER.Debug("file=[" + file + "]");
                Folder folder;
                if (!folders.Contains(Path.GetDirectoryName(file))) {
                    folder = new Folder();
                    folder.Repository = 
                        manager.FetchRepository(Path.GetDirectoryName(file));
                    folder.Root = manager.FetchRoot(Path.GetDirectoryName(file));
                    folder.Tag = manager.FetchTag(Path.GetDirectoryName(file));
                } else {
                    folder = folders[Path.GetDirectoryName(file)];
                }   

                if (!folder.Entries.Contains(file)) {
                    Entry entry = Entry.CreateEntry(file);
                    LOGGER.Debug ("Adding new entry to test entries=[" + entry + "]");
                    folder.Entries.Add(entry.FullPath, entry);
                }
            }

            return folders;
        }

        /// <summary>
        /// 
        /// </summary>
        [TearDown]
        public void TempOverrideTearDown () {
            // Do nothing
        }
	}
}
