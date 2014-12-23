#region "Copyright"
// UpdatedResponse.cs
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
//    <author>Mike Krueger</author>
//    <author>Clayton Harbour  {claytonharbour@sporadicism.com}</author>
#endregion

using System;
using System.IO;
using System.Text;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Messages;
using ICSharpCode.SharpCvsLib.Streams;

using log4net;

namespace ICSharpCode.SharpCvsLib.Responses {
    /// <summary>
    /// Response from cvs server after update command.
    ///
    /// Updated pathname \n
    ///     Additional data: New Entries line, \n, mode, \n, file transmission.
    ///     A new copy of the file is enclosed. This is used for a new revision
    ///     of an existing file, or for a new file, or for any other case in which
    ///     the local (client-side) copy of the file needs to be updated, and after
    ///     being updated it will be up to date. If any directory in pathname does
    ///     not exist, create it. This response is not used if Created and
    ///     Update-existing are supported.
    ///
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class UpdatedResponse : AbstractResponse {
        private readonly ILog LOGGER =
            LogManager.GetLogger (typeof (UpdatedResponse));

        /// <summary>
        /// Process the response from the cvs server.
        /// </summary>
        public override void Process() {
            Manager manager = new Manager (Services.Repository.WorkingPath);
            string localPath = this.ReadLine();
            string reposPath = this.ReadLine();
            string entry     = this.ReadLine();
            string flags     = this.ReadLine();
            string sizeStr   = this.ReadLine();

            PathTranslator orgPath   =
                new PathTranslator (Services.Repository,
                                    reposPath);
            string localPathAndFilename = orgPath.LocalPathAndFilename;
            string directory = orgPath.LocalPath;

            bool compress = sizeStr[0] == 'z';

            if (compress) {
                sizeStr = sizeStr.Substring(1);
            }

            int size  = Int32.Parse(sizeStr);

            if (!Directory.Exists(orgPath.LocalPath)) {
                Directory.CreateDirectory(orgPath.LocalPath);

            }

            if (Services.NextFile != null && Services.NextFile.Length > 0) {
                localPathAndFilename = Services.NextFile;
                Services.NextFile = null;
            }

            Factory factory = new Factory();
            Entry e = (Entry)
                factory.CreateCvsObject(new DirectoryInfo(Path.GetDirectoryName(orgPath.CurrentDir.FullName)), 
                Entry.FILE_NAME, entry);

            if (e.IsBinaryFile) {
                Services.UncompressedFileHandler.ReceiveBinaryFile(Stream,
                        localPathAndFilename,
                        size);
            } else {
                Services.UncompressedFileHandler.ReceiveTextFile(Stream,
                        localPathAndFilename,
                        size);
            }

            e.Date = Services.NextFileDate;
            Services.NextFileDate = null;

            manager.AddEntry(e);
            manager.SetFileTimeStamp (e.FullPath, e.TimeStamp, e.IsUtcTimeStamp);

            UpdateMessage message = new UpdateMessage ();
            message.Module = Services.Repository.WorkingDirectoryName;
            message.Repository =  orgPath.RelativePath;
            message.Filename = e.Name;
            Services.SendMessage (message.Message);
            Services.ResponseMessageEvents.SendResponseMessage(message.Message, this.GetType());
        }

        /// <summary>
        /// Indicator stating whether the response is terminating or not.
        /// </summary>
        public override bool IsTerminating {
            get {return false;}
        }
    }
}
