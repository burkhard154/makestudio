#region "Copyright"
// ClearStaticDirectoryResponse.cs
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
//                Clayton Harbour  {claytonharbour@sporadicism.com}
#endregion

using System;
using System.Text;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Streams;

using log4net;

namespace ICSharpCode.SharpCvsLib.Responses {
    /// <summary>
    /// Handle a clear static directory response.
    ///
    /// from: http://www.loria.fr/~molli/cvs/doc/cvsclient_5.html
    ///    Clear-static-directory pathname \n
    ///
    /// This instructs the client to un-set the Entries.Static flag,
    /// which it should then send back to the server in a Static-directory
    /// request whenever the directory is operated on. pathname ends in a
    /// slash; its purpose is to specify a directory, not a file within a
    /// directory.
    ///
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class ClearStaticDirectoryResponse : AbstractResponse {
        private readonly ILog LOGGER =
            LogManager.GetLogger (typeof (ClearStaticDirectoryResponse));
        /// <summary>
        /// Process a clear static directory response.
        /// </summary>
        public override void Process() {
            string localPath      = this.ReadLine();
            string reposPath      = this.ReadLine();

            Manager manager = new Manager (Services.Repository.WorkingPath);
            manager.AddRepository (Services.Repository, localPath, reposPath);
            manager.AddRoot (Services.Repository, localPath, reposPath);
            PathTranslator pathTranslator = new PathTranslator (Services.Repository, reposPath);

            Factory factory = new Factory();
            Entry entry = 
                (Entry)factory.CreateCvsObject(pathTranslator.CurrentDir, Entry.FILE_NAME, 
                Entry.CreateEntry(pathTranslator.CurrentDir).FileContents);
            // the root module directory does not get a cvs Entries line.
            // TODO: There has to be a cleaner way to do this...
            if (Services.Repository.WorkingPath.Length <= entry.Path.Length) {
                manager.AddEntry(entry);
            }

            Services.ResponseMessageEvents.SendResponseMessage(
                String.Format("Updating {0}", RemoveTrailingSlash(localPath)), this.GetType());
        }

        private string RemoveTrailingSlash(string localPath) {
            if (localPath.EndsWith("/")) {
                return localPath.Substring(0, localPath.Length - 1);
            }
            return localPath;
        }

        /// <summary>
        /// Return true if this response cancels the transaction
        /// </summary>
        public override bool IsTerminating {
            get {return false;}
        }
    }
}
