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
#endregion

using System;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.FileSystem;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Streams;

using log4net;

namespace ICSharpCode.SharpCvsLib.Responses {

    /// <summary>
    /// New-entry pathname \n
    ///     Additional data: New Entries line, \n. Like Checked-in, but the file is 
    ///     not up to date.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class NewEntryResponse : AbstractResponse {
        private readonly ILog LOGGER = LogManager.GetLogger(typeof (NewEntryResponse));
        /// <summary>
        /// Process a new entry response.
        /// 
        /// TODO: Copied implementation from CheckedInResponse, determine if this
        ///     is correct or not.
        /// </summary>
        public override void Process() {
            string localPath      = this.ReadLine();
            string repositoryPath = this.ReadLine();
            string entryLine      = this.ReadLine();

            PathTranslator orgPath   =
                new PathTranslator (Services.Repository,
                repositoryPath);

            string fileName = orgPath.LocalPathAndFilename;
            Factory factory = new Factory();
            Entry  entry = (Entry)
                factory.CreateCvsObject(orgPath.CurrentDir, Entry.FILE_NAME, entryLine);
            Manager manager = new Manager (Services.Repository.WorkingPath);
            manager.Add (entry);
        }

        /// <summary>
        /// Return true if this response cancels the transaction
        /// </summary>
        public override bool IsTerminating {
            get {return false;}
        }
    }
}
