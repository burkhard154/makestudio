#region "Copyright"
// UploadFileRequest.cs
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
// As a special exception, if you link this library with other files to
// produce an executable, this library does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why the
// executable file might be covered by the GNU General Public License.
#endregion

using System.IO;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Config;

namespace ICSharpCode.SharpCvsLib.Requests {

    /// <summary>
    /// Upload a file to the repository.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class UploadFileRequest : AbstractRequest {
        private string filename;
        private SharpCvsLibConfig settings = SharpCvsLibConfig.GetInstance();

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="filename">The name of the file to upload.</param>
        public UploadFileRequest(string filename) {
            this.filename = filename;
        }

        /// <summary>
        /// Upload the file to the server.
        /// </summary>
        public override string RequestString {
            get {
                FileStream fs = File.OpenRead(filename);
                byte[] data = new byte[fs.Length];
                fs.Read(data, 0, data.Length);
                fs.Close();

                return data.Length.ToString() + "\n" + SharpCvsLibConfig.Encoding.GetString(data);
            }
        }

        /// <summary>
        /// Response expected: <code>false</code>.
        /// </summary>
        public override bool IsResponseExpected {
            get {
                return false;
            }
        }
    }
}
