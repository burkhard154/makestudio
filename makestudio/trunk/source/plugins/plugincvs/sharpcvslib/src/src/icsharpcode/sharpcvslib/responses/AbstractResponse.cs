#region "Copyright"
// Copyright (C) 2004 Clayton Harbour
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
//
#endregion

using System;
using System.Text;
using System.Text.RegularExpressions;

using ICSharpCode.SharpCvsLib.Attributes;
using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Streams;

namespace ICSharpCode.SharpCvsLib.Responses {
	/// <summary>
	/// Provides common implementation and methods for the response objects.
	/// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2004-2005")]
	public abstract class AbstractResponse : IResponse {
        private StringBuilder msg = new StringBuilder();
        private CvsStream stream;
        private IResponseServices services;

        /// <summary>
        /// Create a new instance of the abstract response class.
        /// </summary>
        public AbstractResponse() {
        }

        /// <summary>
        /// Append a new line to the internal message.
        /// </summary>
        /// <param name="line"></param>
        protected void AddLine (string line) {
            this.msg.Append(line).Append(Environment.NewLine);
        }

        /// <summary>
        /// Property for holding the cvs stream.
        /// </summary>
        protected CvsStream Stream {
            get {return this.stream;}
            set {this.stream = value;}
        }

        /// <summary>
        /// Property for subclasses to access the response services.
        /// </summary>
        protected IResponseServices Services {
            get {return this.services;}
            set {this.services = value;}
        }

        /// <summary>
        /// Public setter for the cvs stream class.
        /// </summary>
        public CvsStream CvsStream {
            set {this.stream = value;}
        }

        /// <summary>
        /// Public setter for the response services object.
        /// </summary>
        public IResponseServices ResponseServices {
            set {this.services = value;}
        }

        /// <summary>
        /// Read a line from the cvs stream.
        /// </summary>
        /// <returns></returns>
        protected string ReadLine() {
            if (null == this.stream) {
                throw new ArgumentException("CvsStream must be set in the process command.");
            }
            string line = this.stream.ReadLine();
            this.AddLine(line);
            return line;
        }

        /// <summary>
        /// <see cref="IResponse.ResponseString"/> for details.
        /// </summary>
        public string ResponseString {
            get {return this.msg.ToString();}
        }

        /// <summary>
        /// <see cref="IResponse.IsTerminating"/> for details.
        /// </summary>
        public abstract bool IsTerminating {get;}

        /// <summary>
        /// Set the cvs stream and then delegate processing to the parameterless process command.
        /// </summary>
        /// <param name="cvsStream"></param>
        /// <param name="services"></param>
        public void Process(CvsStream cvsStream, IResponseServices services) {
            this.stream = cvsStream;
            this.services = services;
            this.Process();
        }

        /// <summary>
        /// <see cref="IResponse.Process"/> for details.
        /// </summary>
        public abstract void Process();
	}
}
