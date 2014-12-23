#region "Copyright"
// Copyright (C) 2005 Clayton Harbour
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
#endregion "Copyright"

using System;

namespace ICSharpCode.SharpCvsLib.Attributes {
	/// <summary>
	/// Summary description for ProtocolAttribute.
	/// </summary>
	[AttributeUsage(AttributeTargets.Class | AttributeTargets.Assembly | 
         AttributeTargets.Interface | AttributeTargets.Delegate, 
         AllowMultiple=true,Inherited=false)] 
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
	public class AuthorAttribute : System.Attribute {
        private string _name;
        private string _email;
        private string _year;

        /// <summary>
        /// Name of the author.
        /// </summary>
        public string Name {
            get { return this._name; }
            set { this._name = value; }
        }

        /// <summary>
        /// Email address of the author.
        /// </summary>
        public string Email {
            get { return this._email; } 
            set { this._email = value; }
        }

        /// <summary>
        /// The year or year string (i.e. 2004-2005) that the author did 
        /// work on the source.
        /// </summary>
        public string Year {
            get { return this._year; }
            set { this._year = value; }
        }

        /// <summary>
        /// Creat a new instance of the <see cref="AuthorAttribute"/>.
        /// </summary>
        /// <param name="name">The name of the author.</param>
        /// <param name="email">Email to contact the author.</param>
		public AuthorAttribute(string name, string email, string year) {
            this._name = name;
            this._email = email;
            this._year = year;
		}
	}
}
