#region "Copyright"
// CVSServerConnection.cs
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
#endregion

using System;
using System.Text;

using log4net;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Util {
    /// <summary>
    ///     Utility class for formatting the ToString output of an object.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public class ToStringFormatter {

        StringBuilder formattedString;

        /// <summary>
        ///     Create a new instance of the formatter.  Initialize the string
        ///         format with the name of the object.
        /// </summary>
        public ToStringFormatter (String objectName) {
            this.formattedString = new StringBuilder ();
            this.formattedString.Append ("{Object=[").Append (objectName).Append ("]");
        }

        /// <summary>
        ///     Add a property name and property value to the internal string
        ///         representation.
        /// </summary>
        /// <param name="propertyName">Name of the property to include in
        ///     the output.</param>
        /// <param name="propertyValue">The value of the string to include in
        ///     the output.</param>
        public void AddProperty (String propertyName, String propertyValue) {
	    if (null == propertyName) {
		    propertyName = "";
	    }
	    if (null == propertyValue) {
		    propertyValue = "";
	    }
            this.formattedString.Append (Environment.NewLine).Append("\t").Append (propertyName).
            Append ("=[").Append (propertyValue).Append ("]");
        }

        /// <summary>
        ///     Add a property name and value to the internal string representation.
        /// </summary>
        /// <param name="propertyName">The name of the property to add.</param>
        /// <param name="propertyValue">The value of the property.</param>
        public void AddProperty (String propertyName, object propertyValue) {
            if (null == propertyValue) {
                this.AddProperty (propertyName, "null");
            } else {
                this.AddProperty (propertyName, propertyValue.ToString ());
            }

        }

        /// <summary>
        ///     Output the internal representation of the object as a string.
        /// </summary>
        /// <returns>The value of the string.</returns>
        public override String ToString () {
            return this.formattedString.Append(Environment.NewLine).Append(Environment.NewLine).ToString ();
        }
    }

}
