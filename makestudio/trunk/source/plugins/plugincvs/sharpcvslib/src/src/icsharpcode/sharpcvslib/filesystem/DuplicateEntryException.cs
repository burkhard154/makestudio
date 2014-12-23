#region Copyright
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

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.FileSystem {
    /// <summary>
    ///     This exception is thrown if there is no entry matching the specifed
    ///         criteria.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    [Obsolete ("Use ICSharpCode.SharpCvsLib.Exceptions.DuplicateEntryException.")]
    public class DuplicateEntryException : ICSharpCode.SharpCvsLib.Exceptions.DuplicateEntryException {

        /// <summary>
        /// Occurs if a duplicate entry has found it's way into the Entries file.
        ///     This is an indication that something has corrupted the cvs repository.
        /// </summary>
        public DuplicateEntryException () {

        }

        /// <summary>
        /// Occurs if a duplicate entry has found it's way into the Entries file.
        ///     This is an indication that something has corrupted the cvs repository.
        /// </summary>
        /// <param name="message">Additional information to pass on in the
        ///     exception.</param>
        public DuplicateEntryException (String message) : base (message) {
        }

        /// <summary>
        /// Occurs if a duplicate entry has found it's way into the Entries file.
        ///     This is an indication that something has corrupted the cvs repository.
        /// </summary>
        /// <param name="message">A message that will be helpful for someone
        ///     resolving the issue with the library.</param>
        /// <param name="e">An exception that has caused this error, or has
        ///     led to this error.</param>
        public DuplicateEntryException (String message, Exception e) : base (message, e) {

        }

    }

}
