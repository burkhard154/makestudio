#region "Copyright"
// ArgumentRequest.cs
// Copyright (C) 2004 Clayton Harbour
// comments are taken from CVS Client/Server reference manual which
// comes with the cvs client (www.cvshome.org)
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
//  <author>Clayton Harbour</author>
#endregion

using System;
using System.Text;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Requests {

    /// <summary>
    /// Response expected: no.
    /// Save argument for use in a subsequent command. Arguments accumulate until
    /// an argument-using command is given, at which point they are forgotten.
    /// </summary>
    [Author("Mike Krueger", "mike@icsharpcode.net", "2001")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class DateRequest : AbstractRequest {
        private DateTime startDate;
        private DateTime endDate;

        private bool isEndDateIncluded;
        /// <summary>
        /// <code>true</code> if the end date should be included in the request, otherwise
        /// <code>false</code>.
        /// </summary>
        public bool IsEndDateIncluded {
            get {return this.isEndDateIncluded;}
            set {this.isEndDateIncluded = value;}
        }

        /// <summary>The options that are available as
        /// arguments to the cvs server.</summary>
        private class Options {
            /// <summary>The cvs argument used to specify a revision
            ///     by date.</summary>
            public const String LOG_DATE = "-d";
        }

        /// <summary>
        /// Create a new instance of the rlog/ log date request.
        /// </summary>
        /// <param name="startDate"></param>
        public DateRequest(DateTime startDate) : this(startDate, DateTime.Now, true) {
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="startDate"></param>
        /// <param name="endDate"></param>
        public DateRequest(DateTime startDate, DateTime endDate) : this(startDate, endDate, true) {
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="startDate"></param>
        /// <param name="endDate"></param>
        /// <param name="isEndDateIncluded"></param>
        public DateRequest(DateTime startDate, DateTime endDate, bool isEndDateIncluded) {
            this.startDate = startDate;
            this.endDate = endDate;
            this.isEndDateIncluded = true;
        }

        /// <summary>
        /// Argument.
        /// </summary>
        public override string RequestString {
            get {
                StringBuilder msg = new StringBuilder ();
                msg.Append(Options.LOG_DATE);
                msg.Append(Util.DateParser.GetCvsDateString(startDate));
                
                msg.Append("<");
                if (this.IsEndDateIncluded) {
                    msg.Append("=");
                }
                msg.Append(Util.DateParser.GetCvsDateString(endDate));
                msg.Append("\n");
                return msg.ToString();
            }
        }

        /// <summary>
        /// <code>false</code>, response is not expected.
        /// </summary>
        public override bool IsResponseExpected {
            get {
                return false;
            }
        }
    }
}