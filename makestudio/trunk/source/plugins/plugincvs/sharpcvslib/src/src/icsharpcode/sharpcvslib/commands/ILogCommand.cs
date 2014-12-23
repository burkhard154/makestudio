#region "Copyright"
// ILogCommand.cs
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
#endregion

using System;
using System.Collections;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Commands {

    /// <summary>
    /// Interface for the log and rlog commands.
    /// </summary>
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2003-2005")]
    public interface ILogCommand : ICommand {
        /// <summary>
        /// The date arguments for the log command.
        /// </summary>
        ICollection DateArgs {get;}

        /// <summary>
        /// The default branch to use for the module.
        /// </summary>
        bool DefaultBranch {get;set;}

        /// <summary>
        /// Indicate <code>true</code> if only the headers and descriptions are to be output.
        /// </summary>
        bool HeaderAndDescOnly {get;set;}

        /// <summary>
        /// Indicate <code>true</code> if only the headers are to be output.
        /// </summary>
        bool HeaderOnly {get;set;}

        /// <summary>
        /// Indicate <code>true</code> if the tags should not be output.
        /// </summary>
        bool NoTags {get;set;}

        /// <summary>
        /// Adds a date range using exclusive dates.
        /// This is equivalent to the command line option "-d startDate&lt;endDate"
        /// 
        /// <param name="startDate"></param>
        /// <param name="endDate"></param>
        /// </summary>
        void AddExclusiveDateRange(DateTime startDate, DateTime endDate);
        
        /// <summary>
        /// Adds a open ended date range with an exclusive start date.
        /// This is equivalent to the command line option "-d startDate&lt;"
        /// 
        /// <param name="startDate"></param>
        /// </summary>
        void AddExclusiveDateStart(DateTime startDate);
        
        /// <summary>
        /// Adds a open ended date range with an exclusive start date.
        /// This is equivalent to the command line option "-d &lt;endDate"
        /// 
        /// <param name="endDate"></param>
        /// </summary>
        void AddExclusiveDateEnd(DateTime endDate);

        /// <summary>
        /// Adds a date range using inclusive dates.
        /// This is equivalent to the command line option "-d startDate&lt;=endDate"
        /// 
        /// <param name="startDate"></param>
        /// <param name="endDate"></param>
        /// </summary>
        void AddInclusiveDateRange(DateTime startDate, DateTime endDate);
        
        /// <summary>
        /// Adds a open ended date range with an inclusive start date.
        /// This is equivalent to the command line option "-d startDate&lt;="
        /// 
        /// <param name="startDate"></param>
        /// </summary>
        void AddInclusiveDateStart(DateTime startDate);
        
        /// <summary>
        /// Adds a open ended date range with an inclusive start date.
        /// This is equivalent to the command line option "-d &lt;=endDate"
        /// 
        /// <param name="endDate"></param>
        /// </summary>
        void AddInclusiveDateEnd(DateTime endDate);
        
        /// <summary>
        /// Adds a single date to specify the most recent revision at or prior to this date.
        /// This is equivalent to the command line option "-d date"
        /// 
        /// <param name="date"></param>
        /// </summary>
        void AddDate(DateTime date);        
    }
}

