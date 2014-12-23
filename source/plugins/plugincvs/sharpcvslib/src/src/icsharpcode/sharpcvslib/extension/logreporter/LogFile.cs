#region "Copyright"
// Copyright (C) 2004 Gerald Evans
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

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Extension.LogReporter {
	using System;
	using System.Collections;
    using ICSharpCode.SharpCvsLib.Misc;
	
	/// <summary>
	/// Represents a single file from a LogReport
	/// </summary>
	/// <remarks>
	/// 	created by - gne
	/// 	created on - 28/02/2004 15:36:56
	/// </remarks>
    [Author("Gerald Evans", "gne@users.sourceforge.net", "2004")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
	public class LogFile : IEnumerable {
        private CvsRoot cvsRoot;
	    private string repositoryFnm;
		/// <summary>
		/// The repository path+filename
		/// </summary>
	    public string RepositoryFnm {
	        get { return repositoryFnm; }
	        set { repositoryFnm = value; }
	    }
	    
	    private string workingFnm;
		/// <summary>
		/// The repository path+filename
		/// </summary>
	    public string WorkingFnm {
	        get { 
                if ((null == this.workingFnm || this.workingFnm.Length == 0) 
                    && this.RepositoryFnm.Length >= 2) {
                    // remove the ,v from the end of the file.
                    string temp = this.RepositoryFnm.Substring(0, this.RepositoryFnm.Length - 2);
                    try {
                        // remove the cvs root server absolute path information
                        temp = temp.Substring(this.cvsRoot.CvsRepository.Length, 
                            temp.Length - this.cvsRoot.CvsRepository.Length);
                        // remove the /
                        temp = temp.Substring(1, temp.Length - 1);
                    } catch (Exception) {
                        // just take the repository filename if this errors out
                    }
                    return temp;
                }
                return workingFnm; }
	        set { workingFnm = value; }
	    }
	    
	    private string description;
		/// <summary>
		/// Description of this file
		/// </summary>
	    public string Description {
	        get { return description; }
	        set { description = value; }
	    }
	    
	    private LogSymbolicNames symbolicNames = new LogSymbolicNames();
		/// <summary>
		/// The symbolic name collection for this file
		/// </summary>
	    public LogSymbolicNames SymbolicNames {
	        get { return symbolicNames; }
	    }
	    
	    private ArrayList revisions = new ArrayList();
		
		/// <summary>
		/// Default constructor - initializes all fields to default values
		/// </summary>
		public LogFile(CvsRoot cvsRoot)
		{
		    this.repositoryFnm = "";
		    this.workingFnm = "";
		    this.description = "";
            this.cvsRoot = cvsRoot;
		}
		
//		/// <summary>
//		/// State constructor - initializes all fields to given values
//		/// </summary>
//		public LogFile(string repositoryFnm,
//		               string workingFnm,
//		               string description)
//		{
//System.Console.WriteLine("LogFile({0}, {1}, {2})", repositoryFnm, workingFnm, description);
//		    this.repositoryFnm = repositoryFnm;
//		    this.workingFnm = workingFnm;
//		    this.description = description;
//		}
		
		/// <summary>
		/// Adds a LogRevision to the LogFile
		/// Only called when the LogReport is being constructed
		/// </summary>
		internal void AddRevision(LogRevision revision)
		{
		    revisions.Add(revision);
		}
		
		/// <summary>
		/// Gets an enumerator to enumerate over the revisions in the LogFile
		/// </summary>
		public IEnumerator GetEnumerator()
		{
		    return revisions.GetEnumerator();
		}
		
		/// <summary>
		/// The number of revisions in this LogFile
		/// </summary>
		public int Count {
		    get { return revisions.Count; }
		}
		
		/// <summary>
		/// Indexer to the revisions in this LogFile
		/// </summary>
		public LogRevision this[int index] {
		    get { return (LogRevision)revisions[index]; }
		}
		
		/// <summary>
		/// ToString() for debugging etc.
		/// </summary>
		public override string ToString()
		{
		    return String.Format("[RepositoryFnm={0}, WorkingFnm={1}, Description={2}]",
		                         repositoryFnm, workingFnm, description);
		}
	}
}
