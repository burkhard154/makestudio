#region "Copyright"
// Copyright (C) 2003 Gerald Evans
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
using System.Xml;

using ICSharpCode.SharpCvsLib.Attributes;

namespace ICSharpCode.SharpCvsLib.Extension.ChangeLogReport {
    /// <summary>
    /// Represents the 'file' node that will be added to the XML file
    /// </summary>
    [Author("Gerald Evans", "gne@users.sourceforge.net", "2003")]
    [Author("Clayton Harbour", "claytonharbour@sporadicism.com", "2005")]
    public class FileRevision : object {
        
        private string filename;
        private string revision;
        private string previousRevision;
    	
    	/// <summary>
    	/// constructor
    	/// </summary>
    	public FileRevision(string filename, string revision, string previousRevision) {
    	    this.filename = filename;
    	    this.revision = revision;
    	    this.previousRevision = previousRevision;
    	}
    	
    	/// <summary>
    	/// Appends details about this file node to the XML file
    	/// </summary>
    	public void ExportToXml(XmlTextWriter xmlWriter) {
    	    xmlWriter.WriteStartElement("file");
    	    
    	    xmlWriter.WriteStartElement("name");
    	    xmlWriter.WriteString(filename);
    	    xmlWriter.WriteEndElement();
    	    		    
    	    xmlWriter.WriteStartElement("revision");
    	    xmlWriter.WriteString(revision);
    	    xmlWriter.WriteEndElement();
    	    		
    	    if (previousRevision != null && previousRevision.Length != 0) {
    		    xmlWriter.WriteStartElement("prevrevision");
    		    xmlWriter.WriteString(previousRevision);
    		    xmlWriter.WriteEndElement();
    	    }
    
    	    xmlWriter.WriteEndElement();    // "file"
    	}
    }
}

