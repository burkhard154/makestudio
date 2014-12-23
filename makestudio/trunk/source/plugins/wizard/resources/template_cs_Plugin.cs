{$IFDEF BLOCKHEADER}
(------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: template_cs_JVCSMakPlugin.cs

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
Mar/05
- USc: - AxHostPublic lets the IDE think this a form
       - strange EOleException "Overflow or underflow in the arithmetic operation"
         in JVCSMak
ToDo
- real icons(how to store, modify and retrieve icons from resources?)
-----------------------------------------------------------------------------

Unit history:


------------------------------------------------------------------------------)
{$ENDIF BLOCKHEADER}
using System;
using System.Threading;

using System.Runtime.InteropServices;
using Jvcsmak;
using System.Drawing;
using System.Windows.Forms;

using System.Resources;
using stdole;

namespace %PLUGINIDENTIFIER%
{

	public class Vars
	{
		static public JApplication jvcsmak = null;
		static public bool FCanceled = false;

		public const string struPluginName = "%PLUGINNAME%";
		public const string struPluginAuthor = "%PLUGINAUTHOR%";
		public const string struPluginHint = "%PLUGINHINT%";
		public const string stCategory = "%PLUGINCATEGORY%";
	}

	/// <summary>
	/// This class is a helper class and just to get the picture into jedi make
	/// </summary>
	public class AxHostPublic: AxHost
	{
		public AxHostPublic(): base(null)
		{
		}
		public new static IPicture GetIPictureDispFromPicture(Image image)
		{
			return (IPicture)AxHost.GetIPictureFromPicture(image);
		}
	}

	/// <summary>
	/// This is the implementation of the plugin interface to jedi make
	/// </summary>
	public class JMPlugin
	{

	  public string Name {
			get
			{
				return Vars.struPluginName;
			}
	  }

	  public string Author {
			get
			{
				return Vars.struPluginAuthor;
			}
	  }

	  public string Description {
			get
			{
				return Vars.struPluginHint;
			}
	  }

	  public string RequiredPlugins {
			get
			{
				return "";
			}
	  }

	  public int RegisterPlugin(JApplication Application)
	  {
	    	Vars.jvcsmak = Application;


{$IFDEF BLOCKMENUACTION}
            Actions cs = new Actions();
            stdole.StdPicture p = (stdole.StdPicture)AxHostPublic.GetIPictureDispFromPicture(cs.imageList1.Images[0]);

             //--- add actions --------------------------------------------------------
    		Application.AddMenuAction("FormActions_acTestaction1_Name",
	    	  "%MENUACTIONPATH%" + "%TESTACTIONCAPTION%",
		      "<Hint>", p, cs);
{$ENDIF BLOCKMENUACTION}

	    	//--- add modules --------------------------------------------------------
		    Application.LogMessage("C# Register Plugin");

    		Application.AddCommandType("%COMMANDNAME%", "", Vars.stCategory, p, "txt", -1,
	    	  Plugin%COMMANDIDENTIFIER%CB);

    		//Credits
	    	Application.AddCreditInfo(Vars.struPluginName + " by " + Vars.struPluginAuthor);

    		//Additional Info
	    	Application.AddAdditionalInfo(Vars.struPluginHint);

		return 0;
	  }

	  public int UnregisterPlugin()
	  {
        return 0;
	  }

	  public int MinorVersion {
			get
			{
				return 0;
			}
	  }

	  public int MajorVersion {
			get
			{
				return 1;
			}
	  }

	  public Guid OptionsPageGUID {
			get
			{
				return new Guid();
			}
	  }

	  public void AfterAllPluginsLoaded()
	  {

	  }

	}
}
