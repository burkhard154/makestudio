using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using System.Drawing;
using System.Threading;
using System.Resources;
using System.Reflection;
using stdole;
using makestudio;

namespace jpl.vscs2017.testplugin
{

	// This class is a helper class and just to get the picture into jedi make
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
	
	// global Vars
	public class Vars
	{
		static public makestudio.IJApplication _jedimake = null;
		static public bool FCanceled = false;

		public const string struPluginName = "Visual C# 2010 Plugin Example";
		public const string struPluginAuthor = "Burkhard Schranz (burkhard.schranz@optimeas.de)";
        public const string struPluginHint = "Visual C# 2010 Test Plugin for MakeStudio";
		public const string stCategory = "Examples";
	}

	// This is the implementation of the plugin interface to jedi make - Fixed Name: JMPlugin
	public class JMPlugin
	{

		public string Name() 
		{
			return Vars.struPluginName;
		}
	  
		public string Author()
		{
			return Vars.struPluginAuthor;
		}

		public string Description() 
		{
 			return Vars.struPluginHint;
		}

	  	public int RegisterPlugin(JApplication Application)
	    {
			Vars._jedimake = Application;

            Actions cs = new Actions();

            Bitmap image = new Bitmap(Properties.Resources.cs);
            stdole.StdPicture p = (stdole.StdPicture)AxHostPublic.GetIPictureDispFromPicture(image);

			//Actions
            Application.AddMenuAction("ActionVSCS2005", "Examples\\" + "Action from test plugin", "Hint VS 2005", p, cs);

			//Commands
			Application.LogMessage("C# Register Plugin");
            Application.AddCommandType("New Example VS C# 2010", "", Vars.stCategory, p, "txt", -1, new VSCS2005Create());

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

	  	public int MinorVersion()
		{
			return 0;
		}
	  

	  	public int MajorVersion() 
	  	{
			return 1;
		}


	  	public void AfterAllPluginsLoaded()
	  	{

	  	}
		
	}
}
