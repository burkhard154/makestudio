/*
 * Created by SharpDevelop.
 * User: Burkhard
 * Date: 02.06.2006
 * Time: 18:36
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using System.Drawing;
using System.Threading;
using System.Resources;
using System.Reflection;
using stdole;
using JediMake;

namespace jpl.sharpdevelop.testplugin
{

	public class Vars
	{
		static public JApplication _jedimake = null;
		static public bool FCanceled = false;

		public const string struPluginName = "SharpDevelop 2.0 Plugin Example";
		public const string struPluginAuthor = "Burkhard Schranz (burkhard@burkhardschranz.de)";
		public const string struPluginHint = "SharpDevelop 2.0 Test Plugin for JVCSMAK";
		public const string stCategory = "Examples";
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
	/// The plugininterface always has to be COM visible
	[ComVisible(true)]
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
			
			//ResourceManager rm = new ResourceManager("plugin", Assembly.GetExecutingAssembly());

			Bitmap bmp = new Bitmap(16, 16);
			//bmp = (Bitmap)rm.GetObject( "Batchfile");
			
			stdole.StdPicture p = (stdole.StdPicture)AxHostPublic.GetIPictureDispFromPicture(bmp);

				//--- add actions --------------------------------------------------------
			/*Application.AddMenuAction("FormActions_acTestaction1_Name",
		  		"Testplugin\\TestItem\\" + "Action from test plugin",
		  		"FormActions.acTestaction1.Hint", p, TestActionCB);*/

			//--- add modules --------------------------------------------------------
			Application.LogMessage("C# Register Plugin");

			//Application.AddCommandType("Test command cs", "", Vars.stCategory, p, "txt", -1,
		  	//	PluginTestcommandcsCB);

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
