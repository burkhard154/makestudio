/* ------------------------------------------------------------------------------
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

2005/03/14  USchuster - ported template to C#

------------------------------------------------------------------------------*/
using System;
using System.Threading;

using System.Runtime.InteropServices;
using JediMake;
using System.Drawing;
using System.Windows.Forms;

using System.Resources;
using stdole;

namespace csharpplugintest_
{

	public class Vars
	{
		static public JApplication jvcsmak = null;
		static public bool FCanceled = false;

		public const string struPluginName = "C# Plugin Template";
		public const string struPluginAuthor = "Burkhard Schranz (burkhard@additive-net.de)";
		public const string struPluginHint = "Test Plugin for JVCSMAK";
		public const string stCategory = "C#";
	}


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

	[ComVisibleAttribute(true)]
	public class TestAction: JediMake.IActionCallback
	{
	   public void Execute(string Action)
	   {
		  ActionTest TestForm = new ActionTest();
		  TestForm.ShowDialog();
	   }
	}

	[ComVisibleAttribute(true)]
	public class Plugin: JediMake.IPlugin
	{
	  PluginTestcommandcsCallback PluginTestcommandcsCB = new PluginTestcommandcsCallback();
	  TestAction TestActionCB = new TestAction();

	  public string PluginName {
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

	  public int RegisterPlugin(JApplication AJVCSMakApp)
	  {
		Vars.jvcsmak = AJVCSMakApp;

		Bitmap bmp = new Bitmap(16, 16);
		for(int x = 0; x < 16; x++)
		{
		  for(int y = 0; y < 16; y++)
		  {
			bmp.SetPixel(x, y, Color.Green);
		  }
		}

		stdole.StdPicture p = (stdole.StdPicture)AxHostPublic.GetIPictureDispFromPicture(bmp);

				//--- add actions --------------------------------------------------------
		AJVCSMakApp.AddMenuAction("FormActions_acTestaction1_Name",
		  "Testplugin\\TestItem\\" + "Action from test plugin",
		  "FormActions.acTestaction1.Hint", p, TestActionCB);

		//--- add modules --------------------------------------------------------
		AJVCSMakApp.LogMessage("C# Register Plugin");

		AJVCSMakApp.AddCommandType("Test command cs", "", Vars.stCategory, p, "txt", -1,
		  PluginTestcommandcsCB);

		//Credits
		AJVCSMakApp.AddCreditInfo(Vars.struPluginName + " by " + Vars.struPluginAuthor);

		//Additional Info
		AJVCSMakApp.AddAdditionalInfo(Vars.struPluginHint);

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
