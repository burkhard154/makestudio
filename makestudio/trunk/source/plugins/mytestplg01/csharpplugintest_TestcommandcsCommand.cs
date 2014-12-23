/*------------------------------------------------------------------------------
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


/**** Sample Code to register this command *******

	  PluginTestcommandcsCallback PluginTestcommandcsCB = new PluginTestcommandcsCallback();

	  //Create and register Callback for the command type
		AJVCSMakApp.AddCommandType("Test command cs", "", Vars.stCategory, p, "txt", -1,
		  PluginTestcommandcsCB);
**** End Sample Code  *******/


//perhaps you have to change to the common namespace of this module
namespace csharpplugintest_
{

	[ComVisibleAttribute(true)]
	public class PluginTestcommandcs: JediMake.ICommand2
	{
	   string FCaption = "Test command cs";
	   string FTestValue = "TestValue";

	   public bool DrawItem(int Handle, int Left, int Top, int Right, int Bottom, bool Selected, bool BriefView, uint BkColor)
	   {
		  //----------------------------- Example ------------------------
		  Graphics newGraphics = Graphics.FromHdc((IntPtr) Handle);
		  Rectangle Rect = new Rectangle(Left, Top, Right - Left, Bottom - Top);
		  Brush brush = null;
		  if (Selected)
		  {
			brush = new SolidBrush(SystemColors.Highlight);
			newGraphics.FillRectangle(brush, Rect);
		  }
		  else
		  {
			brush = new SolidBrush(SystemColors.Window);
			newGraphics.FillRectangle(brush, Rect);
		  }
		  int Offset = 2;
		  System.Drawing.Font stringFont = new System.Drawing.Font("MS Sans Serif", 8, System.Drawing.FontStyle.Bold);
		  if (Selected)
		  {
			brush = new SolidBrush(SystemColors.HighlightText);
		  }
		  else
		  {
			brush = new SolidBrush(SystemColors.WindowText);
		  }
		  newGraphics.DrawString(FCaption + " " + FTestValue, stringFont, brush,
			Rect.Left + 2, Rect.Top + Offset);
		  if (!BriefView)
		  {
			Offset = (int)(newGraphics.MeasureString(FCaption, stringFont).Height) + 2;
			stringFont = new System.Drawing.Font("MS Sans Serif", 8);
			brush = new SolidBrush(Color.Blue);
			newGraphics.DrawString("only for testing", stringFont, brush,
			  Rect.Left + 10, Rect.Top + Offset);
		  }
		  return false;
	   }

	   public bool EditItem()
	   {
		  EditTestcommandcsParams EditForm = new EditTestcommandcsParams();
		  EditForm.textBox1.Text = FTestValue;
		  if (EditForm.ShowDialog() == DialogResult.OK)
		  {
			FTestValue = EditForm.textBox1.Text;
		  }
		  return true;
	   }

	   public bool ExecuteItem()
	   {
		  Vars.FCanceled = false;
		  Vars.jvcsmak.LogMessage(FCaption + " " + FTestValue);
		  Vars.jvcsmak.LogMessage("Executing Test command cs...");
		  return true;
	   }

	   public int MeasureItem(int Handle, bool BriefView)
	   {
		  //----------------------------- Example ------------------------
		  Graphics newGraphics = Graphics.FromHdc((IntPtr) Handle);
		  int Result = 2;
		  System.Drawing.Font stringFont = new System.Drawing.Font("MS Sans Serif", 8, System.Drawing.FontStyle.Bold);
		  Result += (int)(newGraphics.MeasureString(FCaption, stringFont).Height) + 2;
		  if (!BriefView)
		  {
			stringFont = new System.Drawing.Font("MS Sans Serif", 8);
			Result += (int)(newGraphics.MeasureString(FCaption, stringFont).Height) + 2;
		  }
		  return Result;
	   }

	   public void SetFilename(string Filename)
	   {
		 //Setting the Filename - used by the host at drag&drop
		 //enter your code here
	   }

	   public string Caption {

			get
			{
				return FCaption;
			}

			set
			{
			   FCaption = value;
			}

	   }

	   public int ParamCount{
			get
			{
				return 1;
			}
	   }

	   public string get_ParamNames(int Index)
	   {
		   return "TestEntry";
	   }

	   public void set_ParamValues(string ParamName, string Value)
	   {
		   if (ParamName == "TestEntry")
		   {
				FTestValue = Value;
		   }
	   }

	   public string get_ParamValues(string ParamName)
	   {
		   if (ParamName == "TestEntry")
		   {
				return FTestValue;
		   }
		   else
		   {
				return "";
		   }
	   }

	   public bool OwnerDraw {
			get
			{
			   //Use Caption and PreviewText!
			   //Otherwise, if Result = true, you can use
			   //DrawItem and MeasureItem
			   return false;
			}
	   }

	   public string PreviewText {
			get
			{
				   return FTestValue;
			}
	   }

	   public object Notify( string Notification, object Parameter)
	   {
		  //nothing to do
		  //for future purpose - e.g. active language changed
		  return 0;
	   }

	   public object Properties
	   {
			get
			{
			  //nothing to do
			  //for future purpose - integration of an property inspector
			  //and extended handling of command parameters/properties
			  return null;
			}
	   }

	}

	[ComVisibleAttribute(true)]
	public class PluginTestcommandcsCallback: JediMake.ICommandCallback
	{

		public const string IDPluginTestcommandcs = "csharpplugintest_.Testcommandcs";
		PluginTestcommandcs PluginTest = new PluginTestcommandcs();

		public Object CreateCommand()
		{
			return PluginTest;
		}

		public void SetCanceled(bool aCanceled)
		{
			Vars.FCanceled = aCanceled; //set by the server if the user press "Cancel" oder "Stop"
		}

		public string GetIdentifier()
		{
			return IDPluginTestcommandcs;
		}

	}

}
