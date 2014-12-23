{$IFDEF BLOCKHEADER}
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
{$ENDIF BLOCKHEADER}
using System;
using System.Threading;

using System.Runtime.InteropServices;
using Jvcsmak;
using System.Drawing;
using System.Windows.Forms;

//perhaps you have to change to the module where "Vars" are defined
using %FILESPREFIX%JVCSMakPlugin;
using %FILESPREFIX%%COMMANDIDENTIFIER%Edit;

using System.Resources;
using stdole;


/**** Sample Code to register this command *******

	  Plugin%COMMANDIDENTIFIER%Callback Plugin%COMMANDIDENTIFIER%CB = new Plugin%COMMANDIDENTIFIER%Callback();

	  //Create and register Callback for the command type
		AJVCSMakApp.AddCommandType("%COMMANDNAME%", "", Vars.stCategory, p, "txt", -1,
		  Plugin%COMMANDIDENTIFIER%CB);
**** End Sample Code  *******/


//perhaps you have to change to the common namespace of this module
namespace %PLUGINIDENTIFIER%
{

	public class Plugin%COMMANDIDENTIFIER%: Jvcsmak.ICommand2
	{
	   string FCaption = "%COMMANDNAME%";
	   {$IFDEF BLOCKSAMPLEVAR}
	   string FTestValue = "%SAMPLEVARVALUE%";
	   {$ENDIF BLOCKSAMPLEVAR}

	   public bool DrawItem(int Handle, int Left, int Top, int Right, int Bottom, bool Selected, bool BriefView, uint BkColor)
	   {
		  {$IFNDEF BLOCKSAMPLEPAINTCODE}
		  return true;
		  {$ELSE}
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
		  {$IFDEF BLOCKSAMPLEVAR}
		  newGraphics.DrawString(FCaption + " " + FTestValue, stringFont, brush,
			Rect.Left + 2, Rect.Top + Offset);
		  {$ENDIF BLOCKSAMPLEVAR}
		  if (!BriefView)
		  {
			Offset = (int)(newGraphics.MeasureString(FCaption, stringFont).Height) + 2;
			stringFont = new System.Drawing.Font("MS Sans Serif", 8);
			brush = new SolidBrush(Color.Blue);
			newGraphics.DrawString("only for testing", stringFont, brush,
			  Rect.Left + 10, Rect.Top + Offset);
		  }
		  return false;
		  {$ENDIF ~BLOCKSAMPLEPAINTCODE}
	   }

	   public bool EditItem()
	   {
		  Edit%COMMANDIDENTIFIER%Params EditForm = new Edit%COMMANDIDENTIFIER%Params();
		  {$IFDEF BLOCKSAMPLEVAR}
		  EditForm.textBox1.Text = FTestValue;
		  {$ENDIF BLOCKSAMPLEVAR}
		  if (EditForm.ShowDialog() == DialogResult.OK)
		  {
		        {$IFDEF BLOCKSAMPLEVAR}
			FTestValue = EditForm.textBox1.Text;
			{$ENDIF BLOCKSAMPLEVAR}
		  }
		  return true;
	   }

	   public bool ExecuteItem()
	   {
          Vars.FCanceled = false;
                  {$IFDEF BLOCKSAMPLEVAR}
		  Vars.jvcsmak.LogMessage(FCaption + " " + FTestValue);
		  {$ENDIF BLOCKSAMPLEVAR}
		  Vars.jvcsmak.LogMessage("Executing %COMMANDNAME%...");
		  return true;
	   }

	   public int MeasureItem(int Handle, bool BriefView)
	   {
		  {$IFNDEF BLOCKSAMPLEPAINTCODE}
		  return -1; //auto
		  {$ELSE}
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
		  {$ENDIF ~BLOCKSAMPLEPAINTCODE}
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
				{$IFDEF BLOCKSAMPLEVAR}
				return 1;
				{$ELSE}
				return 0;
				{$ENDIF BLOCKSAMPLEVAR}
			}
	   }

	   public string get_ParamNames(int Index)
	   {
		   {$IFDEF BLOCKSAMPLEVAR}
		   return "%SAMPLEVARNAME%";
		   {$ELSE}
		   return "";
		   {$ENDIF BLOCKSAMPLEVAR}
	   }

	   public void set_ParamValues(string ParamName, string Value)
	   {
	           {$IFDEF BLOCKSAMPLEVAR}
		   if (ParamName == "%SAMPLEVARNAME%")
		   {
		   		FTestValue = Value;
		   }
		   {$ENDIF BLOCKSAMPLEVAR}
	   }

	   public string get_ParamValues(string ParamName)
	   {
	           {$IFDEF BLOCKSAMPLEVAR}
		   if (ParamName == "%SAMPLEVARNAME%")
		   {
				return FTestValue;
		   }
		   else
		   {$ENDIF BLOCKSAMPLEVAR}
		   {
				return "";
		   }
	   }
       
       public bool Get_OwnerDraw();
       {
           //Use Caption and PreviewText!
           //Otherwise, if Result = true, you can use
           //DrawItem and MeasureItem
           return false;
	   }

	   string Get_PreviewText();
	   {
		   {$IFDEF BLOCKSAMPLEVAR}
		   return FTestValue;
		   {$ELSE}
		   return "";
		   {$ENDIF BLOCKSAMPLEVAR}
	   }

	   object Notify( string Notification, object Parameter);
	   {
		  //nothing to do
		  //for future purpose - e.g. active language changed
		  return 0;
	   }

	   object Get_Properties();
	   {
		  //nothing to do
		  //for future purpose - integration of an property inspector
		  //and extended handling of command parameters/properties
		  return null;
	   }

	}

	public class Plugin%COMMANDIDENTIFIER%Callback: Jvcsmak.ICommandCallback
	{

     	public const string IDPlugin%COMMANDIDENTIFIER% = "%PLUGINIDENTIFIER%.%COMMANDIDENTIFIER%";
		Plugin%COMMANDIDENTIFIER% PluginTest = new Plugin%COMMANDIDENTIFIER%();

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
			return IDPluginTestmodule;
		}

	}

}
