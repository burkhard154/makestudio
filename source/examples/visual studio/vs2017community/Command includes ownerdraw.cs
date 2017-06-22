/*
 * Created by SharpDevelop.
 * User: Burkhard
 * Date: 02.06.2006
 * Time: 22:37
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */

using System;
using System.Threading;
using System.Runtime.InteropServices;
using JediMake;
using System.Drawing;
using System.Windows.Forms;
using System.Resources;
using stdole;

namespace jpl.vscs2010.testplugin
{
	/// <summary>
	 /**** Sample Code to register this command *******

	  PluginTestcommandcsCallback PluginTestcommandcsCB = new PluginTestcommandcsCallback();

	  //Create and register Callback for the command type
		Application.AddCommandType("Test command cs", "", Vars.stCategory, p, "txt", -1,
		  PluginTestcommandcsCB);
	**** End Sample Code  *******/
	/// </summary>
	/// 

	public class VSCS2005 : ICommand2
	{
	   string FCaption = "Test command cs";
	   string FTestValue = "TestValue";

	   public VSCS2005()
       {
       }

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
		  EditCommand EditForm = new EditCommand();
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
		  Vars._jedimake.LogMessage(FCaption + " " + FTestValue);
		  Vars._jedimake.LogMessage("Executing Test command cs...");
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

	   public string Caption 
	   {

			get
			{
				return FCaption;
			}

			set
			{
			   FCaption = value;
			}

	   }

	   public int ParamCount
	   {
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
			   return true;
			}
	   }

	   public string PreviewText 
	   {
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

    //[ComVisible(true)]
    public class VSCS2005Create:ICommandCallback
    {
        const string Ident = "vscs2005.examplecommand";

        public VSCS2005Create()
        { }

        public Object CreateCommand()
        {
            return new VSCS2005();
        }

        public string GetIdentifier()
        {
            return Ident;
        }

        public void SetCanceled(bool aCanceled)
        {
            Vars.FCanceled = aCanceled;
        }
    
    }
}
