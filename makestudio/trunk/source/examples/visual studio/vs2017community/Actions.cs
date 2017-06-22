using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using makestudio;

namespace jpl.vscs2010.testplugin
{
    public class Actions : IActionCallback
    {
                
        public void Execute( String Action)
        {
            MessageBox.Show( "Action from VS C# Called");
        }
    }
}