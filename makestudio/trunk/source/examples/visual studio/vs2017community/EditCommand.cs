using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace jpl.vscs2010.testplugin
{
    public partial class EditCommand : Form
    {
        public EditCommand()
        {
            InitializeComponent();
        }

        private void button3_Click(object sender, EventArgs e)
        {
            MessageBox.Show("Hallo BUS");
        }
    }
}