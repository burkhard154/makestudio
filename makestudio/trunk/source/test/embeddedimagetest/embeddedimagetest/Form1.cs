using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using System.Diagnostics;

namespace EmbeddedImageTest {
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class Form1 : System.Windows.Forms.Form 	{

		private void DoTheDemo(){
			// m_toolBar1 is already created by visual designer

			CPlugInImages cImgs = new CPlugInImages();
			m_toolBar1.ImageList = cImgs.Images;
			for( int i=0; i<m_toolBar1.ImageList.Images.Count; i++){
				ToolBarButton tbb = new ToolBarButton();
				tbb.ImageIndex = i;
				m_toolBar1.Buttons.Add( tbb );
			}//i
		}

		#region Common stuff
		public Form1()	{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
			DoTheDemo();
		}
		private System.Windows.Forms.ToolBar m_toolBar1;

		private System.ComponentModel.IContainer components;

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing ){
			if( disposing )	{
				if (components != null) {
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.m_toolBar1 = new System.Windows.Forms.ToolBar();
			this.SuspendLayout();
			// 
			// m_toolBar1
			// 
			this.m_toolBar1.DropDownArrows = true;
			this.m_toolBar1.Location = new System.Drawing.Point(0, 0);
			this.m_toolBar1.Name = "m_toolBar1";
			this.m_toolBar1.ShowToolTips = true;
			this.m_toolBar1.Size = new System.Drawing.Size(850, 42);
			this.m_toolBar1.TabIndex = 0;
			// 
			// Form1
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(6, 15);
			this.ClientSize = new System.Drawing.Size(850, 90);
			this.Controls.Add(this.m_toolBar1);
			this.Name = "Form1";
			this.Text = "Demo for embedded images";
			this.ResumeLayout(false);

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 	{
			Application.Run(new Form1());
		}
		#endregion
	}
}
