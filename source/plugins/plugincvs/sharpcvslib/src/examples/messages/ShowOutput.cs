using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.IO;
using System.Threading;
using System.Windows.Forms;
using System.Data;

using ICSharpCode.SharpCvsLib.Client;
using ICSharpCode.SharpCvsLib.Commands;
using ICSharpCode.SharpCvsLib.Messages;
using ICSharpCode.SharpCvsLib.Misc;
using ICSharpCode.SharpCvsLib.Responses;

namespace Listen
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class ShowOutput : System.Windows.Forms.Form
	{
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label ErrorLabel;
        private System.Windows.Forms.Button CheckoutButton;
        private System.Windows.Forms.TextBox CvsRoot;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox Module;
        private System.Windows.Forms.TextBox UpdatedResponse;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label LocalDirectory;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox ErrorResponse;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox Response;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox Password;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public ShowOutput()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if (components != null) 
				{
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
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(ShowOutput));
            this.label1 = new System.Windows.Forms.Label();
            this.UpdatedResponse = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.ErrorLabel = new System.Windows.Forms.Label();
            this.CvsRoot = new System.Windows.Forms.TextBox();
            this.CheckoutButton = new System.Windows.Forms.Button();
            this.label3 = new System.Windows.Forms.Label();
            this.Module = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.LocalDirectory = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.ErrorResponse = new System.Windows.Forms.TextBox();
            this.Response = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.Password = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(8, 296);
            this.label1.Name = "label1";
            this.label1.TabIndex = 0;
            this.label1.Text = "UpdatedResponse";
            // 
            // UpdatedResponse
            // 
            this.UpdatedResponse.ForeColor = System.Drawing.SystemColors.WindowText;
            this.UpdatedResponse.Location = new System.Drawing.Point(112, 296);
            this.UpdatedResponse.Multiline = true;
            this.UpdatedResponse.Name = "UpdatedResponse";
            this.UpdatedResponse.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.UpdatedResponse.Size = new System.Drawing.Size(688, 56);
            this.UpdatedResponse.TabIndex = 1;
            this.UpdatedResponse.Text = "";
            // 
            // label2
            // 
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            this.label2.Location = new System.Drawing.Point(8, 8);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(216, 23);
            this.label2.TabIndex = 2;
            this.label2.Text = "Listen to Message Events";
            // 
            // label4
            // 
            this.label4.Location = new System.Drawing.Point(8, 64);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(64, 16);
            this.label4.TabIndex = 4;
            this.label4.Text = "CVSROOT:";
            // 
            // ErrorLabel
            // 
            this.ErrorLabel.BackColor = System.Drawing.SystemColors.Control;
            this.ErrorLabel.ForeColor = System.Drawing.Color.Red;
            this.ErrorLabel.Location = new System.Drawing.Point(8, 144);
            this.ErrorLabel.Name = "ErrorLabel";
            this.ErrorLabel.Size = new System.Drawing.Size(808, 16);
            this.ErrorLabel.TabIndex = 5;
            // 
            // CvsRoot
            // 
            this.CvsRoot.Location = new System.Drawing.Point(72, 64);
            this.CvsRoot.Name = "CvsRoot";
            this.CvsRoot.Size = new System.Drawing.Size(640, 20);
            this.CvsRoot.TabIndex = 6;
            this.CvsRoot.Text = ":pserver:anonymous@cvs.sourceforge.net:/cvsroot/sharpcvslib";
            // 
            // CheckoutButton
            // 
            this.CheckoutButton.Location = new System.Drawing.Point(728, 40);
            this.CheckoutButton.Name = "CheckoutButton";
            this.CheckoutButton.TabIndex = 7;
            this.CheckoutButton.Text = "Checkout";
            this.CheckoutButton.Click += new System.EventHandler(this.CheckoutButton_Click);
            // 
            // label3
            // 
            this.label3.Location = new System.Drawing.Point(16, 112);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(48, 16);
            this.label3.TabIndex = 8;
            this.label3.Text = "Module";
            // 
            // Module
            // 
            this.Module.Location = new System.Drawing.Point(72, 112);
            this.Module.Name = "Module";
            this.Module.Size = new System.Drawing.Size(640, 20);
            this.Module.TabIndex = 9;
            this.Module.Text = "sharpcvslib";
            // 
            // label5
            // 
            this.label5.Location = new System.Drawing.Point(8, 40);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(56, 16);
            this.label5.TabIndex = 10;
            this.label5.Text = "Local Dir";
            // 
            // LocalDirectory
            // 
            this.LocalDirectory.Location = new System.Drawing.Point(72, 40);
            this.LocalDirectory.Name = "LocalDirectory";
            this.LocalDirectory.Size = new System.Drawing.Size(632, 16);
            this.LocalDirectory.TabIndex = 11;
            this.LocalDirectory.Text = "C:\\DOCUME~1\\CLAYTO~1\\LOCALS~1\\Temp\\sharpcvslib-examples";
            // 
            // label6
            // 
            this.label6.Location = new System.Drawing.Point(8, 168);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(88, 16);
            this.label6.TabIndex = 12;
            this.label6.Text = "ErrorResponse";
            // 
            // ErrorResponse
            // 
            this.ErrorResponse.ForeColor = System.Drawing.Color.Red;
            this.ErrorResponse.Location = new System.Drawing.Point(112, 168);
            this.ErrorResponse.Multiline = true;
            this.ErrorResponse.Name = "ErrorResponse";
            this.ErrorResponse.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.ErrorResponse.Size = new System.Drawing.Size(688, 56);
            this.ErrorResponse.TabIndex = 13;
            this.ErrorResponse.Text = "";
            // 
            // Response
            // 
            this.Response.ForeColor = System.Drawing.SystemColors.WindowText;
            this.Response.Location = new System.Drawing.Point(112, 232);
            this.Response.Multiline = true;
            this.Response.Name = "Response";
            this.Response.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.Response.Size = new System.Drawing.Size(688, 56);
            this.Response.TabIndex = 15;
            this.Response.Text = "";
            // 
            // label7
            // 
            this.label7.Location = new System.Drawing.Point(8, 232);
            this.label7.Name = "label7";
            this.label7.TabIndex = 14;
            this.label7.Text = "Response";
            // 
            // label8
            // 
            this.label8.Location = new System.Drawing.Point(8, 88);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(56, 16);
            this.label8.TabIndex = 16;
            this.label8.Text = "Password";
            // 
            // Password
            // 
            this.Password.Location = new System.Drawing.Point(72, 88);
            this.Password.Name = "Password";
            this.Password.Size = new System.Drawing.Size(640, 20);
            this.Password.TabIndex = 17;
            this.Password.Text = "";
            // 
            // ShowOutput
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(824, 437);
            this.Controls.Add(this.Password);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.Response);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.ErrorResponse);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.LocalDirectory);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.Module);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.CheckoutButton);
            this.Controls.Add(this.CvsRoot);
            this.Controls.Add(this.ErrorLabel);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.UpdatedResponse);
            this.Controls.Add(this.label1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "ShowOutput";
            this.Text = "Display Cvs Output";
            this.ResumeLayout(false);

        }
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
			Application.Run(new ShowOutput());
		}

        private CvsRoot GetCvsRoot (string cvsRootString) {
            return new CvsRoot(cvsRootString);
        }

        public void WriteErrorResponse(object sender, MessageEventArgs e) {
            this.ErrorResponse.Text += String.Format("{0} - {1}\n", e.Message, e.Prefix);
        }

        /// <summary>
        /// Write the update message to the update message text box.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void WriteUpdatedResponse(object sender, MessageEventArgs e) {
            this.UpdatedResponse.Text += String.Format("{0} - {1}\n", e.Message, e.Prefix);
        }

        public void WriteResponse(object sender, MessageEventArgs e) {
            this.Response.Text += String.Format("{0} - {1}\n", e.Message, e.Prefix);
        }

        /// <summary>
        /// Registers events that will be listened to.  An extension of this might be to add a check box
        /// that can be unchecked/ checked if the event should be listened to.
        /// </summary>
        private void RegisterListenEvents (CVSServerConnection connection) {
            connection.ResponseMessageEvents.ErrorResponseMessageEvent += 
                new MessageEventHandler(WriteErrorResponse);

            connection.ResponseMessageEvent += 
                new MessageEventHandler(WriteResponse);

            connection.ResponseMessageEvents.UpdatedResponseMessageEvent += 
                new MessageEventHandler(WriteUpdatedResponse);
        }

        protected void CheckoutButton_Click(object sender, System.EventArgs e) {
            try {
                if (!Directory.Exists(this.LocalDirectory.Text)) {
                    Directory.CreateDirectory(this.LocalDirectory.Text);
                }

                Thread thread = new Thread(new ThreadStart(Checkout));
                thread.Start();
            } catch (Exception ex) {
                this.ErrorResponse.Text += String.Format("{0}\n", ex);
            } finally {
//                if (Directory.Exists(localDirectory)) {
//                    Directory.Delete(localDirectory);
//                }
            }
        }

        private void Checkout () {
            CvsRoot cvsRoot;
            try {
                cvsRoot = this.GetCvsRoot (this.CvsRoot.Text);
            } catch (CvsRootParseException ex) {
                this.ErrorLabel.Text = ex.Message;

                // can't do anything without a root.
                return;
            }

            WorkingDirectory CurrentWorkingDirectory = new WorkingDirectory(cvsRoot,
                this.LocalDirectory.Text, Module.Text);

            // Create new CheckoutModuleCommand object
            ICommand checkoutCommand = new CheckoutModuleCommand(CurrentWorkingDirectory);

            CVSServerConnection serverConn = new CVSServerConnection(CurrentWorkingDirectory);

            this.RegisterListenEvents(serverConn);

            serverConn.Connect(CurrentWorkingDirectory, this.Password.Text);
            checkoutCommand.Execute(serverConn);
            serverConn.Close();
        }

	}
}
