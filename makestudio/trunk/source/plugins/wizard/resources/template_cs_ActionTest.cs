{$IFDEF BLOCKHEADER}
(------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: template_cs_ActionTest.cs

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/03/14  USchuster - ported template to C#

------------------------------------------------------------------------------)
{$ENDIF BLOCKHEADER}
using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;

namespace %PLUGINIDENTIFIER%
{
	/// <summary>
	/// Zusammenfassende Beschreibung für WinForm
	/// </summary>
	public class ActionTest : System.Windows.Forms.Form
	{
		/// <summary>
		/// Erforderliche Designervariable
		/// </summary>
		private System.ComponentModel.Container components = null;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Button button1;

		public ActionTest()
		{
			//
			// Erforderlich für die Unterstützung des Windows Forms-Designer
			//
			InitializeComponent();

			//
			// TODO: Fügen Sie nach dem Aufruf von InitializeComponent() Konstruktorcode hinzu.
			//
			FormBorderStyle = FormBorderStyle.Sizable;
			StartPosition = FormStartPosition.CenterParent;
			ShowInTaskbar = false;
		}

		/// <summary>
		/// Ressourcen nach der Verwendung bereinigen
		/// </summary>
		protected override void Dispose (bool disposing)
		{
			if (disposing)
			{
				if (components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}

		#region Vom Windows Form-Designer erzeugter Code
		/// <summary>
		/// Erforderliche Methode zur Unterstützung des Designers -
		/// ändern Sie die Methode nicht mit dem Quelltext-Editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.label1 = new System.Windows.Forms.Label();
			this.button1 = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// label1
			// 
			this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 24F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
			this.label1.Location = new System.Drawing.Point(24, 48);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(224, 40);
			this.label1.TabIndex = 0;
			this.label1.Text = "Testaction";
			// 
			// button1
			// 
			this.button1.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.button1.Location = new System.Drawing.Point(104, 168);
			this.button1.Name = "button1";
			this.button1.TabIndex = 1;
			this.button1.Text = "Ok";
			// 
			// ActionTest
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(352, 333);
			this.Controls.Add(this.button1);
			this.Controls.Add(this.label1);
			this.Name = "ActionTest";
			this.Text = "FormActionTest";
			this.ResumeLayout(false);
		}
		#endregion
	}
}
