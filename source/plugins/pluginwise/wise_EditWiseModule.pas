(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: wise_EditWiseModule.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/21  JDuenow   - launched Wise Module
2005/01/04  BSchranz  - Plugin created
2005/02/05  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit wise_EditWiseModule;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, wise_Module,
  Dialogs, StdCtrls, ExtCtrls, Buttons, JvBaseDlg, JvBrowseFolder,
  JvComponent, JvToolEdit, JclFileUtils, Mask, JvExMask;

type
  TFormWiseModuleEdit = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    ListProjects: TListBox;
    btnAddProject: TSpeedButton;
    btnRemoveProject: TSpeedButton;
    OpenDialog2: TOpenDialog;
    edtExecPath: TJvFilenameEdit;
    edtOutputDir: TJvDirectoryEdit;
    procedure btnAddProjectClick(Sender: TObject);
    procedure btnRemoveProjectClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgEditWiseModule(M: TWiseCommand): Boolean;

var
  FormWiseModuleEdit: TFormWiseModuleEdit;

implementation

{$R *.dfm}

function DlgEditWiseModule(M: TWiseCommand): Boolean;
var
  I: Integer;
begin
  Result := False;
  with TFormWiseModuleEdit.Create(nil) do
  try
    edtExecPath.Text := M.ExecPath;
    if M.ExecPath = '' then edtExecPath.Text := M.DefaultPath;
    edtOutputDir.Text := M.OutputDir;
    for I := 0 to M.Projects.Count - 1 do
      ListProjects.Items.Add(M.Projects[I]);
    if ShowModal = mrOk then
    begin
      Result := True;
      M.ExecPath := edtExecPath.Text;
      M.OutputDir := PathAddSeparator(edtOutputDir.Text);
      M.Projects.Clear;
      for I := 0 to ListProjects.Items.Count - 1 do
        M.Projects.Add(ListProjects.Items[I]);
    end;
  finally
    Free;
  end;
end;

procedure TFormWiseModuleEdit.btnAddProjectClick(Sender: TObject);
begin
  if ListProjects.Items.Count > 0 then
    OpenDialog2.InitialDir := ExtractFilePath(ListProjects.Items[ListProjects.Items.Count - 1])
  else
    OpenDialog2.InitialDir := ExtractFilePath(edtOutputDir.Text);

  if OpenDialog2.Execute then
    ListProjects.Items.Add(OpenDialog2.FileName);
end;

procedure TFormWiseModuleEdit.btnRemoveProjectClick(Sender: TObject);
begin
  if ListProjects.ItemIndex >= 0 then
    ListProjects.Items.Delete(ListProjects.ItemIndex);
end;

end.
