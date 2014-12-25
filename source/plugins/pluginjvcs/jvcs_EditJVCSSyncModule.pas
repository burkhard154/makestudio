(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EditJVCSSyncModule.pas

The Initial Devoloper of the original DMAK-Code is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
Code move to JEDI VCS:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to MakeStudio
2003/11/28  USchuster - 2nd Migrationstep (fixed header and removed Variants)
2003/12/05  USchuster - re-formatted
2004/05/02  USchuster - fixed #bf009 (added possibility to add and remove single projects)
2005/01/05  BSchranz  - Migration to plugin code
2005/02/04  USchuster - preparations for check in
2006/04/29  BSchranz  - jvcs Labels added

-----------------------------------------------------------------------------*)

unit jvcs_EditJVCSSyncModule;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, StdCtrls, ExtCtrls, jvcs_Module, ComCtrls, ToolWin, Buttons,
  jvcs_Utils, jvcs_vars;

type
  TFormJVCSSyncEdit = class(TForm)
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lbCompilerswitch: TLabel;
    Label4: TLabel;
    Panel5: TPanel;
    edUsername: TEdit;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Panel4: TPanel;
    lvProjects: TListBox;
    ImageList1: TImageList;
    edServer: TEdit;
    edPort: TEdit;
    edPasswort: TEdit;
    btProjects: TButton;
    ToolBar2: TToolBar;
    tbRemoveProjects: TToolButton;
    OpenDialogJVCS: TOpenDialog;
    btJVCSExe: TButton;
    cbIdentities: TComboBox;
    Panel6: TPanel;
    lbLabel: TLabel;
    Panel7: TPanel;
    edLabel: TEdit;
    rgOperation: TRadioGroup;
    procedure FormShow(Sender: TObject);
    procedure rgOperationClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnUserInput(Sender: TObject; var Key: Char);
    procedure cbIdentitiesChange(Sender: TObject);
    procedure btExePathClick(Sender: TObject);
    procedure btProjectsClick(Sender: TObject);
    procedure tbRemoveProjectsClick(Sender: TObject);
  private
    Module: TJVCSSyncCommand;
  public
    { Public-Deklarationen }
  end;

function DlgEditJVCSSyncModule(M: TJVCSSyncCommand): Boolean;

implementation

{$R *.dfm}

uses
  jvcs_DlgSelectJVCSProject;

resourcestring
  stdAcceptLoginProps = 'Die Projektauswahl kann nur vorgenommen werden wenn die Logineinstellungen gespeichert werden'#10#13+
                        'Sollen die Einstellungen jetzt gespeichert werden?';

function DlgEditJVCSSyncModule(M: TJVCSSyncCommand): Boolean;
var
  I, j: Integer;
begin
  Result := False;
  with TFormJVCSSyncEdit.Create(nil) do
  try
    Module := M;
    edUsername.Text := M.Username;
    edPort.Text := IntToStr( M.Port);
    edPasswort.Text := M.Password;
    edServer.Text := M.Server;
    rgOperation.ItemIndex := Ord( M.Operation);
    edLabel.Text := M.JLabel;

    j := FindIdentity( M.Username, m.Server, m.Password, m.Port);
    if j>-1 then
      cbIdentities.ItemIndex := j
    else
      cbIdentities.ItemIndex := cbIdentities.Items.Count - 1;

    for I := 0 to M.Projects.Count - 1 do
      lvProjects.Items.Add(M.Projects[I]);
    if ShowModal = mrOk then
    begin
      if cbIdentities.ItemIndex<cbIdentities.Items.Count-1 then
        SetLastUsedIdentity( cbIdentities.ItemIndex)
      else
        try
          SetLastUsedIdentityEx( edUsername.Text, edServer.Text, edPasswort.Text, StrToInt( edPort.Text));
        except end;

      M.Server := edServer.Text;
      M.Username := edUsername.Text;
      try
        M.Port := StrToInt( edPort.Text);
      except end;
      M.Password := edPasswort.Text;
      M.Operation := TJVCSSyncOperation( rgOperation.ItemIndex);
      M.JLabel := edLabel.Text;

      M.Projects.Clear;
      for I := 0 to lvProjects.Items.Count - 1 do
        M.Projects.Add(lvProjects.Items[I]);
      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TFormJVCSSyncEdit.btProjectsClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Assign(lvProjects.Items);
    if SelectJVCSProjects( edUsername.Text, edServer.Text, edPasswort.Text, StrToInt( edPort.Text), sl)
    then
      lvProjects.Items.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TFormJVCSSyncEdit.tbRemoveProjectsClick(Sender: TObject);
var
  I: Integer;
begin
  if lvProjects.MultiSelect then
  begin
    if lvProjects.SelCount > 0 then
      for I := Pred(lvProjects.Items.Count) downto 0 do
        if lvProjects.Selected[I] then
          lvProjects.Items.Delete(I);
  end
  else
  if lvProjects.ItemIndex <> -1 then
    lvProjects.Items.Delete(lvProjects.ItemIndex);
end;

procedure TFormJVCSSyncEdit.btExePathClick(Sender: TObject);
begin
  OpenDialogJVCS.FileName := GetJVCSExe;
  if OpenDialogJVCS.Execute then
    SetJVCSExe( OpenDialogJvcs.FileName);
end;

procedure TFormJVCSSyncEdit.cbIdentitiesChange(Sender: TObject);
begin
  if cbIdentities.ItemIndex<cbIdentities.Items.Count-1 then begin
    with GetIdentity( cbIdentities.ItemIndex) do begin
      edUsername.Text := User;
      edServer.Text := Hostname;
      edPort.Text := IntToStr( Port);
      edPasswort.Text := Password;
    end;
  end;
end;

procedure TFormJVCSSyncEdit.OnUserInput(Sender: TObject; var Key: Char);
begin
  cbIdentities.ItemIndex := cbIdentities.Items.Count-1;
end;

procedure TFormJVCSSyncEdit.FormCreate(Sender: TObject);
var i:Integer;
begin
  for i:=0 to GetIdentityCount-1 do
    cbIdentities.Items.Add( GetIdentity( i).Identity);
  cbIdentities.Items.Add( stdDefaultIdentity);
end;

procedure TFormJVCSSyncEdit.rgOperationClick(Sender: TObject);
begin
  case rgOperation.ItemIndex of
    0: edLabel.Enabled := false;
    1, 2: edLabel.Enabled := true;
  end;
  lbLabel.Enabled := edLabel.Enabled;
end;

procedure TFormJVCSSyncEdit.FormShow(Sender: TObject);
begin
  rgOperationClick( Sender);
end;

end.
