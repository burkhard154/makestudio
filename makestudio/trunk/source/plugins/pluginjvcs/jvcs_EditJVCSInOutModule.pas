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

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to JVCSMAK
2003/11/28  USchuster - 2nd Migrationstep (fixed header and removed Variants)
2003/12/05  USchuster - re-formatted
2004/05/02  USchuster - fixed #bf009 (added possibility to add and remove single projects)
2005/01/05  BSchranz  - Migration to plugin code
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit jvcs_EditJVCSInOutModule;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, StdCtrls, ExtCtrls, jvcs_CheckInOut, ComCtrls, ToolWin, Buttons,
  jvcs_Utils, jvcs_vars, jvcs_dlgselectjvcsmodule, jclstrings;

type
  TFormJVCSInOutEdit = class(TForm)
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
    ImageList1: TImageList;
    edServer: TEdit;
    edPort: TEdit;
    edPasswort: TEdit;
    OpenDialogJVCS: TOpenDialog;
    btJVCSExe: TButton;
    cbIdentities: TComboBox;
    lvModules: TListView;
    ToolImageList: TImageList;
    Panel6: TPanel;
    btDelete: TButton;
    btAddIn: TButton;
    btAddOut: TButton;
    procedure btAddOutClick(Sender: TObject);
    procedure btAddInClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure cbIdentitiesChange(Sender: TObject);
    procedure btExePathClick(Sender: TObject);
    procedure OnUserInput(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    Module: TJVCSInOutCommand;
    procedure FillModuleList;
    procedure AddModules(InOut: Integer);
  public
    { Public-Deklarationen }
  end;

function DlgEditJVCSInOutModule(M: TJVCSInOutCommand): Boolean;

implementation

{$R *.dfm}

uses
  jvcs_DlgSelectJVCSProject;

resourcestring
  stdAcceptLoginProps = 'Die Projektauswahl kann nur vorgenommen werden wenn die Logineinstellungen gespeichert werden'#10#13+
                        'Sollen die Einstellungen jetzt gespeichert werden?';

function DlgEditJVCSInOutModule(M: TJVCSInOutCommand): Boolean;
var
  I, J: Integer;
  S: string;
begin
  Result := False;
  with TFormJVCSInOutEdit.Create(nil) do
  try
    Module := M;
    edUsername.Text := M.Username;
    edPort.Text := IntToStr(M.Port);
    edPasswort.Text := M.Password;
    edServer.Text := M.Server;

    FillModuleList;

    J := FindIdentity(M.Username, M.Server, M.Password, M.Port);
    if J > -1 then
      cbIdentities.ItemIndex := J
    else
      cbIdentities.ItemIndex := cbIdentities.Items.Count - 1;

    if ShowModal = mrOk then
    begin
      if cbIdentities.ItemIndex<cbIdentities.Items.Count - 1 then
        SetLastUsedIdentity(cbIdentities.ItemIndex)
      else
        try
          SetLastUsedIdentityEx(edUsername.Text, edServer.Text, edPasswort.Text, StrToInt(edPort.Text));
        except
        end;

      M.Server := edServer.Text;
      M.Username := edUsername.Text;
      try
        M.Port := StrToInt(edPort.Text);
      except
      end;
      M.Password := edPasswort.Text;

      M.Modules.Clear;
      for I := 0 to lvModules.Items.Count - 1 do begin
        S := lvModules.Items[I].Caption;
        S := S + ';' + IntToStr(lvModules.Items[I].ImageIndex);
        S := S + ';' + lvModules.Items[I].SubItems[0];
        S := S + ';' + lvModules.Items[I].SubItems[1];
        M.Modules.Add(S);
      end;

      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TFormJVCSInOutEdit.btExePathClick(Sender: TObject);
begin
  OpenDialogJVCS.FileName := GetJVCSExe;
  if OpenDialogJVCS.Execute then
    SetJVCSExe(OpenDialogJVCS.FileName);
end;

procedure TFormJVCSInOutEdit.cbIdentitiesChange(Sender: TObject);
begin
  if cbIdentities.ItemIndex < cbIdentities.Items.Count - 1 then begin
    with GetIdentity(cbIdentities.ItemIndex) do begin
      edUsername.Text := User;
      edServer.Text := Hostname;
      edPort.Text := IntToStr(Port);
      edPasswort.Text := Password;
    end;
  end;
end;

procedure TFormJVCSInOutEdit.OnUserInput(Sender: TObject; var Key: Char);
begin
  cbIdentities.ItemIndex := cbIdentities.Items.Count - 1;
end;

procedure TFormJVCSInOutEdit.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to GetIdentityCount - 1 do
    cbIdentities.Items.Add(GetIdentity(I).Identity);
  cbIdentities.Items.Add(stdDefaultIdentity);
end;

procedure TFormJVCSInOutEdit.btDeleteClick(Sender: TObject);
var
  I: Integer;
begin
  if lvModules.Selected <> nil then begin
    I := lvModules.Selected.Index;
    lvModules.Selected.Delete;
    if I < lvModules.Items.Count then
      lvModules.Items[I].Selected := True
    else
    if lvModules.Items.Count > 0 then
      lvModules.Items[Pred(lvModules.Items.Count)].Selected := True;
  end;
end;

procedure TFormJVCSInOutEdit.btAddInClick(Sender: TObject);
begin
  AddModules(1);
end;

procedure TFormJVCSInOutEdit.FillModuleList;
var
  I: Integer;
  sl: TStringList;
  it: TListItem;
begin
  lvModules.Items.BeginUpdate;
  try
    lvModules.Items.Clear;
  finally
    lvModules.Items.EndUpdate;
  end;
  sl := TStringList.Create;
  try
    for I := 0 to Module.Modules.Count - 1 do begin
      StrTokenToStrings(Module.Modules[I], ';', sl);
      if sl.Count > 3 then begin
        it := lvModules.Items.Add;
        it.Caption := sl[0];
        it.SubItems.Add(sl[2]);
        it.SubItems.Add(sl[3]);
        if sl[1] = '0' then
          it.ImageIndex := 0
        else
          it.ImageIndex := 1;
        it.StateIndex := it.ImageIndex;
      end;
    end;
  finally
  end;
end;

procedure TFormJVCSInOutEdit.AddModules(InOut: Integer);
var
  sl, sl1: TStringList;
  I: Integer;
  it: TListItem;
begin
  sl1 := TStringList.Create;
  sl := TStringList.Create;
  try
    if SelectJVCSModules(edUsername.Text, edServer.Text, edPasswort.Text,
                         StrToInt(edPort.Text), sl) then
    begin
      for I := 0 to sl.Count - 1 do begin
        StrTokenToStrings(sl[I], ';', sl1);
        if sl1.Count > 2 then begin
          it := lvModules.Items.Add;
          it.Caption := sl1[0];
          it.SubItems.Add(sl1[1]);
          it.SubItems.Add(sl1[2]);
          it.ImageIndex := InOut;
          it.StateIndex := it.ImageIndex;
        end;
      end;
    end;
  finally
    sl1.Free;
    sl.Free;
  end;
end;

procedure TFormJVCSInOutEdit.btAddOutClick(Sender: TObject);
begin
  AddModules(0);
end;

end.
