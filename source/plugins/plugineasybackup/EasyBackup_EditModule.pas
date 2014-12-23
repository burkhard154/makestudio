(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EasyBackup_EditModule.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/05  BSchranz  - Easybackup Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit EasyBackup_EditModule;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ExtCtrls, ComCtrls,
  ActnList, ImgList, ToolWin,
  JvBaseDlg, JvBrowseFolder, Mask, JvToolEdit, JvEdit,
  EasyBackup_Module, JvSpin, JvComponent, JvExMask;

type
  TFormEditEasyBakModule = class(TForm)
    ImageList1: TImageList;
    ActionList1: TActionList;
    acAdd: TAction;
    acDelete: TAction;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lstSource: TListBox;
    TabSheet2: TTabSheet;
    rgCopy: TRadioGroup;
    GroupBox3: TGroupBox;
    rbbkCopyAge: TRadioButton;
    rbbkCopyAll: TRadioButton;
    rbbkDeleteAll: TRadioButton;
    Label1: TLabel;
    rbbkIgnoreAll: TRadioButton;
    cbbkShowSelection: TCheckBox;
    rgLogbook: TRadioGroup;
    Label2: TLabel;
    DlgDir: TJvBrowseForFolderDialog;
    edTarget: TJvDirectoryEdit;
    ToolBar1: TToolBar;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    Button1: TButton;
    Button2: TButton;
    edbkAge: TJvSpinEdit;
    procedure btAddClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure rgCopyClick(Sender: TObject);
  end;

function DlgEditEasyBakModule(M: TEasyBackupCommand): Boolean;

implementation


{$R *.dfm}

function DlgEditEasyBakModule(M: TEasyBackupCommand): Boolean;
begin
  Result := False;
  with TFormEditEasyBakModule.Create(nil) do
  try
    rbbkDeleteAll.Checked := M.BackupDelete;
    rbbkCopyAll.Checked := M.BackupCopy;
    rbbkIgnoreAll.Checked := M.BackupIgnore;
    rbbkCopyAge.Checked := M.BackupCopyAge;
    cbbkShowSelection.Checked := M.BackupShowSelection;
    rgCopy.ItemIndex := M.CopyOption;
    rgLogbook.ItemIndex := M.LogbookOption;
    edbkAge.Value := M.BackupCopyAgeValue;
    edTarget.Text := M.TargetDir;
    lstSource.Items.Clear;
    lstSource.Items.AddStrings(M.SourceDirs);
    if ShowModal = mrOk then
    begin
      Result := True;
      M.BackupDelete := rbbkDeleteAll.Checked;
      M.BackupCopy := rbbkCopyAll.Checked;
      M.BackupIgnore := rbbkIgnoreAll.Checked;
      M.BackupCopyAge := rbbkCopyAge.Checked;
      M.BackupShowSelection := cbbkShowSelection.Checked;
      M.CopyOption := rgCopy.ItemIndex;
      M.LogbookOption := rgLogbook.ItemIndex;
      M.BackupCopyAgeValue := Round(edbkAge.Value);
      M.TargetDir := edTarget.Text;
      M.SourceDirs.Clear;
      M.SourceDirs.AddStrings(lstSource.Items);
    end;
  finally
    Free;
  end;
end;

procedure TFormEditEasyBakModule.btAddClick(Sender: TObject);
begin
  if DlgDir.Execute then
    lstSource.Items.Add(DlgDir.Directory);
end;

procedure TFormEditEasyBakModule.btDeleteClick(Sender: TObject);
begin
  if lstSource.ItemIndex>=0 then
    lstSource.Items.Delete(lstSource.ItemIndex);
end;

{procedure TFormMain.btStartClick(Sender: TObject);
var
  f: TFormDoCopy;
begin
  f := TFormDoCopy.Create(nil);
  try
    f.Source.Assign(lstSource.Items);
    f.Target := edTarget.Text;
    f.DoSynchronize := rgCopy.ItemIndex = 1;
    f.ShowSelectDlg := cbbkShowSelection.Checked;
    LogbookShowAll := rgLogbook.ItemIndex = 1;

    if rbbkCopyAll.Checked then
      f.SyncronizeOption := soCopy
    else if rbbkDeleteAll.Checked then
      f.SyncronizeOption := soDelete
    else if rbbkIgnoreAll.Checked then
      f.SyncronizeOption := soIgnore
    else if rbbkCopyAge.Checked then
      f.SyncronizeOption := soCopyAge;
    f.SyncAge := edbkAge.Value;

    f.Timer1.Enabled := true;
    f.ShowModal;
  finally
    f.Free;
  end;
end;}

{procedure TFormMain.LoadFromStrings(sl: TStringlist);
var
  ini: TIniFile;
  i, co: Integer;
begin
  if not FileExists(aFilename) then
  begin
    MessageDlg('"'+aFilename + '" existiert nicht!', mtError, [mbOk], 0);
    Exit;
  end;

  ini := TIniFile.Create(aFileName);
  try
    lstSource.Items.Clear;
    co := ini.ReadInteger('Main', 'Count', 0);
    for i:=0 to co-1 do
      lstSource.Items.Add(ini.ReadString('Main', 'Dir'+IntToStr(i), '???'));
    edTarget.Text := ini.ReadString('Main', 'Target', '');
    Filename := aFileName;

    rbbkDeleteAll.Checked := ini.ReadBool('Main', 'OnBackupDelete', false);
    rbbkCopyAll.Checked := ini.ReadBool('Main', 'OnBackupCopy', true);
    rbbkIgnoreAll.Checked := ini.ReadBool('Main', 'OnBackupIgnore', false);
    rbbkCopyAge.Checked := ini.ReadBool('Main', 'OnBackupCopyAge', false);
    cbbkShowSelection.Checked := ini.ReadBool('Main', 'OnBackupShowSelection', true);

    rgCopy.ItemIndex := ini.ReadInteger('Main', 'OnCopy', 0);
    edbkAge.Value := ini.ReadFloat('Main', 'RestoreAge', 1.0);
  finally
    ini.Free;
  end;
  SetCaption;
end;

procedure TFormMain.SaveToStrings(sl: TStringList);
var
  ini: TIniFile;
  i: Integer;
begin
  ini := TIniFile.Create(aFileName);
  try
    ini.WriteInteger('Main', 'Count', lstSource.Items.Count);
    for i:=0 to lstSource.Items.Count-1 do
      ini.WriteString('Main', 'Dir'+IntToStr(i), lstSource.Items[i]);
    ini.WriteString('Main', 'Target', edTarget.Text);
    Filename := aFileName;
    ini.WriteBool('Main', 'OnBackupDelete', rbbkDeleteAll.Checked);
    ini.WriteBool('Main', 'OnBackupCopy', rbbkCopyAll.Checked);
    ini.WriteBool('Main', 'OnBackupIgnore', rbbkIgnoreAll.Checked);
    ini.WriteBool('Main', 'OnBackupCopyAge', rbbkCopyAge.Checked);
    ini.WriteBool('Main', 'OnBackupShowSelection', cbbkShowSelection.Checked);
    ini.WriteInteger('Main', 'OnCopy', rgCopy.ItemIndex);
    ini.WriteFloat('Main', 'RestoreAge', edbkAge.Value);
  finally
    ini.Free;
  end;
  SetCaption;
end;}

procedure TFormEditEasyBakModule.rgCopyClick(Sender: TObject);
begin
  rbbkCopyAll.Enabled := rgCopy.ItemIndex = 1;
  rbbkCopyAge.Enabled := rgCopy.ItemIndex = 1;
  rbbkDeleteAll.Enabled := rgCopy.ItemIndex = 1;
  rbbkIgnoreAll.Enabled := rgCopy.ItemIndex = 1;
  cbbkShowSelection.Enabled := rgCopy.ItemIndex = 1;
  edbkAge.Enabled := rgCopy.ItemIndex = 1;
end;

end.
