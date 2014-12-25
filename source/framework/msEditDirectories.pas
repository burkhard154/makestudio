(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EditDirectories.pas

The Initial Developer of the original DMAK-Code is:
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
2003/11/28  USchuster - 2nd Migrationstep (fixed header)
2003/12/05  USchuster - re-formatted
2004/02/24  USchuster - Update to JVCL 3.0 / JCL 1.90 (mantis #1353)
2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in
2005/02/09  BSchranz  - Added Copy, Past, Docking, Debugging
2005/04/09  BSchranz  - Translated to englisch


-----------------------------------------------------------------------------*)

unit msEditDirectories;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ToolWin, ExtCtrls, ImgList, Buttons, JvBaseDlg,
  JvBrowseFolder, msGlobals, JvComponent, JclFileUtils;

type
  TFormEditDirectories = class(TForm)
    Panel8: TPanel;
    Panel9: TPanel;
    ToolBar1: TToolBar;
    lvDirs: TListView;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    Panel1: TPanel;
    Panel2: TPanel;
    cbChangeSub: TCheckBox;
    ImageList1: TImageList;
    edDir: TEdit;
    btDir: TSpeedButton;
    DirDlg: TJvBrowseForFolderDialog;
    procedure lvDirsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure edDirChange(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure btDirClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    procedure FillDirList;
  end;

var
  FormEditDirectories: TFormEditDirectories;

implementation

{$R *.dfm}

procedure TFormEditDirectories.FillDirList;
var
  I, K: Integer;
  S: string;

  function IsDirInList(ADir: string): Boolean;
  var
    ii: Integer;
  begin
    Result := False;
    for ii := 0 to lvDirs.Items.Count - 1 do
      if CompareText(lvDirs.Items[ii].Caption, ADir) = 0 then
      begin
        Result := True;
        Break;
      end;
  end;

  procedure AddItem(S: string);
  begin
    if Length(S) > 0 then
      with lvDirs.Items.Add do
      begin
        Caption := S;
        SubItems.Add(S);
      end;
  end;

begin
  lvDirs.Items.Clear;
  with Programhandler do
  begin

    for I := 0 to Count - 1 do
{    begin
      //Module directory
      if Items[I] is TDMakNormalModule then
      begin
        S := CheckNoBackslash(ExtractFilePath(TDMakNormalModule(Items[I]).Filename));
        if not IsDirInList(S) then
          AddItem(S);
        //Destination directory
        S := CheckNoBackslash(TDMakNormalModule(Items[I]).OutputDir);
        if not IsDirInList(S) then
          AddItem(S);
        //Search Path's
        for K := 0 to TDMakNormalModule(Items[I]).SearchDirs.Count - 1 do
        begin
          S := CheckNoBackslash(TDMakNormalModule(Items[I]).SearchDirs[K]);
          if not IsDirInList(S) then
            AddItem(S);
        end;
      end;
    end;}

  end;
  edDir.Visible := False;
  btDir.Visible := False;
end;

procedure TFormEditDirectories.lvDirsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  edDir.Visible := lvDirs.Selected <> nil;
  btDir.Visible := edDir.Visible;
  if lvDirs.Selected <> nil then
  begin
    if lvDirs.Selected.SubItems[0] <> '' then
      edDir.Text := lvDirs.Selected.SubItems[0]
    else
      edDir.Text := lvDirs.Selected.Caption;
  end;
end;

procedure TFormEditDirectories.edDirChange(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  if lvDirs.Selected <> nil then
  begin
    lvDirs.Selected.SubItems[0] := edDir.Text;

    //find subdirectories
    if cbChangeSub.Checked then
      for I := 0 to lvDirs.Items.Count - 1 do
      begin
        if Pos(UpperCase(lvDirs.Selected.Caption), UpperCase(lvDirs.Items[I].Caption)) = 1 then
          if lvDirs.Items[I] <> lvDirs.Selected then
          begin
            S := Copy(lvDirs.Items[I].Caption, Length(lvDirs.Selected.Caption) + 1,
                  Length(lvDirs.Items[I].Caption) - Length(lvDirs.Selected.Caption));
            lvDirs.Items[I].SubItems[0] := edDir.Text + S;
          end;
      end;
  end;
end;

procedure TFormEditDirectories.ToolButton1Click(Sender: TObject);
var
  I, K: Integer;
  S: string;

  //todo - is equal to FillDirList.IsDirInList
  function IsDirInList(ADir: string): Boolean;
  var
    ii: Integer;
  begin
    Result := False;
    for ii := 0 to lvDirs.Items.Count - 1 do
      if CompareText(lvDirs.Items[ii].Caption, ADir) = 0 then
      begin
        Result := True;
        Break;
      end;
  end;

  function GetChangedDir(ADir: string): string;
  var
    ii: Integer;
  begin
    Result := ADir;
    for ii := 0 to lvDirs.Items.Count - 1 do
      if CompareText(lvDirs.Items[ii].Caption, ADir) = 0 then
      begin
        Result := PathAddSeparator(lvDirs.Items[ii].SubItems[0]);
        Break;
      end;
  end;

  procedure AddItem(S: string);
  begin
    if Length(S) > 0 then
      with lvDirs.Items.Add do
      begin
        Caption := S;
        SubItems.Add(S);
      end;
  end;

begin
  with Programhandler do
  begin

    for I := 0 to Count - 1 do
{    begin
      if Items[I] is TDMakNormalModule then
      begin
        //Module directory
        S := CheckNoBackslash(ExtractFilePath(TDMakNormalModule(Items[I]).Filename));
        if IsDirInList(S) then
          TDMakNormalModule(Items[I]).Filename :=
            GetChangedDir(S) + ExtractFileName(TDMakNormalModule(Items[I]).Filename);
        //Destination directory
        S := CheckNoBackslash(TDMakNormalModule(Items[I]).OutputDir);
        if IsDirInList(S) then
          TDMakNormalModule(Items[I]).OutputDir := CheckNoBackslash(GetChangedDir(S));
        //Search Path's
        for K := 0 to TDMakNormalModule(Items[I]).SearchDirs.Count - 1 do
        begin
          S := CheckNoBackslash(TDMakNormalModule(Items[I]).SearchDirs[K]);
          if IsDirInList(S) then
            TDMakNormalModule(Items[I]).SearchDirs[K] :=
              CheckNoBackslash(GetChangedDir(S));
        end;
      end;
    end;}

  end;

  Close;
  ModalResult := mrOk;
end;

procedure TFormEditDirectories.ToolButton2Click(Sender: TObject);
begin
  Close;
  ModalResult := mrCancel;
end;

procedure TFormEditDirectories.btDirClick(Sender: TObject);
begin
  DirDlg.Directory := edDir.Text;
  if DirDlg.Execute then
    edDir.Text := DirDlg.Directory;
end;

end.
