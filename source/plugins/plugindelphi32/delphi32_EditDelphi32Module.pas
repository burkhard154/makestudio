(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EditModule.pas

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
2003/11/28  USchuster - 2nd Migrationstep (fixed header)
2003/12/05  USchuster - re-formatted
2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in
2006/04/11  BSchranz  - Register IDE DLL Expert support added
2006/04/30  USchuster - D5 fix

-----------------------------------------------------------------------------*)

unit delphi32_EditDelphi32Module;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, StdCtrls, ExtCtrls, ImgList, delphi32_Compile,
  Buttons, JvBaseDlg, JvBrowseFolder, delphi32_Vars, JvComponent,
  JvComponentBase;

type
  TFormEditModule = class(TForm)
    Panel3: TPanel;
    Label1: TLabel;
    lbCompilerswitch: TLabel;
    Label4: TLabel;
    Panel5: TPanel;
    edCompilerSwitch: TEdit;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Panel4: TPanel;
    ToolBar2: TToolBar;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ImageList1: TImageList;
    edFilename: TEdit;
    SpeedButton1: TSpeedButton;
    edOutputdir: TEdit;
    OpenDialog: TOpenDialog;
    BrowseDir: TJvBrowseForFolderDialog;
    SpeedButton2: TSpeedButton;
    btRequires: TButton;
    ListDirectories: TMemo;
    Panel6: TPanel;
    Panel7: TPanel;
    cbAction: TComboBox;
    procedure btRequiresClick(Sender: TObject);
    procedure edFilenameChange(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure edOutputdirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbActionChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
  public
    procedure RemoveEmptySearchdirs;
  end;

function EditDMakModule(M: TDelphi32Module): Boolean;

implementation

{$R *.dfm}

uses
  {$IFNDEF DELPHI6_UP}
  FileCtrl,
  {$ENDIF ~DELPHI6_UP}  
  delphi32_CompilerSwitch, delphi32_Utils;

resourcestring
  strCompilerswitch = 'Additional compiler directives';
  strCommandLine = 'Command line parameters';

function EditDMakModule(M: TDelphi32Module): Boolean;
var
  I: Integer;
begin
  Result := False;
  with TFormEditModule.Create(nil) do
  try
    edFilename.Text := M.DelphiFilename;
    edOutputdir.Text := M.OutputDir;
    edCompilerSwitch.Text := M.CompilerSwitch;
    cbAction.ItemIndex := Ord(M.DMakAction);
    ListDirectories.Lines.Clear;
    for I := 0 to M.SearchDirs.Count - 1 do
      ListDirectories.Lines.Add(M.SearchDirs[I]);
    if ShowModal = mrOk then
    begin
      RemoveEmptySearchdirs;
      M.DelphiFilename := edFilename.Text;
      M.OutputDir := edOutputdir.Text;
      M.CompilerSwitch := edCompilerSwitch.Text;
      M.DMakAction := TDMakAction(cbAction.ItemIndex);
      M.SearchDirs.Clear;
      for I := 0 to ListDirectories.Lines.Count - 1 do
        M.SearchDirs.Add(ListDirectories.Lines[I]);
      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TFormEditModule.ToolButton9Click(Sender: TObject);
begin
  if DirectoryExists(ExtractFilePath(edFilename.Text)) then
    BrowseDir.Directory := ExtractFilePath(edFilename.Text)
  else begin
    if ListDirectories.SelLength>=0 then
      BrowseDir.Directory := ListDirectories.SelText;
  end;

  if BrowseDir.Execute then
  begin
    ListDirectories.Lines.Add(BrowseDir.Directory);
  end;
end;

procedure TFormEditModule.edOutputdirClick(Sender: TObject);
begin
{  if edOutputdir.Text = '' then
    edOutputdir.InitialDir := ExtractFilePath(edFilename.Text);}
end;

procedure TFormEditModule.FormCreate(Sender: TObject);
var
  I: TDMakAction;
begin
  cbAction.Items.Clear;
  for I := dmaNone to High(stActions) do
    cbAction.Items.Add(stActions[I]);
  cbAction.DropDownCount := cbAction.Items.Count;
end;

procedure TFormEditModule.cbActionChange(Sender: TObject);
begin
  if cbAction.ItemIndex = 9 then
    lbCompilerswitch.Caption := strCommandLine
  else
    lbCompilerswitch.Caption := strCompilerswitch;
end;

procedure TFormEditModule.FormShow(Sender: TObject);
begin
  cbActionChange(Sender);
end;

procedure TFormEditModule.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    edFilename.Text := OpenDialog.FileName;

    if CompareText(ExtractFileExt(edFilename.Text), stExtPackage) = 0 then
    begin
      ListDirectories.Lines.Add(CheckNoBackslash(ExtractFilePath(edFilename.Text)));
      if GetPackageRunOnly(edFilename.Text) then
        cbAction.ItemIndex := Ord(dmaCompile)
      else
        cbAction.ItemIndex := Ord(dmaRegDPK);
    end
    else begin
      if CompareText(ExtractFileExt(edFilename.Text), stExtProject)=0 then begin
        if GetIsProjectExpert(edFilename.Text) then
          cbAction.ItemIndex := Ord(dmaRegisterDllExpert)
        else
          cbAction.ItemIndex := Ord(dmaCompile);
      end;
    end;

  end;
end;

procedure TFormEditModule.SpeedButton2Click(Sender: TObject);
var
  S: string;
  Pt: TPoint;
begin
  Pt := Point(edCompilerSwitch.Left, edCompilerSwitch.Top);
  Pt := ClientToScreen(Pt);
  S := DlgGetCommandlineSwitch(Pt.X, Pt.Y, edCompilerSwitch.Width);
  if S <> '' then
    edCompilerSwitch.Text := edCompilerSwitch.Text + S;
end;

procedure TFormEditModule.edFilenameChange(Sender: TObject);
begin
  btRequires.Enabled := GetIsPackage(edFilename.Text);
end;

procedure TFormEditModule.btRequiresClick(Sender: TObject);
var
  sl: TStringList;
begin
  if GetIsPackage(edFilename.Text) then begin
    sl := TStringList.Create;
    try
      if GetPackageDepencies(edFilename.Text, sl) then
        MessageDlg(sl.Text, mtInformation, [mbOk], 0);
    finally
      sl.Free;
    end;
  end;
end;

procedure TFormEditModule.RemoveEmptySearchdirs;
var
  I: Integer;
begin
  I := 0;
  while I < ListDirectories.Lines.Count do
    if ListDirectories.Lines[I] = '' then
      ListDirectories.Lines.Delete(I)
    else
      Inc(I);
end;

end.
