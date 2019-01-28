(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: delphi32_BDSEnvironment.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit delphi32_BDSEnvironment;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvToolEdit, JvExMask, JclFileUtils, ImgList,
  JvExStdCtrls, JvCombobox, JvListComb, delphi32_vars, JvMaskEdit, System.ImageList;

type
  TFormSelectBDSEnvironment = class(TForm)
    Memo: TMemo;
    Button1: TButton;
    Button2: TButton;
    GroupBox4: TGroupBox;
    cbVer: TJvImageComboBox;
    d5: TCheckBox;
    d6: TCheckBox;
    d7: TCheckBox;
    d2005: TCheckBox;
    d2006: TCheckBox;
    d2007: TCheckBox;
    d2009: TCheckBox;
    ImageList1: TImageList;
    d2010: TCheckBox;
    dXE: TCheckBox;
    edProjectDir: TJvDirectoryEdit;
    lbProjectsDir: TLabel;
    lbCommonDir: TLabel;
    edCommonDir: TJvDirectoryEdit;
    edUserDir: TJvDirectoryEdit;
    lbUserDir: TLabel;
    lbLibDir: TLabel;
    edLibDir: TJvDirectoryEdit;
    edLangDir: TJvMaskEdit;
    lbLangDir: TLabel;
    dXE2: TCheckBox;
    cbPlatform: TJvImageComboBox;
    Label1: TLabel;
    dxe3: TCheckBox;
    dxe4: TCheckBox;
    dxe5: TCheckBox;
    dxe6: TCheckBox;
    dxe7: TCheckBox;
    dxe8: TCheckBox;
    dD10S: TCheckBox;
    dD101B: TCheckBox;
    dD102T: TCheckBox;
    dD103R: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbVerChange(Sender: TObject);
    procedure cbPlatformChange(Sender: TObject);
  private
    fPath: TStringList;
  public
    constructor Create( AOwner:TComponent); override;
    destructor Destroy; override;

    procedure FillDialog;
    procedure GetPathList;
  end;

function DlgBDSEnvironment( dVersion:TDelphiVersion): Boolean;

implementation

{$R *.dfm}

uses
  {$IFNDEF D5TODO}
  {$IFDEF DELPHI5}
  JclSysInfo,
  {$ENDIF DELPHI5}
  {$ENDIF ~D5TODO}
  delphi32_Utils;

{$IFNDEF D5TODO}
{$IFDEF DELPHI5}
function GetEnvironmentVariable(const Name: string): string; overload;
begin
  if not GetEnvironmentVar(Name, Result) then
    Result := '';
end;
{$ENDIF DELPHI5}
{$ENDIF ~D5TODO}

function DlgBDSEnvironment( dVersion:TDelphiVersion): Boolean;
begin
  Result := false;
  with TFormSelectBDSEnvironment.Create(nil) do
  try
    case dVersion of
      dver5: cbVer.ItemIndex := 0;
      dver6: cbVer.ItemIndex := 1;
      dver7: cbVer.ItemIndex := 2;
      dver2005: cbVer.ItemIndex := 3;
      dver2006: cbVer.ItemIndex := 4;
      dver2007: cbVer.ItemIndex := 5;
      dver2009: cbVer.ItemIndex := 6;
      dver2010: cbVer.ItemIndex := 7;
      dverXE: cbVer.ItemIndex := 8;
      dverXE2: cbVer.ItemIndex := 9;
      dverXE3: cbVer.ItemIndex := 10;
      dverXE4: cbVer.ItemIndex := 11;
      dverXE5: cbVer.ItemIndex := 12;
      dverXE6: cbVer.ItemIndex := 13;
      dverXE7: cbVer.ItemIndex := 14;
      dverXE8: cbVer.ItemIndex := 15;
      dverD10S: cbVer.ItemIndex := 16;
      dverD101B: cbVer.ItemIndex := 17;
      dverD102T: cbVer.ItemIndex := 18;
      dverD103R: cbVer.ItemIndex := 19;
    end;
    if ShowModal = mrOk then begin
      Result := cbVer.ItemIndex > -1;
    end;
  finally
    Free;
  end;
end;

procedure TFormSelectBDSEnvironment.FormCreate(Sender: TObject);
begin
  FillDialog;
end;

procedure TFormSelectBDSEnvironment.Button1Click(Sender: TObject);
var
  S: string;
  path: string;
  I: Integer;
  found: Boolean;
begin

  S := PathRemoveSeparator( GetDelphiBPLPath);
  found := False;
  for I := 0 to fPath.Count - 1 do
    if SameText(S, fPath[I]) then
    begin
      found := True;
      Break;
    end;

  if not found then
  begin
    if MessageDlg(stdBPLDirNotInPath, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      path := GetEnvironmentVariable('PATH');
      if path[Length(path)] <> ';' then
        path := path + ';';
      path := path + S + ';';
      SetEnvironmentVariable('PATH', PChar(path));

      path := GetSystemEnvironmentVariable('PATH');
      if path[Length(path)] <> ';' then
        path := path + ';';
      path := path + S + ';';
      SetSystemEnvironmentVariable('PATH', path);
    end;
  end;

  Close;
  ModalResult := mrOk;
end;

procedure TFormSelectBDSEnvironment.FillDialog;
begin
  Memo.Lines.Clear;

  Memo.Lines.Add( stdActualDelphiVersion);
  Memo.Lines.Add( stdRootDirectory);
  Memo.Lines.Add( stdBPLDirectory);
  Memo.Lines.Add( stdDCPDirectory);
  Memo.Lines.Add( stdRegistryKey);

  Memo.Lines.Text := Format(Memo.Lines.Text,
    [GetVersionText, GetDelphiRootPathLong, GetDelphiBPLPath,
    GetDelphiDCPPath, GetDelphiRootKey]);

  Memo.Lines.Add( Format( stdLibDir, [GetBDSLIB]));
  Memo.Lines.Add( Format( stdBinDir, [GetBDSBIN]));
  Memo.Lines.Add( Format( stdPlatform, [GetPlatformString]));
  Memo.Lines.Add( Format( stdLangDir, [GetLANGDIR]));

  edCommonDir.Visible := False;
  lbCommonDir.Visible := false;
  edUserDir.Visible := False;
  lbUserDir.Visible := false;
  edLibDir.Visible := false;
  lbLibDir.Visible := false;
  edLangDir.Visible := false;
  lbLangDir.Visible := false;
  cbPlatform.Enabled := false;
  cbPlatform.ItemIndex := Ord( GetCompilerPlatform);

  if GetDelphiVersion < dver2005 then
  begin
    Button1.Enabled := False;
    edProjectDir.Enabled := False;
  end
  else begin
    Button1.Enabled := true;
    edProjectDir.Enabled := true;
    edProjectDir.Text := GetBDSProjectsPath;
    if GetDelphiVersion > dver2006 then begin
      edCommonDir.Visible := true;
      lbCommonDir.Visible := true;
      edUserDir.Visible := true;
      lbUserDir.Visible := true;
      edCommonDir.Text := GetBDSCommonPath;
      edUserDir.Text := GetBDSUserPath;

      if GetDelphiVersion > dver2010 then begin
        edLibDir.Text := GetBDSLIB;
        edLangDir.Text := GetLANGDIR;

        edLibDir.Visible := true;
        lbLibDir.Visible := true;
        edLangDir.Visible := true;
        lbLangDir.Visible := true;
      end;

      if GetDelphiVersion > dverXE then begin
        cbPlatform.Enabled := true;
      end;
    end;
  end;

  d5.Checked := CheckDelphiVersion(dver5);
  d6.Checked := CheckDelphiVersion(dver6);
  d7.Checked := CheckDelphiVersion(dver7);
  d2005.Checked := CheckDelphiVersion(dver2005);
  d2006.Checked := CheckDelphiVersion(dver2006);
  d2007.Checked := CheckDelphiVersion(dver2007);
  d2009.Checked := CheckDelphiVersion(dver2009);
  d2010.Checked := CheckDelphiVersion(dver2010);
  dXE.Checked := CheckDelphiVersion(dverXE);
  dXE2.Checked := CheckDelphiVersion(dverXE2);
  dXE3.Checked := CheckDelphiVersion(dverXE3);
  dXE4.Checked := CheckDelphiVersion(dverXE4);
  dXE5.Checked := CheckDelphiVersion(dverXE5);
  dXE6.Checked := CheckDelphiVersion(dverXE6);
  dXE7.Checked := CheckDelphiVersion(dverXE7);
  dXE8.Checked := CheckDelphiVersion(dverXE8);
  dD10S.Checked := CheckDelphiVersion(dverD10S);
  dD101B.Checked := CheckDelphiVersion(dverD101B);
  dD102T.Checked := CheckDelphiVersion(dverD102T);
  dD103R.Checked := CheckDelphiVersion(dverD103R);

  GetPathList;
end;

procedure TFormSelectBDSEnvironment.GetPathList;
var
  I: Integer;
begin

  fPath.Clear;
  fPath.Text := StringReplace(GetEnvironmentVariable('PATH'), ';', #10, [rfReplaceAll]);
  for I := 0 to fPath.Count - 1 do
    fPath[I] := PathRemoveSeparator(ExpandFilename(fPath[I]));

  Memo.Lines.Add( '');
  Memo.Lines.Add( stdSearchPath);
  Memo.Lines.AddStrings(fPath);
end;

constructor TFormSelectBDSEnvironment.Create(AOwner: TComponent);
begin
  inherited;
  fPath := TStringList.Create;
end;

destructor TFormSelectBDSEnvironment.Destroy;
begin
  fPath.Free;
  inherited;
end;

procedure TFormSelectBDSEnvironment.cbPlatformChange(Sender: TObject);
begin
  SetCompilerPlatform( TCompilerPlatform( cbPlatform.ItemIndex));
  FillDialog;
end;

procedure TFormSelectBDSEnvironment.cbVerChange(Sender: TObject);
begin
  SetDelphiVersion( TDelphiVersion( cbVer.ItemIndex));
  FillDialog;
end;

end.
