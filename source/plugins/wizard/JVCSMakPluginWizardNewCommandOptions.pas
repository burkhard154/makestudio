(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSMakPluginWizardOptions.pas

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/02/15  USchuster - new unit
2005/03/12  USchuster - changes for C# Builder wizard

-----------------------------------------------------------------------------*)

unit JVCSMakPluginWizardNewCommandOptions;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs, JVCSMakPluginWizardCommon;

type
  TJVCSMakePluginWizardNewCommandForm = class(TForm)
    OpenPictureDialog: TOpenPictureDialog;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    cbAddMPLHeader: TCheckBox;
    Panel4: TPanel;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    lbCommandTypeName: TLabel;
    Label9: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label10: TLabel;
    lbCSharpNY2: TLabel;
    imgCommando: TImage;
    Label13: TLabel;
    edCommandName: TEdit;
    btnLoadCommandoImage: TButton;
    cbSampleVar: TCheckBox;
    cbSamplePaintCode: TCheckBox;
    Cancel: TButton;
    OK: TButton;
    Label1: TLabel;
    edFilesPrefix: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure edCommandNameChange(Sender: TObject);
    procedure btnLoadActionImageClick(Sender: TObject);
  private
    { Private declarations }
    FWizardKind: TJVCSMakPluginWizardKind;
    procedure LoadPictureWithDialog(AImage: TImage);
    procedure SetWizardKind(AValue: TJVCSMakPluginWizardKind);
  public
    { Public declarations }
    property WizardKind: TJVCSMakPluginWizardKind read FWizardKind write SetWizardKind;
  end;

function GetNewCommandOptions(AWizardKind: TJVCSMakPluginWizardKind; AParameterList: TStrings;
  ACommandoBitmap: TBitmap): Boolean;

implementation

uses wizard_parser;

{$R *.dfm}

{$IFNDEF D5TODO}
{$IFDEF DELPHI5}
function BoolToStr(ABoolean: Boolean): string;
begin
  if ABoolean then
    Result := '1'
  else
    Result := '0';
end;
{$ENDIF DELPHI5}
{$ENDIF ~D5TODO}

//USc 12.03.2005 function copied code from jvcssrv25 source
// move to JVCSFunctions.pas or JclStrings.pas?
function DoubleSlash(S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] <> '\' then
      Result := Result + S[I]
    else
      Result := Result + S[I] + S[I];
end;

function GetNewCommandOptions(AWizardKind: TJVCSMakPluginWizardKind; AParameterList: TStrings;
  ACommandoBitmap: TBitmap): Boolean;
var
  OldDirectory: string;
begin
  with TJVCSMakePluginWizardNewCommandForm.Create(Application) do
  try
    OldDirectory := GetCurrentDir;
    try
      WizardKind := AWizardKind;
      Result := ShowModal = mrOK;
    finally
      SetCurrentDir(OldDirectory);
    end;
    if Result then
    begin
      ACommandoBitmap.Assign(imgCommando.Picture.Bitmap);
      with AParameterList do
      begin
        Add(Format('PLUGINIDENTIFIER=%s', [edFilesPrefix.Text]));
        Add(Format('COMMANDCOMPONENTNAME=%s', [ MakeValidIdent( edCommandName.Text)]));
        Add(Format('COMMANDNAME=%s', [edCommandName.Text]));
        Add(Format('COMMANDIDENTIFIER=%s', [lbCommandTypeName.Caption]));
        Add(Format('BLOCKSAMPLEVAR=%s', [BoolToStr(cbSampleVar.Checked)]));
        Add(Format('SAMPLEVARNAME=%s', ['TestEntry']));
        Add(Format('SAMPLEVARVALUE=%s', ['TestValue']));
        Add(Format('BLOCKSAMPLEPAINTCODE=%s', [BoolToStr(cbSamplePaintCode.Checked)]));
        Add(Format('BLOCKHEADER=%s', [BoolToStr(cbAddMPLHeader.Checked)]));
      end;
    end;
  finally
    Free;
  end;
end;

procedure TJVCSMakePluginWizardNewCommandForm.FormCreate(Sender: TObject);
begin
  edCommandNameChange(nil);
end;

procedure TJVCSMakePluginWizardNewCommandForm.edCommandNameChange(
  Sender: TObject);
var
  S: string;
  I: Integer;
begin
  S := '';
  for I := 1 to Length(edCommandName.Text) do
    if edCommandName.Text[I] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
      S := S + edCommandName.Text[I];
  lbCommandTypeName.Caption := S;
  if FWizardKind = wkDelphiWin32VCL then
  begin
    Label11.Caption := Format('TPlugin%s', [S]);
    Label12.Caption := Format('TPlugin%sCallback', [S]);
  end
  else
  if FWizardKind = wkCSharp then
  begin
    Label11.Caption := Format('Plugin%s', [S]);
    Label12.Caption := Format('Plugin%sCallback', [S]);
  end;
end;

procedure TJVCSMakePluginWizardNewCommandForm.LoadPictureWithDialog(AImage: TImage);
var
  Bmp: TBitmap;
begin
  if OpenPictureDialog.Execute then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromFile(OpenPictureDialog.FileName);
      if (Bmp.Width = 16) and (Bmp.Height = 16) then
        AImage.Picture.Assign(Bmp)
      else
        raise Exception.Create('Wrong Size <> 16x16');
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TJVCSMakePluginWizardNewCommandForm.btnLoadActionImageClick(
  Sender: TObject);
begin
  LoadPictureWithDialog(imgCommando);
end;

procedure TJVCSMakePluginWizardNewCommandForm.SetWizardKind(AValue: TJVCSMakPluginWizardKind);
begin
  if FWizardKind <> AValue then
  begin
    FWizardKind := AValue;
    lbCSharpNY2.Visible := FWizardKind = wkCSharp;
    edCommandNameChange(nil);
  end;
end;

end.
