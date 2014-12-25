(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MakeStudioPluginWizardOptions.pas

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

unit msPluginWizardOptions;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs, msPluginWizardCommon;

type
  TmsPluginWizardOptionsForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbMenuActionPath: TLabel;
    Label6: TLabel;
    edPluginName: TEdit;
    edPluginAuthor: TEdit;
    edPluginHint: TEdit;
    edPluginCategory: TEdit;
    edFilesPrefix: TEdit;
    edMenuActionPath: TEdit;
    edCommandName: TEdit;
    cbMenuAction: TCheckBox;
    cbSampleVar: TCheckBox;
    cbSamplePaintCode: TCheckBox;
    OK: TButton;
    Cancel: TButton;
    Label7: TLabel;
    lbCommandTypeName: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    edTestActionCaption: TEdit;
    lbTestActionCaption: TLabel;
    imgCommando: TImage;
    OpenPictureDialog: TOpenPictureDialog;
    imgAction: TImage;
    Label13: TLabel;
    btnLoadCommandoImage: TButton;
    btnLoadActionImage: TButton;
    lbimgAction: TLabel;
    procedure cbMenuActionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edCommandNameChange(Sender: TObject);
    procedure btnLoadActionImageClick(Sender: TObject);
  private
    { Private declarations }
    FWizardKind: TMakeStudioPluginWizardKind;
    procedure LoadPictureWithDialog(AImage: TImage);
    procedure SetWizardKind(AValue: TMakeStudioPluginWizardKind);
  public
    { Public declarations }
    property WizardKind: TMakeStudioPluginWizardKind read FWizardKind write SetWizardKind;
  end;

function GetPluginOptions(AWizardKind: TMakeStudioPluginWizardKind; AParameterList: TStrings;
  ACommandoBitmap, AActionBitmap: TBitmap): Boolean;

implementation

uses wizard_parser;

resourcestring
  StrWrongPictureSize = 'Wrong picture size <> 16x16';

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

function GetPluginOptions(AWizardKind: TMakeStudioPluginWizardKind; AParameterList: TStrings;
  ACommandoBitmap, AActionBitmap: TBitmap): Boolean;
var
  OldDirectory: string;
begin
  with TmsPluginWizardOptionsForm.Create(Application) do
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
      AActionBitmap.Assign(imgAction.Picture.Bitmap);
      with AParameterList do
      begin
        Add(Format('%s=%s', [FilesPrefixParameterName, edFilesPrefix.Text]));
        Add(Format('PLUGINIDENTIFIER=%s', [edFilesPrefix.Text]));
        Add(Format('PLUGINNAME=%s', [edPluginName.Text]));
        Add(Format('PLUGINAUTHOR=%s', [edPluginAuthor.Text]));
        Add(Format('PLUGINHINT=%s', [edPluginHint.Text]));
        Add(Format('PLUGINCATEGORY=%s', [edPluginCategory.Text]));

        Add(Format('BLOCKMENUACTION=%s', [BoolToStr(cbMenuAction.Checked)]));
        if FWizardKind = wkCSharp then
          Add(Format('MENUACTIONPATH=%s', [DoubleSlash(edMenuActionPath.Text)]))
        else
          Add(Format('MENUACTIONPATH=%s', [edMenuActionPath.Text]));
        Add(Format('%s=%s', [TestActionCaptionParameterName, edTestActionCaption.Text]));
        Add(Format('COMMANDNAME=%s', [edCommandName.Text]));
        Add(Format('COMMANDCOMPONENTNAME=%s', [ MakeValidIdent( edCommandName.Text)]));
        Add(Format('COMMANDIDENTIFIER=%s', [lbCommandTypeName.Caption]));
        Add(Format('BLOCKSAMPLEVAR=%s', [BoolToStr(cbSampleVar.Checked)]));
        Add(Format('SAMPLEVARNAME=%s', ['TestEntry']));
        Add(Format('SAMPLEVARVALUE=%s', ['TestValue']));
        Add(Format('BLOCKSAMPLEPAINTCODE=%s', [BoolToStr(cbSamplePaintCode.Checked)]));
      end;
    end;
  finally
    Free;
  end;
end;

procedure TmsPluginWizardOptionsForm.cbMenuActionClick(
  Sender: TObject);
begin
  lbMenuActionPath.Enabled := cbMenuAction.Checked;
  edMenuActionPath.Enabled := cbMenuAction.Checked;
  lbTestActionCaption.Enabled := cbMenuAction.Checked;
  edTestActionCaption.Enabled := cbMenuAction.Checked;
  lbimgAction.Enabled := cbMenuAction.Checked;
  imgAction.Enabled := cbMenuAction.Checked;
  btnLoadActionImage.Enabled := cbMenuAction.Checked;
end;

procedure TmsPluginWizardOptionsForm.FormCreate(Sender: TObject);
begin
  cbMenuActionClick(nil);
  edCommandNameChange(nil);
end;

procedure TmsPluginWizardOptionsForm.edCommandNameChange(
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

procedure TmsPluginWizardOptionsForm.LoadPictureWithDialog(AImage: TImage);
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
        raise Exception.Create(StrWrongPictureSize);
    finally
      Bmp.Free;
    end;
  end;
end;

procedure TmsPluginWizardOptionsForm.btnLoadActionImageClick(
  Sender: TObject);
begin
  if Sender = btnLoadActionImage then
    LoadPictureWithDialog(imgAction)
  else
    LoadPictureWithDialog(imgCommando);
end;

procedure TmsPluginWizardOptionsForm.SetWizardKind(AValue: TMakeStudioPluginWizardKind);
begin
  if FWizardKind <> AValue then
  begin
    FWizardKind := AValue;
    cbMenuActionClick(nil);
    edCommandNameChange(nil);
  end;
end;

end.
