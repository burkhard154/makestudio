(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsplugintemplate_Edit.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2006/04/21  BSchranz  - Plugin ZIP created

-----------------------------------------------------------------------------*)
unit zip_CommandEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, zip_utils, zip_vars, ExtCtrls;

type
  TFormEditZipCommand = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    cbAction: TComboBox;
    Label2: TLabel;
    edZIPFilename: TJvFilenameEdit;
    lbParams: TLabel;
    edParams: TMemo;
    cbVerbose: TCheckBox;
    cbTrace: TCheckBox;
    rgZipUpdate: TRadioGroup;
    procedure FormShow(Sender: TObject);
    procedure cbActionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;


implementation

resourcestring
  StrExtractFileToThe = 'Extract File to the following Folder';
  StrZipFilesOfTheF = 'Zip File(s) of the following Folder (recourse)';
  StrZipFilesWildcard = 'Zip File(s) of the following file mask';
  StrZipFilesWildcardRecourse = 'Zip File(s) of the following file mask (rec' +
  'ourse)';

{$R *.dfm}

procedure TFormEditZipCommand.FormCreate(Sender: TObject);
var i:TZIPAction;
begin
  cbAction.Clear;
  for i:=Low( TZIPAction) to High( TZIPAction) do
    cbAction.Items.Add( ZipActionStrings[ i]);
end;

procedure TFormEditZipCommand.cbActionChange(Sender: TObject);
begin
  case TZIPAction( cbAction.ItemIndex) of
    tzaUnzipToFolder :
      begin
        lbParams.Caption := StrExtractFileToThe;
        edZIPFilename.DialogKind := dkOpen;
        rgZipUpdate.Enabled := false;
      end;
    tzaZipFolder :
      begin
        lbParams.Caption := StrZipFilesOfTheF;
        edZIPFilename.DialogKind := dkSave;
        rgZipUpdate.Enabled := true;
      end;
    tzaZipWildcard :
      begin
        lbParams.Caption := StrZipFilesWildcard;
        edZIPFilename.DialogKind := dkSave;
        rgZipUpdate.Enabled := true;
      end;
    tzaZipWildcardRecurse :
      begin
        lbParams.Caption := StrZipFilesWildcardRecourse;
        edZIPFilename.DialogKind := dkSave;
        rgZipUpdate.Enabled := true;
      end;
  end;
end;

procedure TFormEditZipCommand.FormShow(Sender: TObject);
begin
  cbActionChange(Sender);
end;

end.
