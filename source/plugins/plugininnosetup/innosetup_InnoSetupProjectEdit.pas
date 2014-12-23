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

2005/01/08  BSchranz  - Plugin template created
2005/02/15  USchuster - preparations for check in and modified for Wizard

-----------------------------------------------------------------------------*)
unit innosetup_InnoSetupProjectEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExControls, JvComponent, JvEditorCommon, JvEditor,
  JvExStdCtrls, JvEdit, Mask, JvExMask, JvToolEdit;

type
  TFormEditInnoSetupProjectParams = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    edFilename: TJvFilenameEdit;
    Label2: TLabel;
    edOutputfilename: TJvFilenameEdit;
    edOptions: TJvEdit;
    Label3: TLabel;
    JvEditor1: TJvEditor;
    btCompiler: TButton;
    btEdit: TButton;
    OpenDialog1: TOpenDialog;
    procedure btEditClick(Sender: TObject);
    procedure btCompilerClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormEditInnoSetupProjectParams: TFormEditInnoSetupProjectParams;

implementation

{$R *.dfm}

uses innosetup_InnoSetupProjectCommand;

procedure TFormEditInnoSetupProjectParams.btEditClick(Sender: TObject);
begin
  LoadInnoSetupScript( edFilename.Text);
end;

procedure TFormEditInnoSetupProjectParams.btCompilerClick(Sender: TObject);
begin
  OpenDialog1.FileName := GetInnoSetupCompiler;
  if OpenDialog1.Execute then
    SetInnoSetupCompiler( OpenDialog1.Filename);
end;

end.
