(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: resources_MakeStudioPlugin.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/08/13  BSchranz  - Plugin created
2005/08/16  USchuster - fixed compilation over makefile
2005/09/09  BSchranz  - translation bug fixed
2005/11/12  USchuster - reintegrated changes from revision 0.2(2005/08/16)
2006/04/11  BSchranz  - Version also set to "FileVersion" Key

-----------------------------------------------------------------------------*)
unit resources_Edit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Mask, JvExMask, JvToolEdit, ComCtrls;

type
  TFormEditParams = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edFilename: TJvFilenameEdit;
    rgType: TRadioGroup;
    GroupBox1: TGroupBox;
    edVersionKey: TEdit;
    rgIncSet: TRadioGroup;
    grpSet: TGroupBox;
    edtMainVer: TEdit;
    edtMinorVer: TEdit;
    edtRel: TEdit;
    edtBuild: TEdit;
    updoMain: TUpDown;
    updoMinor: TUpDown;
    updoRel: TUpDown;
    updoBuild: TUpDown;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure rgIncSetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FormEditParams: TFormEditParams;

implementation

{$R *.dfm}

procedure TFormEditParams.FormShow(Sender: TObject);
begin
  rgIncSetClick( Sender);
end;

procedure TFormEditParams.rgIncSetClick(Sender: TObject);
begin
  grpSet.Enabled := rgIncSet.ItemIndex = 0;
  edtMainVer.Enabled := grpSet.Enabled;
  edtMinorVer.Enabled := grpSet.Enabled;
  edtRel.Enabled := grpSet.Enabled;
  edtBuild.Enabled := grpSet.Enabled;

  rgType.Enabled := rgIncSet.ItemIndex = 1;
end;

end.
