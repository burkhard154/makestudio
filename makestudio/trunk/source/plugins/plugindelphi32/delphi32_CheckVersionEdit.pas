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
unit delphi32_CheckVersionEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExStdCtrls, JvCombobox, JvListComb, ImgList,
  delphi32_CheckVersionCommand, delphi32_Vars;

type
  TFormEditDelphi32CheckVersionParams = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    cbVersion: TJvImageComboBox;
    Label2: TLabel;
    cbVar: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgCheckDelphiVersion( Command:TPluginDelphi32CheckVersion):Boolean;

implementation

{$R *.dfm}

function DlgCheckDelphiVersion( Command:TPluginDelphi32CheckVersion):Boolean;
begin
  Result := false;
  with TFormEditDelphi32CheckVersionParams.Create(nil) do
  try
    cbVersion.ItemIndex := Ord( Command.Version);
    cbVar.Text := Command.Variable;
    if ShowModal = mrOk then begin
      Command.Version := TDelphiVersion( cbVersion.ItemIndex);
      Command.Variable := cbVar.Text;
      Result := true;
    end;
  finally
    Free;
  end;
end;

procedure TFormEditDelphi32CheckVersionParams.FormCreate(Sender: TObject);
var i:Integer;
begin
  for i:=0 to MakeStudio.Variables.Count-1 do begin
    cbVar.Items.Add( MakeStudio.Variables.Names[i]);
  end;
end;

end.
