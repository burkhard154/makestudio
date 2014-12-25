(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msEditWhile.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/20  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in
2005/04/09  BSchranz  - Translated to englisch

-----------------------------------------------------------------------------*)

unit msEditWhile;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, msGlobals, msProgram;

type
  TFormEditWhile = class(TForm)
    cbVars: TComboBox;
    Label1: TLabel;
    edValue: TEdit;
    lbContent: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    cbCondition: TComboBox;
    Label5: TLabel;
    btOk: TButton;
    btCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cbVarsChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgEditWhile(M: TWHILEBlock): Boolean;

implementation

{$R *.dfm}

function DlgEditWhile(M: TWHILEBlock): Boolean;
begin
  with TFormEditWhile.Create(nil) do
  try
    Result := False;
    cbVars.Text := M.Varname;
    cbCondition.ItemIndex := Ord(M.Condition);
    edValue.Text := M.VarValue;
    cbVarsChange(nil);
    if ShowModal = mrOk then
    begin
      Result := True;
      M.Varname := cbVars.Text;
      M.Condition := TBlockCondition1(cbCondition.ItemIndex);
      M.VarValue := edValue.Text;
    end;
  finally
    Free;
  end;
end;

procedure TFormEditWhile.FormCreate(Sender: TObject);
begin
  Varhandler.GetVarList(cbVars.Items);
end;

procedure TFormEditWhile.cbVarsChange(Sender: TObject);
begin
  if Varhandler.VarExists(cbVars.Text) then
    lbContent.Caption := Varhandler.FindXVar(cbVars.Text).AsString
  else
    lbContent.Caption := '';
end;

end.

