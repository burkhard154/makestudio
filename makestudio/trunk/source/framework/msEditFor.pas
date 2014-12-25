(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msEditFor.pas

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
2005/02/09  BSchranz  - Added Copy, Past, Docking, Debugging
2005/04/09  BSchranz  - Translated to englisch

-----------------------------------------------------------------------------*)

unit msEditFor;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin, msProgram, msGlobals;

type
  TFormEditFor = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    lbContent: TLabel;
    cbVars: TComboBox;
    Label4: TLabel;
    Label1: TLabel;
    edStart: TJvSpinEdit;
    edEnd: TJvSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cbVarsChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgEditFor(M: TFORBlock): Boolean;

implementation

{$R *.dfm}

function DlgEditFor(M: TFORBlock): Boolean;
begin
  with TFormEditFor.Create(nil) do
  try
    Result := False;
    cbVars.Text := M.Varname;
    edStart.Value := M.StartValue;
    edEnd.Value := M.EndValue;
    cbVarsChange(nil);
    if ShowModal = mrOk then
    begin
      Result := True;
      if edStart.Value > edEnd.Value then
        edEnd.Value := edStart.Value;
      M.Varname := cbVars.Text;
      M.StartValue := Round(edStart.Value);
      M.EndValue := Round(edEnd.Value);
    end;
  finally
    Free;
  end;
end;

procedure TFormEditFor.FormCreate(Sender: TObject);
begin
  Varhandler.GetVarList(cbVars.Items);
end;

procedure TFormEditFor.cbVarsChange(Sender: TObject);
begin
  if Varhandler.VarExists(cbVars.Text) then
    lbContent.Caption := Varhandler.FindXVar(cbVars.Text).AsString
  else
    lbContent.Caption := '';
end;

end.
