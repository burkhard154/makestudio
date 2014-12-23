(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: delphi32_CompilerSwitch.pas

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

unit delphi32_CompilerSwitch;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFormCompilerSwitches = class(TForm)
    lb: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure lbClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function DlgGetCommandlineSwitch(ALeft, ATop, AWidth: Integer): string;

implementation

{$R *.dfm}

function DlgGetCommandlineSwitch(ALeft, ATop, AWidth: Integer): string;
begin
  Result := '';
  with TFormCompilerSwitches.Create(nil) do
  try
    Left := ALeft;
    Top  := ATop;
    Width := AWidth;
    if ShowModal = mrOk then
    begin
      if lb.ItemIndex>-1 then
        Result := lb.Items[lb.ItemIndex];
    end;
  finally
  end;
end;

procedure TFormCompilerSwitches.lbClick(Sender: TObject);
begin
  Close;
  ModalResult := mrOk;
end;

end.
