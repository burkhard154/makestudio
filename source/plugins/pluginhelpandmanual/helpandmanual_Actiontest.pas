(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: helpandmanual_Actiontest.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/01  JDuenow   - launched Help & Manual Module
2005/06/20  USchuster - D5 fix

-----------------------------------------------------------------------------*)

unit helpandmanual_Actiontest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,msTLB;

type
  TForm2 = class(TForm, IActionCallback)
    Label1: TLabel;
    Button1: TButton;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
     procedure Execute(const Action: WideString); safecall;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

{ TForm2 }

procedure TForm2.Execute(const Action: WideString);
begin
  //
end;

end.
