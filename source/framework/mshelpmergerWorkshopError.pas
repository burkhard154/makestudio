(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msProgram.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:                                                                                    }

2006/05/28  BSchranz  - Help Merger Added

-----------------------------------------------------------------------------*)
unit mshelpmergerWorkshopError;

interface
{$I jedi.inc}

uses
  Windows, Messages, SysUtils,
  {$IFDEF DELPHI6_UP}
  Variants, Types,
  {$ENDIF DELPHI6_UP}
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellApi, JvExControls, JvLinkLabel;

type
  TFormHelpWorkshopError = class(TForm)
    GroupBox1: TGroupBox;
    StaticText1: TStaticText;
    btOk: TButton;
    Label1: TLabel;
    procedure Label1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

procedure DlgHelpWorkshopNotInstalled;

implementation

{$R *.dfm}

procedure DlgHelpWorkshopNotInstalled;
begin
  with TFormHelpWorkshopError.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormHelpWorkshopError.Label1Click(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://go.microsoft.com/fwlink/p/?linkid=14188',
    nil, nil, SW_SHOWNORMAL);
end;

end.
