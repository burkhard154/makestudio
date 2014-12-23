(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsplugintemplate_Actions.pas

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
unit nsis_Actions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, msTLB, ActiveX, AxCtrls;

type
  TFormActions = class(TForm, IActionCallback)
    ActionList1: TActionList;
    ImageList1: TImageList;
    acTestaction1: TAction;
  private
    { Private-Deklarationen }
  public
    procedure Execute(const Action: WideString); safecall;
  end;

var
  FormActions: TFormActions;

procedure GetPictureFromImageList(AImages: TImageList; AIndex: Integer; out APic: Picture);

implementation

{$R *.dfm}


procedure GetPictureFromImageList(AImages: TImageList; AIndex: Integer; out APic: Picture);
var
  pic: TPicture;
begin
  APic := nil;
  pic := TPicture.Create;
  try
    AImages.GetBitmap(AIndex, pic.Bitmap);
    GetOlePicture(pic, APic);
  finally
    pic.Free;
  end;
end;                 

procedure TFormActions.Execute(const Action: WideString);
var
  I: Integer;
begin
  for I := 0 to ActionList1.ActionCount - 1 do
    if CompareText(Action, ActionList1.Actions[I].Name) = 0 then
    begin
      ActionList1.Actions[I].Execute;
    end;
end;

end.
 
