(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dialogs_Actions.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/04  JDuenow   - launched Dialogs Module

-----------------------------------------------------------------------------*)

unit dialogs_Actions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, makestudio_TLB, ActiveX, AxCtrls, ExtCtrls,
  dialogs_Vars;

type
  TForm3 = class(TForm, IActionCallback)
    ImageList1: TImageList;
    ActionList1: TActionList;
    acTestaction1: TAction;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    procedure acTestaction1Execute(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    procedure Execute(const Action: WideString); safecall;
  end;

var
  Form3: TForm3;

procedure GetPictureFromImageList(aImages: TImageList; aIndex: Integer; out apic: Picture);

implementation

{$R *.dfm}

uses
  dialogs_Actiontest, dialogs_MsgBox;

procedure GetPictureFromImageList(aImages: TImageList; aIndex: Integer; out apic: Picture);
var
  pic: TPicture;
begin
  apic := nil;
  pic := TPicture.Create;
  try
    aImages.GetBitmap(aIndex, pic.Bitmap);
    GetOlePicture(pic, apic);
  finally
    pic.Free;
  end;
end;

procedure TForm3.Execute(const Action: WideString);
var
  I: Integer;
begin
  for I := 0 to ActionList1.ActionCount - 1 do
    if CompareText(Action, ActionList1.Actions[I].Name)=0 then
      ActionList1.Actions[I].Execute;
end;

procedure TForm3.acTestaction1Execute(Sender: TObject);
begin
  with TForm2.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
