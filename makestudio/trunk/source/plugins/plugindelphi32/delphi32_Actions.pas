(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: delphi32_Actions.pas

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

unit delphi32_Actions;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, msTLB, ActiveX, AxCtrls;

type
  TForm3 = class(TForm, IActionCallback)
    ActionList1: TActionList;
    ImageList1: TImageList;
    acDelphiSearchPath: TAction;
    acSelectDelphiVersion: TAction;
    acBDSProjectDir: TAction;
    acAddComponentFolder: TAction;
    procedure acDelphiSearchPathExecute(Sender: TObject);
    procedure acSelectDelphiVersionExecute(Sender: TObject);
    procedure acBDSProjectDirExecute(Sender: TObject);
    procedure acAddComponentFolderExecute(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    procedure Execute(const Action: WideString); safecall;
  end;

var
  Form3: TForm3;

procedure GetPictureFromImageList(AImages: TImageList; AIndex: Integer; out APic: Picture);

implementation

{$R *.dfm}

uses
  delphi32_DelphiPath, delphi32_SelectDelphiVersion, delphi32_Utils,
  delphi32_Vars, delphi32_BDSEnvironment, delphi_AddComponentFolder;

procedure GetPictureFromImageList(AImages: TImageList; AIndex: Integer; out APic: Picture);
var
  pic: TPicture;
begin
  pic := TPicture.Create;
  try
    AImages.GetBitmap(AIndex, pic.Bitmap);
    GetOlePicture(pic, APic);
  finally
    pic.Free;
  end;
end;

procedure TForm3.Execute(const Action: WideString);
var
  I: Integer;
begin
  for I := 0 to ActionList1.ActionCount - 1 do
    if CompareText(Action, ActionList1.Actions[I].Name) = 0 then
    begin
      ActionList1.Actions[I].Execute;
    end;
end;

procedure TForm3.acDelphiSearchPathExecute(Sender: TObject);
begin
  DlgSearchPathDelphi;
end;

procedure TForm3.acSelectDelphiVersionExecute(Sender: TObject);
var
  dVersion: TDelphiVersion;
begin
  dVersion := GetDelphiVersion;
  if DlgSelectDelphiVersion(dVersion) then
  begin
    SetDelphiVersion(dVersion);
    WriteDelphiVersionReg;
  end;
end;

procedure TForm3.acAddComponentFolderExecute(Sender: TObject);
begin
  with TFormAddComponentFolder.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TForm3.acBDSProjectDirExecute(Sender: TObject);
begin
  DlgBDSEnvironment( GetDelphiVersion);
end;

end.
