unit PlginEmailactions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, makestudio_TLB, ActiveX, AxCtrls, System.Actions;

type
  TFormActions = class(TForm, IActionCallback)
    ImageList1: TImageList;
    ActionList1: TActionList;
    acServerSettings: TAction;
    procedure acTestaction1Execute(Sender: TObject);
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

uses
  emailsettings;

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

procedure TFormActions.acTestaction1Execute(Sender: TObject);
begin
  DlgEditEmailSettings;
end;

end.
