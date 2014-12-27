unit resources_Actions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, makestudio_TLB, ActiveX, AxCtrls;

type
  TFormActions = class(TForm, IActionCallback)
    ActionList1: TActionList;
    ImageList1: TImageList;
    acCreateRCVersionResource: TAction;
    SaveRCDialog: TSaveDialog;
    procedure acCreateRCVersionResourceExecute(Sender: TObject);
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

uses simplercversioninfo;

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

procedure TFormActions.acCreateRCVersionResourceExecute(Sender: TObject);
begin
  if SaveRCDialog.Execute then begin
    with TRCVersionInfo.Create do
    try
      SetIntFileVersion( 0, 1, 0, 0);
      SetIntProductVersion( 0, 1, 0, 0);
      SavetoFile( SaveRCDialog.FileName);
    finally
      Free;
    end;
  end;
end;

end.
