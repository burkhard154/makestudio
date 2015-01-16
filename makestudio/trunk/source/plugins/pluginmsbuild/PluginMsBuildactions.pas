unit PluginMsBuildactions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, makestudio_TLB, ActiveX, AxCtrls, System.Actions;

type
  TFormActions = class(TForm, IActionCallback)
    ActionList1: TActionList;
    ImageList1: TImageList;
    acSettings: TAction;
    procedure acSettingsExecute(Sender: TObject);
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
  PluginMsBuildActiontest, PluginMsBuildvars;



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

procedure TFormActions.acSettingsExecute(Sender: TObject);
begin
  with TFormActionTest.Create(nil) do
  try
    tbMSBuildExe.Text := gMSBuildExe;
    if ShowModal = mrOk then
    begin
      gMSBuildExe := Trim(tbMSBuildExe.Text);
      SaveToRegistry;
    end;
  finally
    Free;
  end;
end;

end.
