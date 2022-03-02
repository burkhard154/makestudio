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
unit mshelpmergerRun;

interface
{$I jedi.inc}

uses
  Windows, Messages, SysUtils,
  {$IFDEF DELPHI6_UP}
  Variants, Types,
  {$ENDIF DELPHI6_UP}
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, mshelpmerger;

type
  TFormHelpmergerRun = class(TForm)
    Panel1: TPanel;
    Timer1: TTimer;
    edlog: TMemo;
    btCancel: TButton;
    procedure btCancelClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FMerger : THelpMerger;
    FRunning : Boolean;
    procedure OnCapture(aLine: String; var aAbort: Boolean);
  public
    { Public-Deklarationen }
  end;

procedure DlgHelpMergerRun( aMerger:THelpMerger);

implementation

{$R *.dfm}

uses System.UITypes;
resourcestring
  strClose = 'Close';
  strNeedCompile = 'The help system has been modified and will be recompiled...';

procedure DlgHelpMergerRun( aMerger:THelpMerger);
begin
  if aMerger.NeedCompile then
    with TFormHelpMergerRun.Create( nil) do
    try
      FRunning := false;
      FMerger := aMerger;
      FMerger.OnCaptureOutput := OnCapture;
      ShowModal;
      FMerger.OnCaptureOutput := nil;
    finally
      Free;
    end;
end;

procedure TFormHelpmergerRun.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  MessageDlg( strNeedCompile, mtInformation, [mbOK], 0);
  FRunning := true;
  if FMerger<>nil then
    if FMerger.Execute then begin
      FRunning := false;
      btCancel.Caption := strClose;
    end
    else begin

      Close;
    end;
end;

procedure TFormHelpmergerRun.OnCapture(aLine: String; var aAbort: Boolean);
begin
  edlog.Lines.Add( aLine);
  aAbort := not FRunning;
end;

procedure TFormHelpmergerRun.btCancelClick(Sender: TObject);
begin
  if FRunning then
    FRunning := false
  else
    Close;
end;

end.
