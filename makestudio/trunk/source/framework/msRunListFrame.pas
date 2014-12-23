(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ModuleListFrame.pas

The Initial Developer of the original DMAK-Code is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
Code move to JEDI VCS:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to JVCSMAK
2003/11/28  USchuster - 2nd Migrationstep (fixed header and removed Variants)
2003/12/05  USchuster - re-formatted
                      - minor cleanup
2004/02/24  USchuster - Update to JVCL 3.0 / JCL 1.90 (mantis #1353)
2005/01/02  BSchranz  - Migration to JVCSMak with external plugins
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit jvcsmak_RunListFrame;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ImgList, StdCtrls, ExtCtrls, JvComponent, JvDragDrop,
  jvcsmak_TLB;

type
  TFrameModuleList = class(TFrame)
    Panel8: TPanel;
    ModuleListBox: TListBox;
    Panel5: TPanel;
    JvDragDrop1: TJvDragDrop;
    procedure ModuleListBoxDblClick(Sender: TObject);
    procedure ModuleListBoxMeasureItem(Control: TWinControl;
      Index: Integer; var Height: Integer);
    procedure ModuleListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure JvDragDrop1Drop(Sender: TObject; Pos: TPoint;
      Value: TStrings);
  private
    { Private-Deklarationen }
  public
    procedure FillList;
  end;

implementation

uses
  jvcsmak_Main, jvcsmak_Globals, jvcsmak_Program;

{$R *.dfm}

procedure TFrameModuleList.ModuleListBoxDblClick(Sender: TObject);
begin
  FormMain.acEditModule.Execute;
end;

procedure TFrameModuleList.FillList;
var
  I: Integer;
begin
  ModuleListBox.Items.Clear;
  for I := 0 to Programhandler.Count - 1 do
    ModuleListBox.Items.Add(DateTimeToStr(Now));
end;

procedure TFrameModuleList.ModuleListBoxMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Height := TCommand(Programhandler[Index]).GetDrawHeight(TListBox(Control).Canvas);
end;

procedure TFrameModuleList.ModuleListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  I: Integer;
begin
  if Programhandler.IsRun and TCommand(Programhandler[Index]).Breakpoint then
    I := 2
  else
  if not(Programhandler.IsRun) and TCommand(Programhandler[Index]).Breakpoint then
    I := 1
  else
    I := 0;
  TCommand(Programhandler[Index]).Draw(TListBox(Control).Canvas, Rect, State, I);
end;

procedure TFrameModuleList.JvDragDrop1Drop(Sender: TObject; Pos: TPoint;
  Value: TStrings);
var
  M: TCommand;
  m1: TCommandTypeItem;
  I: Integer;
begin
  with Programhandler do
  begin
    for I := 0 to Value.Count - 1 do
    begin
      m1 := CommandTypes.GetItemByExtension(ExtractFileExt(Value[I]));
      if m1 <> nil then
      begin
        M := TCommand.Create(m1, Programhandler);
        M.SetFilename(Value[I]);
        Add(M);
        ModuleListBox.Items.Add(DateTimeToStr(Now));
        Programhandler.Modified := True;
        Break;
      end;
    end;
  end;
end;

end.
