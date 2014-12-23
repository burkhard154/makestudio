(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msfrmrunlistlistbox.pas

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

2005/26/08  BSchranz  - Created
2005/09/02  BSchranz  - Added Copy, Past, Docking, Debugging
2005/09/04  BSchranz  - Translated to englisch
2005/09/04  USchuster - D5 fix
2005/09/12  USchuster - another D5 fix and minor style cleaning
2006/02/05  BSchranz  - Include Support Added

-----------------------------------------------------------------------------*)

unit msFrmRunListListbox;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, msglobals, JvComponent, JvDragDrop, StdCtrls, ExtCtrls,
  msprogram, JvEmbeddedForms, ImgList, Menus, ActnList, msutils,
  JvComponentBase;

type
  TFormRunlistLisbox = class(TForm, IProgramNotification)
    Panel8: TPanel;
    ModuleListBox: TListBox;
    Link: TJvEmbeddedFormLink;
    PopupList: TPopupMenu;
    ImageList1: TImageList;
    mnCopy: TMenuItem;
    mnCut: TMenuItem;
    mnPaste: TMenuItem;
    mnDelete: TMenuItem;
    N1: TMenuItem;
    mnAdd: TMenuItem;
    N2: TMenuItem;
    mnEdit: TMenuItem;
    ActionList1: TActionList;
    PanelInclude: TPanel;
    Label1: TLabel;
    ImageInclude: TImage;
    procedure FormDestroy(Sender: TObject);
    procedure mnPasteClick(Sender: TObject);
    procedure mnCutClick(Sender: TObject);
    procedure mnCopyClick(Sender: TObject);
    procedure PopupListPopup(Sender: TObject);
    procedure mnDeleteClick(Sender: TObject);
    procedure mnEditClick(Sender: TObject);
    procedure ModuleListBoxClick(Sender: TObject);
    procedure ModuleListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ModuleListBoxDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ModuleListBoxDblClick(Sender: TObject);
    procedure ModuleListBoxMeasureItem(Control: TWinControl;
      Index: Integer; var Height: Integer);
    procedure ModuleListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    fExecBitmap: TBitmap;
    fExecPosition: Integer;
  protected
    //Start/Stop/Debug
    procedure OnBeforeStartProgram; stdcall;
    procedure OnAfterStopProgram; stdcall;
    procedure OnProgramPaused; stdcall;
    procedure OnSetProgramPosition(const ItemIndex: Integer); stdcall;
    procedure OnSetProgress(const ACaption: string; const ProgressMin, ProgressMax,
    ProgressPosition: Integer); stdcall;

    //List Notifications
    procedure OnClearItems; stdcall;
    procedure OnRefresh; stdcall;
    procedure OnAddItem(const ItemIndex: Integer); stdcall;
    procedure OnInsertItem(const InsertAt: Integer); stdcall;
    procedure OnDeleteItem(const ItemIndex: Integer); stdcall;
    procedure OnItemChanged(const ItemIndex: Integer); stdcall;
    procedure OnExchangeItem(const Index1, Index2: Integer); stdcall;
    procedure OnSelectionChanged(const ItemIndex: Integer); stdcall;

    //for future purpose - not used yet
    procedure OnNotify(const Notification: Integer; const Param: OLEVariant); stdcall;

    procedure FillMenu;
    procedure AddModule(Sender: TObject);
  public
  end;

var
  FormRunlistLisbox: TFormRunlistLisbox;

implementation

{$R *.dfm}

uses
  msmain;

procedure TFormRunlistLisbox.ModuleListBoxDblClick(Sender: TObject);
begin
  if ModuleListBox.ItemIndex >= 0 then
    Programhandler.Items[ModuleListBox.ItemIndex].Edit;
end;

procedure TFormRunlistLisbox.ModuleListBoxMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Height := TCommand(Programhandler.ActiveProgram[Index]).GetDrawHeight(TListBox(Control).Canvas);
end;

procedure TFormRunlistLisbox.ModuleListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  I: Integer;
  aRect: TRect;
begin
  TListBox(Control).Canvas.Brush.Color := clWindow;
  TListBox(Control).Canvas.FillRect(Rect);
  aRect := Rect;

  //Execution Point
  if Programhandler.IsRun then
  begin
    aRect.Left := aRect.Left + 20;
    if Index = fExecPosition then
      TListBox(Control).Canvas.Draw(Rect.Left + 2, Rect.Top + 2, fExecBitmap)
  end;

  if Programhandler.IsRun and TCommand(Programhandler.ActiveProgram[Index]).Breakpoint then
    I := 2
  else
  if not(Programhandler.IsRun) and TCommand(Programhandler.ActiveProgram[Index]).Breakpoint then
    I := 1
  else
    I := 0;

  TCommand(Programhandler.ActiveProgram[Index]).Draw(TListBox(Control).Canvas, aRect, State, I);

  PanelInclude.Visible := Programhandler.ActiveProgram.IsInclude;
  ImageInclude.Hint := Programhandler.ActiveProgram.Filename;

end;

procedure TFormRunlistLisbox.FormCreate(Sender: TObject);
begin
  fExecPosition := -1;
  fExecBitmap := TBitmap.Create;
  ImageList1.GetBitmap(8, fExecBitmap);
  fExecBitmap.TransparentMode := tmAuto;
  fExecBitmap.Transparent := True;
  ModuleListBox.DoubleBuffered := True;
  FillMenu;
  Programhandler.AddNotification(Self);
end;

procedure TFormRunlistLisbox.ModuleListBoxDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := ((Source is TCommandTypeDragObject) and
            (TCommandTypeDragObject(Source).CommandType <> nil)) or
            (Source = ModuleListBox);
end;

procedure TFormRunlistLisbox.ModuleListBoxDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  M: TCommand;
  I: Integer;
begin
  if (Source is TCommandTypeDragObject) and
    (TCommandTypeDragObject(Source).CommandType <> nil) then
  begin
    M := TCommand.Create(TCommandTypeDragObject(Source).CommandType, Programhandler);
    I := ModuleListBox.ItemAtPos(Point(X, Y), True);
    if M.Edit then
    begin
      if I > -1 then
      begin
        Programhandler.Insert(I, M)
      end
      else
      begin
        Programhandler.Add(M);
        I := Programhandler.Count-1;
      end;
      Programhandler.ClearSelection;
      Programhandler.Selected := Programhandler.Items[I];
      Programhandler.Items[I].Selected := True;
      ModuleListBox.SetFocus;
    end;
  end
end;

procedure TFormRunlistLisbox.OnProgramPaused;
begin

end;

procedure TFormRunlistLisbox.OnExchangeItem(const Index1, Index2: Integer);
begin
  ModuleListBox.Items.Exchange(Index1, Index2);
  Programhandler.Refresh;
end;

procedure TFormRunlistLisbox.OnNotify(const Notification: Integer;
  const Param: OLEVariant);
begin

end;

procedure TFormRunlistLisbox.OnBeforeStartProgram;
begin
  ModuleListBox.Invalidate;
end;

procedure TFormRunlistLisbox.OnSelectionChanged(const ItemIndex: Integer);
begin
  if ModuleListBox.Selected[ItemIndex] <> Programhandler.Items[ItemIndex].Selected then
    ModuleListBox.Selected[ItemIndex] := Programhandler.Items[ItemIndex].Selected;
end;

procedure TFormRunlistLisbox.OnClearItems;
begin
  ModuleListBox.Clear;
end;

procedure TFormRunlistLisbox.OnSetProgramPosition(const ItemIndex: Integer);
var
  S: string;
begin
  fExecPosition := ItemIndex;

  S := ModuleListBox.Items[ModuleListBox.ItemIndex];
  ModuleListBox.Items[ModuleListBox.ItemIndex] := '';
  ModuleListBox.Items[ModuleListBox.ItemIndex] := S;

  ModuleListBox.ItemIndex := ItemIndex;

  S := ModuleListBox.Items[ModuleListBox.ItemIndex];
  ModuleListBox.Items[ModuleListBox.ItemIndex] := '';
  ModuleListBox.Items[ModuleListBox.ItemIndex] := S;
end;

procedure TFormRunlistLisbox.OnDeleteItem(const ItemIndex: Integer);
begin
  ModuleListBox.Items.Delete(ItemIndex);
end;

procedure TFormRunlistLisbox.OnRefresh;
begin
  ModuleListBox.Invalidate;
end;

procedure TFormRunlistLisbox.OnItemChanged(const ItemIndex: Integer);
begin
  if (ItemIndex >= 0) and (ItemIndex < ModuleListBox.Items.Count) then
    ModuleListBox.Items[ItemIndex] := DateTimeToStr(Now)+IntToStr(ItemIndex);
end;

procedure TFormRunlistLisbox.OnAfterStopProgram;
begin
  ModuleListBox.Invalidate;
end;

procedure TFormRunlistLisbox.OnInsertItem(const InsertAt: Integer);
begin
  ModuleListBox.Items.Insert(InsertAt, DateTimeToStr(Now) + IntToStr(InsertAt));
end;

procedure TFormRunlistLisbox.OnAddItem(const ItemIndex: Integer);
begin
  ModuleListBox.Items.Add(DateTimeToStr(Now) + IntToStr(ItemIndex));
end;

procedure TFormRunlistLisbox.OnSetProgress(const ACaption: string;
  const ProgressMin, ProgressMax, ProgressPosition: Integer);
begin

end;

procedure TFormRunlistLisbox.ModuleListBoxClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ModuleListBox.Items.Count - 1 do
    Programhandler.Items[I].Selected := ModuleListBox.Selected[I];

  Programhandler.Selected := Programhandler[ModuleListBox.ItemIndex];
end;

procedure TFormRunlistLisbox.FillMenu;

  function AddMenuCategory(mn: TMenuItem; ACategory: string): TMenuItem;
  var
    I: Integer;
  begin
    Result := nil;

    for I := 0 to mn.Count - 1 do
    begin
      if SameText(StringReplace(mn.Items[I].Caption, '&', '', [rfReplaceAll]), ACategory) then
      begin
        Result := mn.Items[I];
        Break;
      end;
    end;

    if Result = nil then //not found
    begin
      Result := TMenuItem.Create(ActionList1.Owner);
      Result.Caption := ACategory;
      mn.Insert(0, Result);
    end;
  end;

var
  ac: TAction;
  mn: TMenuItem;
  I: Integer;
begin
  for I := 0 to CommandTypes.Count-1 do
  begin
    //Add the bitmap to the imagelist of the actionlist
    if not CommandTypes.Items[I].Bitmap.Empty then
    begin
      if (CommandTypes.Items[I].Bitmap.Width = ImageList1.Width) and
         (CommandTypes.Items[I].Bitmap.Height = ImageList1.Height) then
      begin
        ImageList1.AddMasked(CommandTypes.Items[I].Bitmap,
          CommandTypes.Items[I].Bitmap.TransparentColor);
      end;
    end;

    //Add the Moduletype to Actionlist and Menus
    ac := TAction.Create(ActionList1.Owner);
    ac.Caption := CommandTypes.Items[I].Name;
    ac.Name := 'acmod' + IntToStr(ActionList1.ActionCount);
    ac.ImageIndex := ImageList1.Count-1;
    ac.Tag := I;
    ac.OnExecute := AddModule;
    ac.ActionList := ActionList1;

    //PopupList Menu
    mn := TMenuItem.Create(ActionList1.Owner);
    mn.Action := ac;
    AddMenuCategory(mnAdd, CommandTypes.Items[I].Category).Add(mn);
  end;
end;

procedure TFormRunlistLisbox.AddModule(Sender: TObject);
var
  M: TCommand;

  procedure AddCommand;
  begin
    if Programhandler.SelectedCount > 0 then
    begin
      if Programhandler.Selected = Programhandler[Programhandler.Count-1] then
      begin
        Programhandler.Add(M);
      end
      else
      begin
        Programhandler.Insert(Programhandler.IndexOf(Programhandler.Selected) + 1, M);
      end;
    end
    else
    begin
      Programhandler.Add(M);
    end;
  end;

begin
  if Sender is TAction then
  begin
    M := TCommand.Create(CommandTypes[TAction(Sender).Tag], Programhandler);
    if M.Edit then
    begin
      AddCommand;
      Programhandler.Modified := True;
      if Programhandler.IsSystemCommand(M) then
      begin
        Programhandler.Refresh;
      end;
    end
    else
      M.Free;
  end;
end;


procedure TFormRunlistLisbox.mnEditClick(Sender: TObject);
begin
  if Programhandler.Selected <> nil then
    Programhandler.Selected.Edit;
end;

procedure TFormRunlistLisbox.mnDeleteClick(Sender: TObject);
begin
  if Programhandler.SelectedCount > 0 then
    Programhandler.DeleteSelectedItems;
end;

procedure TFormRunlistLisbox.PopupListPopup(Sender: TObject);
begin
  mnEdit.Enabled := Programhandler.Selected <> nil;
  mnCopy.Enabled := Programhandler.CanCopy;
  mnCut.Enabled := Programhandler.CanCopy;
  mnPaste.Enabled := Programhandler.CanPaste;
  mnDelete.Enabled := Programhandler.SelectedCount>0;
end;

procedure TFormRunlistLisbox.mnCopyClick(Sender: TObject);
begin
  if Programhandler.CanCopy then
    Programhandler.CopySelectionToClipboard;
end;

procedure TFormRunlistLisbox.mnCutClick(Sender: TObject);
begin
  if Programhandler.CanCopy then
    Programhandler.CutSelectionToClipboard;
end;

procedure TFormRunlistLisbox.mnPasteClick(Sender: TObject);
begin
  if Programhandler.CanPaste then
    Programhandler.PasteFromClipboard;
end;

procedure TFormRunlistLisbox.FormDestroy(Sender: TObject);
begin
  fExecBitmap.Free;
end;

end.
