(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msfrmcommands.pas

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
2005/02/09  BSchranz  - Added Copy, Past, Docking, Debugging
2005/04/09  BSchranz  - Translated to englisch
2005/04/09  USchuster - D5 fix

-----------------------------------------------------------------------------*)

unit msFrmCommands;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvDockControlForm, ComCtrls, msGlobals, msprogram,
  ImgList, msutils, jclStrings, JvComponentBase, JvEmbeddedForms;

type
  TFormCommands = class(TForm)
    lvModules: TTreeView;
    ImageList1: TImageList;
    Link: TJvEmbeddedFormLink;
    procedure JvDockClientFormShow(Sender: TObject);
    procedure lvModulesStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvModulesCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure lvModulesDblClick(Sender: TObject);
    procedure lvModulesCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
  public
    procedure FillCommandList;
  end;

var
  FormCommands: TFormCommands;

implementation

{$R *.dfm}

procedure TFormCommands.lvModulesCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.Level = 0 then
    TTreeView(Sender).Canvas.Font.Style := [fsBold]
  else
    TTreeView(Sender).Canvas.Font.Style := [];
end;

procedure TFormCommands.lvModulesDblClick(Sender: TObject);
var M:TCommand;
    Idx:Integer;
begin
  if (lvModules.Selected<>nil) and (Integer(lvModules.Selected.Data)>=0) then begin
    Idx := Integer(lvModules.Selected.Data);
    M := TCommand.Create(CommandTypes[Idx], Programhandler);
    if M.Edit then
    begin
      Programhandler.Add(M);
      Programhandler.Modified := True;
    end
    else
      M.Free;
  end;
end;

procedure TFormCommands.lvModulesCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  Compare := CompareText(Node1.Text, Node2.Text);
end;

procedure TFormCommands.FormCreate(Sender: TObject);
begin
  FillCommandList;
end;

procedure TFormCommands.FillCommandList;

  function AddTreeCategory( ANode:TTreeNode; ACategory: string): TTreeNode;
  var
    it: TTreeNode;
    sl: TStringList;
    i : Integer;
    s : String;
  begin
    Result := nil;

    sl := TStringList.Create;
    try
      StrTokenToStrings( ACategory, '\', sl);
      if sl.Count>0 then begin

        if aNode<>nil then
          it := aNode.getFirstChild
        else
          it := lvModules.Items.GetFirstNode;
        while it <> nil do
        begin
          if SameText(it.Text, sl[0]) then
          begin
            Result := it;
            it := nil;
          end
          else
            it := it.getNextSibling;
        end;

        if Result = nil then //not found
        begin
          Result := lvModules.Items.AddChild( ANode, sl[0]);
          Result.ImageIndex := 1;
          Result.SelectedIndex := Result.ImageIndex;
          Result.StateIndex := Result.ImageIndex;
          Result.Data := Pointer(-1);
          Result.Expanded := true;
        end;

        //Check for Sub - Nodes and proceed if nessecary
        if sl.Count>1 then begin
          s := '';
          for i:=1 to sl.Count-1 do begin
            s := s + sl[i];
            if i<sl.Count-1 then
              s := s + '\';
          end;
          Result := AddTreeCategory( Result, s);
        end;

      end;
    finally
      sl.Free;
    end;
  end;

var i:Integer;
    it:TTreeNode;
begin
  for i:=0 to CommandTypes.Count-1 do begin

    it := lvModules.Items.AddChild(
      AddTreeCategory( nil, CommandTypes.Items[i].Category),
       CommandTypes.Items[i].Name);

    if not CommandTypes.Items[i].Bitmap.Empty then
    begin
      if (CommandTypes.Items[i].Bitmap.Width = ImageList1.Width) and
         (CommandTypes.Items[i].Bitmap.Height = ImageList1.Height) then
      begin
        ImageList1.AddMasked(CommandTypes.Items[i].Bitmap,
              CommandTypes.Items[i].Bitmap.TransparentColor);
      end;
    end;

    it.ImageIndex := ImageList1.Count-1;
    it.SelectedIndex := ImageList1.Count-1;
    it.StateIndex := ImageList1.Count-1;
    it.Data := Pointer(i);
  end;
end;

procedure TFormCommands.FormShow(Sender: TObject);
begin
  lvModules.FullExpand;
end;

procedure TFormCommands.lvModulesStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var M:TCommand;
    Idx:Integer;
begin
  if (lvModules.Selected<>nil) then begin
    Idx := Integer(lvModules.Selected.Data);
    DragObject := TCommandTypeDragObject.Create;
    if Integer(lvModules.Selected.Data)>=0 then
      TCommandTypeDragObject(DragObject).CommandType :=
           CommandTypes[Integer(lvModules.Selected.Data)]
    else
      TCommandTypeDragObject(DragObject).CommandType := nil;
  end;
end;


procedure TFormCommands.JvDockClientFormShow(Sender: TObject);
begin
  lvModules.FullExpand;
end;

end.
