(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsmakVarListInspect.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/02  BSchranz  - Migration to JVCSMak with external plugins
2005/02/04  USchuster - preparations for check in
2005/04/09  BSchranz  - Translated to englisch

-----------------------------------------------------------------------------*)

unit msVarListInspect;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, ExtCtrls, ToolWin,
  JvComponent, JvDockControlForm, msVarhandler, msglobals,
  JvComponentBase, JvEmbeddedForms;

type
  TFormVarlistInspect = class(TForm, IVarCallback)
    ImageList1: TImageList;
    JvDockClient: TJvDockClient;
    lvVars: TListView;
    Link: TJvEmbeddedFormLink;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure FillList;

    {IVarCallback}
    function OnVarChanged(const Varname: WideString; const Value: OleVariant): HResult; stdcall;
    function OnVarCreated(const Varname: WideString): HResult; stdcall;
    function OnVarDeleted(const Varname: WideString): HResult; stdcall;
    function OnVarnameChanged(const OldVarname: WideString; const NewVarname: WideString): HResult; stdcall;

  public
  end;

var
  FormVarlistInspect: TFormVarlistInspect;


implementation

{$R *.dfm}

procedure TFormVarlistInspect.FillList;

    procedure AddVarItem(AVar: TxVar);
    var
      li: TListItem;
    begin
      li := lvVars.Items.Add;
      li.Caption := AVar.Varname;
      li.SubItems.Add(AVar.AsString);
      li.SubItems.Add(AVar.Category);
      li.SubItems.Add(AVar.Hint);
    end;

var
  sl1, sl: TStringList;
  I, K: Integer;
begin
  sl := TStringList.Create;

  lvVars.Items.Clear;
  lvVars.Items.BeginUpdate;
  try
    Varhandler.GetStructureList(sl);

    for I := 0 to Varhandler.Vars.Count - 1 do
      AddVarItem(TxVar(Varhandler.Vars.Items[I]));

  finally
    lvVars.Items.EndUpdate;
    sl.Free;
  end;
end;

function TFormVarlistInspect.OnVarChanged(const Varname: WideString; const Value: OleVariant): HResult;
var
  I: Integer;
begin
  for I:=0 to lvVars.Items.Count-1 do
  begin
    if SameText(Varname, lvVars.Items[I].Caption) then
      lvVars.Items[I].SubItems[0] := Value;
  end;
  Result := 0;
end;

function TFormVarlistInspect.OnVarCreated(const Varname: WideString): HResult;
begin
  with lvVars.Items.Add do
  begin
    Caption := Varname;
    SubItems.Add('');
    SubItems.Add('');
  end;
  Result := 0;
end;

function TFormVarlistInspect.OnVarDeleted(const Varname: WideString): HResult;
var
  I: Integer;
begin
  for I := 0 to lvVars.Items.Count - 1 do
    if SameText(Varname, lvVars.Items[I].Caption) then
    begin
      lvVars.Items.Delete(I);
      Break;
    end;
  Result := 0;
end;

function TFormVarlistInspect.OnVarnameChanged(const OldVarname: WideString; const NewVarname: WideString): HResult;
var
  I: Integer;
begin
  for I := 0 to lvVars.Items.Count - 1 do
    if SameText(OldVarname, lvVars.Items[I].Caption) then
    begin
      lvVars.Items[I].Caption := NewVarname;
      Break;
    end;
  Result := 0;
end;


procedure TFormVarlistInspect.FormCreate(Sender: TObject);
begin
  Varhandler.AddNotifier(IVarCallback(Self));
  FillList;
end;

procedure TFormVarlistInspect.FormShow(Sender: TObject);
begin
  FillList;
end;

end.
