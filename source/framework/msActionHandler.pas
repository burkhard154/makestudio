(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msActionHandler.pas

The Initial Developer of the original code is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:                                                                                    }

2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in
2005/02/19  USchuster - changes for commandline version
2005/03/05  USchuster - changed some interface typecasts from I..() to .. as I..
                        because the hard I..() typecasts don't work for assemblies
2005/02/09  BSchranz  - Added Copy, Past, Docking, Debugging
2005/04/09  BSchranz  - Translated to englisch

-----------------------------------------------------------------------------*)
//Actionhandling for external actions from plugins
unit msActionHandler;

{$I jedi.inc}

interface

uses
  Windows, Classes, SysUtils, Forms, ActnList, Menus, makestudio_TLB,
  Graphics, Dialogs, JclFileUtils;

const
  JVMENUPATHSEPARATOR = '\';

type
  TJVCSActionhandler = class(TPersistent)
  private
    FExternalActions: array of IDispatch;
    FActionList: TActionList;
    FPluginMenu: TMenuItem;
    procedure AddMenuAction(AAction: TAction; MenuPath: string);
    function MainMenu: TMenu;
  public
    constructor Create(AActionList: TActionList; APluginMenu: TMenuItem); reintroduce;
    destructor Destroy; override;

    procedure ClearCallbacks;
    procedure Add(AName: string; ACaption: string; AHint: string; ABitmap: TBitmap;
      ACallback: IDispatch);
    procedure OnExecute(Sender: TObject);

    property ActionList:TActionList read FActionList write FActionList;
    property PluginMenu:TMenuItem read FPluginMenu write FPluginMenu;
  end;

implementation

function CharDelete(const S: string; ToDelete: Char): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(S) - 1 do
    if S[I + 1] <> ToDelete then
      Result := Result + S[I + 1];
end;

procedure ParseString(const S: string; Separator: Char; AStrings: TStrings);

  function ChangeCharTo(FromChar, ToChar: Char; const S: string): string;
  var
    I: Integer;
  begin
    Result := S;
    for I := Length(Result) - 1 downto 0 do
      if Result[I + 1] = FromChar then
        Result[I + 1] := ToChar;
  end;

var
  slist: TStringList;
begin
  slist := TStringList.Create;
  with slist do
    try
      Text := ChangeCharTo(Separator, #10, S);
      AStrings.AddStrings(slist);
    finally
      Free;
    end;
end;

constructor TJVCSActionhandler.Create(AActionList: TActionList; APluginMenu: TMenuItem);
begin
  inherited Create;

  FActionList := AActionList;
  FPluginMenu := APluginMenu;
end;

destructor TJVCSActionhandler.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FExternalActions) - 1 do
    FExternalActions[I] := nil;
  SetLength(FExternalActions, 0);
  inherited Destroy;
end;

procedure TJVCSActionhandler.ClearCallbacks;
var
  I: Integer;
begin
  for I := 0 to Length(FExternalActions) - 1 do
    FExternalActions[I] := nil;
  SetLength(FExternalActions, 0);
end;

function TJVCSActionhandler.MainMenu: TMenu;
begin
  Result := nil;
  if Assigned(FPluginMenu) then
    Result := FPluginMenu.GetParentMenu;
end;

procedure TJVCSActionhandler.AddMenuAction(AAction: TAction; MenuPath: string);
var
  I, K: Integer;
  sl: TStringList;
  m1: TMenuItem;
  m2: TMenuItem;
begin
  if MainMenu <> nil then
  begin
    with MainMenu do
    begin
      sl := TStringList.Create;
      try
        ParseString(MenuPath, JVMENUPATHSEPARATOR, sl);
        if sl.Count = 0 then //no Path!
        begin
          sl.Clear;
          ParseString(CharDelete(FPluginMenu.Caption, '&'), JVMENUPATHSEPARATOR, sl);
        end;

        m1 := MainMenu.Items;
        for I := 0 to sl.Count - 1 do
        begin
          m2 := nil;

          for K := 0 to m1.Count - 1 do
            if CompareText(CharDelete(m1.Items[K].Caption, '&'), CharDelete(sl[I], '&')) = 0 then
            begin
              m2 := m1.Items[K];
            end;

          if m2 = nil then
          begin
            m2 := TMenuItem.Create(MainMenu);
            m2.Caption := sl[I];
            if m1 = MainMenu.Items then //insert before Help menu
              m1.Insert(m1.Count - 1, m2)
            else
              m1.Add(m2);
          end;

          m1 := m2;
        end;
        m2 := TMenuItem.Create(MainMenu);
        m2.Action := AAction;
        m1.Add(m2);
      finally
        sl.Free;
      end;
    end;
  end;
end;

procedure TJVCSActionhandler.Add(AName: string; ACaption: string; AHint: string;
  ABitmap: TBitmap; ACallback: IDispatch);
var
  ImgIndex: Integer;
  ac: TAction;
  DummyIntf: IUnknown;
begin
  if FActionList = nil then Exit;

  if Supports(ACallback, IID_IActionCallback, DummyIntf) then
  begin
    SetLength(FExternalActions, Length(FExternalActions) + 1);
    FExternalActions[Length(FExternalActions) - 1] := ACallback;

    //Add the bitmap to the imagelist of the actionlist
    ImgIndex := -1;
    if (ABitmap <> nil) and (FActionList.Images <> nil) then
    begin
      if (ABitmap.Width > 0) and (ABitmap.Height > 0) then
      begin
        if FActionList.Images <> nil then
        begin
          ABitmap.PixelFormat := pf8bit;
          FActionList.Images.AddMasked(ABitmap, ABitmap.TransparentColor);
          ImgIndex := FActionList.Images.Count - 1;
        end;
      end;
    end;

    //Add the Action
    ac := TAction.Create(FActionList.Owner);
    ac.ImageIndex := ImgIndex;
    ac.OnExecute := OnExecute;
    ac.Name := AName;
    ac.Caption := ExtractFileName(ACaption);
    ac.Hint := AHint;
    ac.Tag := Length(FExternalActions) - 1;
    ac.ActionList := FActionList;

    //Add the Action to the menu
    AddMenuAction(ac, PathRemoveSeparator(ExtractFilePath(ACaption)));
  end
  else
  begin
    MessageDlg('INTERNAL ERROR: AddAction-CallbackInterface does not support IBarItemCallback!' + #10#13 +
                'If you use an older "IVerbActionCallback" Plugin, you have to modify the plugin!!' + #10#13 +
                'Menu Item=' + AName,
          mtError, [mbOK], 0);
  end;
end;

procedure TJVCSActionhandler.OnExecute(Sender: TObject);
begin
  (FExternalActions[TAction(Sender).Tag] as IActionCallback).
    Execute(TAction(Sender).Name);
end;

end.
