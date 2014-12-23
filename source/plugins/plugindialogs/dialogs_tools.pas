(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dialogs_tools.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/04  JDuenow   - launched Dialogs Module
2005/06/20  USchuster - D5 fix
2006/02/25  BSchranz - MessageBoxButtons Bug Fix


-----------------------------------------------------------------------------*)

unit dialogs_tools;

interface

uses
  Dialogs, Classes, SysUtils, dialogs_Vars;

function BStringToBType(sButtons: string): TMsgDlgButtons;
function FormatText(Text: string): string;
function FormatDefault(Text: string): string;

implementation

uses
  JclStrings, TypInfo;

function BStringToBType(sButtons: string): TMsgDlgButtons;
var
  I: Integer;
  s:TMsgDlgButtons;
begin

  s := [];
  for I := 0 to Ord( High(TMsgDlgBtn)) do
  begin
    if Pos( GetEnumName( TypeInfo( TMsgDlgBtn), i), sButtons)<>0 then
      Include( s, TMsgDlgBtn(i));
  end;
  if s = [] then
    s := [mbOk];
  Result := s;
end;

function FormatText(Text: string): string;
var
  I, J: Integer;
  variable: string;
  slCrlf,
  slVars: TStringList;
begin
  Text := StringReplace(Text, '%%', '%', [rfReplaceAll]);
  Text := StringReplace(Text, '"', '', [rfReplaceAll]);

  slVars := TStringList.Create;
  try
    slCrlf := TStringList.Create;
    try
      StrToStrings(Text, '|', slCrlf);
      for J := 0 to slCrlf.Count - 1 do
      begin
        StrToStrings(slCrlf.Strings[J], ' ', slVars);
        for I := 0 to slVars.Count - 1 do
        begin
          if Pos('%', slVars.Strings[I]) = 1 then
          begin
            variable := StringReplace(slVars.Strings[I], '%', '', [rfReplaceAll]);
            if jvcsmak.Variables.VarExists(variable) then
              Text := StringReplace(Text, '%' + variable, jvcsmak.Variables.Values[variable], [rfReplaceAll]);
          end;
        end;
      end;
    finally
      slCrlf.Free;
    end;
  finally
    slVars.Free;
  end;

  Result := StringReplace(Text, '|', #10#13, [rfReplaceAll]);
end;

function FormatDefault(Text: string): string;
var
  I: Integer;
  variable: string;  
  slVars: TStringList;
begin
  Text := StringReplace(Text, '%%', '%', [rfReplaceAll]);

  slVars := TStringList.Create;
  try
    StrToStrings(Text, ' ', slVars);
    for I := 0 to slVars.Count - 1 do
    begin
      if Pos('%', slVars.Strings[I]) = 1 then
      begin
        variable := StringReplace(slVars.Strings[I], '%', '', [rfReplaceAll]);
        if jvcsmak.Variables.VarExists(variable) then
          Text := StringReplace(Text, '%' + variable, jvcsmak.Variables.Values[variable], [rfReplaceAll]);
      end;
    end;
  finally
    slVars.Free;
  end;

  Result := Text;
end;

end.
