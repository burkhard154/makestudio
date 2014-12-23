(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EasyBackup_Utils.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/05  BSchranz  - Easybackup Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit EasyBackup_Utils;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  Classes, SysUtils, Registry, Windows, Forms, Dialogs, ActiveX, JclFileUtils;

{:Sucht alle Dateien im angegebenen Verzeichnis
und füllt die Stringliste mit den Namen ohne die Extensions}
procedure GetFileList(const Path, Extension: string; List: TStrings);

{:Sucht alle Dateien im angegebenen Verzeichnis
und füllt die Stringliste mit den Namen. Der Parameter Options bestimmt wie der
Dateiname aussehen soll
@param Options
0=Dateinamen ohne Pfad und Extension
1=Dateinamen ohne Pfad und mit Extension
2=Dateinamen mit Pfad und mit Extension}
procedure GetFileListEx(const Path, Extension: string; Options: Integer; List: TStrings);

{:Fills the list with all Subdirectories in the given Folder}
procedure GetFileListDir(const Path: string; List: TStrings);

implementation

procedure GetFileList(const Path, Extension: string; List: TStrings);
begin
  GetFileListEx(Path, Extension, 0, List);
end;

procedure GetFileListEx(const Path, Extension: string; Options: Integer; List: TStrings);
var
  sr: TSearchRec;
  P: string;

  procedure Add;
  begin
    case Options of
      0: List.Add(ChangeFileExt(sr.Name, ''));
      1: List.Add(sr.Name);
      2: List.Add(P + sr.Name);
    end;
  end;

begin
  List.Clear;

  if Path = '' then Exit;
  P := PathAddSeparator(Path);

  if FindFirst(P + '*.'+Extension, SysUtils.faArchive, sr) = 0 then
  begin
    Add;
    while FindNext(sr) = 0 do Add;
    SysUtils.FindClose(sr);
  end;
end;

procedure GetFileListDir(const Path: string; List: TStrings);
var
  sr: TSearchRec;
  P: string;
begin
  List.Clear;

  if Path='' then Exit;
  P := PathAddSeparator(Path);

  if FindFirst(P + '*.*', faDirectory, sr) = 0 then
  begin
    if (sr.Name<>'.') and (sr.Name<>'..') and (sr.Attr and faDirectory=faDirectory) then
      List.Add(sr.Name);
    while FindNext(sr) = 0 do
      if (sr.Name<>'.') and (sr.Name<>'..') and (sr.Attr and faDirectory=faDirectory) then
        List.Add(sr.Name);
    SysUtils.FindClose(sr);
  end;
end;

end.
