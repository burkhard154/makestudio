(*======================================================================*
 | unitNTModule unit                                                    |
 |                                                                      |
 | Load resources from a module                                         |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002.  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      04/04/2002  CPWW  Original                                  |
 |          28/05/2005  CPWW  Implemented changing resources.           |
 *======================================================================*)

unit unitNTModule;

interface

uses Windows, Classes, SysUtils, unitResourceDetails, ConTnrs;

type

TNTModule = class (TResourceModule)
private
  fDetailList : TObjectList;
  fTag: Integer;

  procedure AddResourceToList (AType, AName : PWideChar; ADataLen :  Integer; AData : pointer; ALang : word);
  function LoadResourceFromModule (hModule : Integer; const resType, resName : PChar; language : word) : boolean;
protected
  function GetResourceCount: Integer; override;
  function GetResourceDetails(idx: Integer): TResourceDetails; override;
public
  constructor Create;
  destructor Destroy; override;

  procedure LoadFromFile (const FileName : string); override;
  procedure SaveToFile (const FileName : string); override;
  procedure LoadResources (const fileName : string; tp : PChar);
  procedure DeleteResource (resourceNo : Integer); override;
  procedure InsertResource (idx : Integer; details : TResourceDetails); override;
  function AddResource (details : TResourceDetails) : Integer; override;
  function IndexOfResource (details : TResourceDetails) : Integer; override;
  property Tag : Integer read fTag write fTag;
end;

implementation

resourcestring
  rstCantUpdate = 'Must use Windows NT, 2000 or XP to update resoures';

type
  TfnBeginUpdateResource = function (pFileName: PChar; bDeleteExistingResources: BOOL): THandle; stdcall;
  TfnUpdateResource = function (hUpdate: THandle; lpType, lpName: PChar; wLanguage: Word; lpData: Pointer; cbData: DWORD): BOOL; stdcall;
  TfnEndUpdateResource = function (hUpdate: THandle; fDiscard: BOOL): BOOL; stdcall;

var
  fnBeginUpdateResource : TfnBeginUpdateResource = Nil;
  fnEndUpdateResource : TfnEndUpdateResource = Nil;
  fnUpdateResource : TfnUpdateResource = Nil;

(*----------------------------------------------------------------------------*
 | function EnumResLangProc ()                                                |
 |                                                                            |
 | Callback for EnumResourceLanguages                                         |
 |                                                                            |
 | lParam contains the resource module instance.                              |
 *----------------------------------------------------------------------------*)
function EnumResLangProc (hModule : Integer; resType, resName : PChar; wIDLanguage : word; lParam : Integer) : BOOL; stdcall;
begin
  TNTModule (lParam).LoadResourceFromModule (hModule, resType, resName, wIDLanguage);
  result := True
end;

(*----------------------------------------------------------------------*
 | EnumResNamesProc                                                     |
 |                                                                      |
 | Callback for EnumResourceNames                                       |
 |                                                                      |
 | lParam contains the resource module instance.                        |
 *----------------------------------------------------------------------*)
function EnumResNamesProc (hModule : Integer; resType, resName : PChar; lParam : Integer) : BOOL; stdcall;
begin
  if not EnumResourceLanguages (hModule, resType, resName, @EnumResLangProc, lParam) then
    RaiseLastOSError;
  result := True;
end;

(*----------------------------------------------------------------------*
 | EnumResTypesProc                                                     |
 |                                                                      |
 | Callback for EnumResourceTypes                                       |
 |                                                                      |
 | lParam contains the resource module instance.                        |
 *----------------------------------------------------------------------*)
function EnumResTypesProc (hModule : Integer; resType : PChar; lParam : Integer) : BOOL; stdcall;
begin
  EnumResourceNames (hModule, resType, @EnumResNamesProc, lParam);
  result := True;
end;

{ TNTModule }

const
  rstNotSupported = 'Not supported';

(*----------------------------------------------------------------------*
 | TNTModule.AddResourceToList                                          |
 |                                                                      |
 | Add resource to the resource details list                            |
 *----------------------------------------------------------------------*)
function TNTModule.AddResource(details: TResourceDetails): Integer;
begin
  result := fDetailList.Add(details)
end;

procedure TNTModule.AddResourceToList(AType, AName: PWideChar;
  ADataLen: Integer; AData: pointer; ALang: word);
var
  details : TResourceDetails;

  function ws (ws : PWideChar) : string;
  begin
    if (Integer (ws) and $ffff0000) <> 0 then
      result := ws
    else
      result := IntToStr (Integer (ws))
  end;

begin
  details := TResourceDetails.CreateResourceDetails (self, ALang, ws (AName), ws (AType), ADataLen, AData);
  fDetailList.Add (details);
end;

(*----------------------------------------------------------------------*
 | TNTModule.Create                                                     |
 |                                                                      |
 | Constructor for TNTModule                                            |
 *----------------------------------------------------------------------*)
constructor TNTModule.Create;
begin
  inherited Create;
  fDetailList := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | TNTModule.Destroy                                                    |
 |                                                                      |
 | Destructor for TNTModule                                             |
 *----------------------------------------------------------------------*)
procedure TNTModule.DeleteResource(resourceNo: Integer);
var
  res : TResourceDetails;
begin
  res := ResourceDetails [resourceNo];
  inherited;
  resourceNo := IndexOfResource (Res);
  if resourceNo <> -1 then
    fDetailList.Delete (resourceNo);
end;

destructor TNTModule.Destroy;
begin
  fDetailList.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TNTModule.GetResourceCount                                           |
 |                                                                      |
 | Get method for ResourceCount property                                |
 *----------------------------------------------------------------------*)
function TNTModule.GetResourceCount: Integer;
begin
  result := fDetailList.Count
end;

(*----------------------------------------------------------------------*
 | TNTModule.GetResourceDetails                                         |
 |                                                                      |
 | Get method for resource details property                             |
 *----------------------------------------------------------------------*)
function TNTModule.GetResourceDetails(idx: Integer): TResourceDetails;
begin
  result := TResourceDetails (fDetailList [idx])
end;

(*----------------------------------------------------------------------*
 | TNTModule.IndexOfResource                                            |
 |                                                                      |
 | Find the index for specified resource details                        |
 *----------------------------------------------------------------------*)
function TNTModule.IndexOfResource(details: TResourceDetails): Integer;
begin
  result := fDetailList.IndexOf (details);
end;

(*----------------------------------------------------------------------*
 | TNTModule.LoadFromFile                                               |
 |                                                                      |
 | Load all of a module's resources                                     |
 *----------------------------------------------------------------------*)
procedure TNTModule.InsertResource(idx: Integer; details: TResourceDetails);
begin
  fDetailList.Insert(idx, details);
end;

procedure TNTModule.LoadFromFile(const FileName: string);
begin
  LoadResources (FileName, Nil);
end;

(*----------------------------------------------------------------------*
 | TNTModule.LoadResourceFromModule                                     |
 |                                                                      |
 | Load a particular resource from a resource handle.  Called from      |
 | EnumResLangProc when enumerating resources                           |
 *----------------------------------------------------------------------*)
function TNTModule.LoadResourceFromModule(hModule: Integer; const resType,
  resName: PChar; language: word): boolean;
var
  resourceHandle : Integer;
  infoHandle, size : Integer;
  p : PChar;
  pt, pn : PWideChar;
  wType, wName : WideString;
begin
  result := True;
  resourceHandle := Windows.FindResource (hModule, resName, resType);
  if resourceHandle <> 0 then
  begin
    size := SizeOfResource (hModule, resourceHandle);
    infoHandle := LoadResource (hModule, resourceHandle);
    if infoHandle <> 0 then
    try
      p := LockResource (infoHandle);

      if (Integer (resType) and $ffff0000) = 0 then
        pt := PWideChar (resType)
      else
      begin
        wType := resType;
        pt := PWideChar (wType)
      end;

      if (Integer (resName) and $ffff0000) = 0 then
        pn := PWideChar (resName)
      else
      begin
        wName := resName;
        pn := PWideChar (wName)
      end;

      AddResourceToList (pt, pn, size, p, language);
    finally
      FreeResource (infoHandle)
    end
    else
      RaiseLastOSError;
  end
  else
    RaiseLastOSError;
end;

(*----------------------------------------------------------------------*
 | TNTModule.LoadResources                                              |
 |                                                                      |
 | Load resources of a particular type                                  |
 *----------------------------------------------------------------------*)
procedure TNTModule.LoadResources(const fileName: string; tp: PChar);
var
  Instance : THandle;
begin
  Instance := LoadLibraryEx (PChar (fileName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if Instance <> 0 then
  try
    fDetailList.Clear;
    if tp = Nil then
      EnumResourceTypes (Instance, @EnumResTypesProc, Integer (self))
    else
    begin                           // ... no.  Load specified type...
                                    // ... but if that's an Icon or Cursor group, load
                                    // the icons & cursors, too!

      if tp = RT_GROUP_ICON then
        EnumResourceNames (Instance, RT_ICON, @EnumResNamesProc, Integer (self))
      else
        if tp = RT_GROUP_CURSOR then
          EnumResourceNames (Instance, RT_CURSOR, @EnumResNamesProc, Integer (self));

      EnumResourceNames (Instance, tp, @EnumResNamesProc, Integer (self))
    end
  finally
    FreeLibrary (Instance)
  end
  else
    RaiseLastOSError;
end;

(*----------------------------------------------------------------------*
 | TNTModule.SaveToFile                                                 |
 |                                                                      |
 | Update the module's resources.                                       |
 *----------------------------------------------------------------------*)
procedure TNTModule.SaveToFile(const FileName: string);
var
  UpdateHandle : THandle;
  i : integer;
  details : TResourceDetails;
  discard : boolean;
  namest, tpst : string;

  function ResourceNameInt (const name : string) : PChar;
  var
    n : Integer;
  begin
    n := ResourceNameToInt (name);
    if n = -1 then
      result := PChar (name)
    else
      result := PChar (n)
  end;

begin
  if not Assigned (fnUpdateResource) or not Assigned (fnBeginUpdateResource) or not Assigned (fnEndUpdateResource) then
    raise Exception.Create (rstCantUpdate);

  discard := True;
  UpdateHandle := fnBeginUpdateResource (PChar (FileName), true);
  try
    for i := 0 to ResourceCount - 1 do
    begin
      details := ResourceDetails [i];

      namest := details.ResourceName;
      tpst   := details.ResourceType;

      if not fnUpdateResource (UpdateHandle,
                      ResourceNameInt (tpst),
                      ResourceNameInt (namest),
                      details.ResourceLanguage,
                      details.Data.Memory,
                      details.Data.Size) then
        RaiseLastOSError;
    end;
    ClearDirty;
    discard := False

  finally
    fnEndUpdateResource (UpdateHandle, discard)
  end
end;

procedure Initialize;
var
  hkernel : THandle;
begin
  hkernel := LoadLibrary ('kernel32.dll');
  fnBeginUpdateResource := TfnBeginUpdateResource (GetProcAddress (hkernel, 'BeginUpdateResourceA'));
  fnEndUpdateResource := TfnEndUpdateResource (GetProcAddress (hkernel, 'EndUpdateResourceA'));
  fnUpdateResource := TfnUpdateResource (GetProcAddress (hkernel, 'UpdateResourceA'));
end;

begin
  Initialize
end.
