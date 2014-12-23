unit SimpleRCVersionInfo;

{ Written by Rick Peterson, Modified by Frank Wunderlich }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TypInfo;

type
{$M+}
  TVersionType = (vtCompanyName, vtFileDescription, vtFileVersion, vtInternalName, vtLegalCopyright,
    vtLegalTradeMark, vtOriginalFileName, vtProductName, vtProductVersion, vtComments);

  TIntVersionInfo = record
    MainV, SubV, Release, Build: Word;
  end;


  TCustomVersionInfo = class(TObject)
  private
    FVersionInfo: Array [0 .. ord( high(TVersionType))] of string;
    FIntVersionInfo: Array [0 .. 1] of TIntVersionInfo;
    FLangID, FCharset: Word;
    FFileOS, FFileType, FFileFlags: Integer;
    function GetVersionInfo(VersionType: TVersionType): string;
    procedure SetVersionInfo(VersionType: TVersionType; const Value: String);
  public
    constructor Create; overload; virtual;
    constructor Create(Filename: String); overload; virtual;

    procedure LoadFromFile(Filename: String); virtual;
    procedure SavetoFile(Filename: String); virtual;
    procedure SetIntFileVersion( Main, Sub, Release, Build: Word);
    function GetIntFileVersion: TIntVersionInfo;
    procedure SetIntProductVersion( Main, Sub, Release, Build: Word);
    function GetIntProductVersion: TIntVersionInfo;
    function GetLangID: Word;
    function GetCharset: Word;
    function GetFileOS: Integer;
    function GetFileType: Integer;
    function isPrivateBuild: Boolean;
    function isSpecialBuild: Boolean;
    function isDebugBuild: Boolean;
    function isPatched: Boolean;
    function isPreRelease: Boolean;
    function GetCompanyName: string;
    function GetFileDescription: string;
    function GetFileVersion: string;
    function GetInternalName: string;
    function GetLegalCopyright: string;
    function GetLegalTradeMark: string;
    function GetOriginalFileName: string;
    function GetProductName: string;
    function GetProductVersion: string;
    function GetComments: string;
    property VersionInfo[ VersionType:TVersionType]:String read GetVersionInfo write SetVersionInfo;
  end;

  // EXE and DLL
  TPEVersionInfo = class(TCustomVersionInfo)
  public
    procedure LoadFromFile(Filename: String); override;
  end;

  TRCVersionInfo = class(TCustomVersionInfo)
  private
    FInnerText: TStrings;
    procedure ParseVersion;
    function ParseVersionNumber(vStr: string): TIntVersionInfo;
    procedure SetInnerText(const Value: TStrings);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromFile(Filename: String); override;
    procedure SavetoFile(Filename: String); override;
    property InnerText:TStrings read FInnerText write SetInnerText;
  end;

implementation

resourcestring
  StrInvalidVersionstring = '%s: Version "%s" ist nicht gültig!';

constructor TCustomVersionInfo.Create(Filename: String);
begin
  Create;
  LoadFromFile(Filename);
end;

function TCustomVersionInfo.GetIntFileVersion: TIntVersionInfo;
begin
  result := FIntVersionInfo[0];
end;

function TCustomVersionInfo.GetIntProductVersion: TIntVersionInfo;
begin
  result := FIntVersionInfo[1];
end;

function TCustomVersionInfo.GetLangID: Word;
begin
  result := FLangID;
end;

constructor TCustomVersionInfo.Create;
begin
  inherited;
end;

function TCustomVersionInfo.GetCharset: Word;
begin
  result := FCharset;
end;

function TCustomVersionInfo.GetFileOS: Integer;
begin
  result := FFileOS;
end;

function TCustomVersionInfo.GetFileType: Integer;
begin
  result := FFileType;
end;

function TCustomVersionInfo.isPrivateBuild: Boolean;
begin
  result := (FFileFlags and VS_FF_PRIVATEBUILD) > 0;
end;

function TCustomVersionInfo.isSpecialBuild: Boolean;
begin
  result := (FFileFlags and VS_FF_SPECIALBUILD) > 0;
end;

procedure TCustomVersionInfo.LoadFromFile(Filename: String);
begin
end;

function TCustomVersionInfo.isDebugBuild: Boolean;
begin
  result := (FFileFlags and VS_FF_DEBUG) > 0;
end;

function TCustomVersionInfo.isPatched: Boolean;
begin
  result := (FFileFlags and VS_FF_PATCHED) > 0;
end;

function TCustomVersionInfo.isPreRelease: Boolean;
begin
  result := (FFileFlags and VS_FF_PRERELEASE) > 0;
end;

// Stringfileinfo
function TCustomVersionInfo.GetCompanyName: string;
begin
  result := getversioninfo(vtCompanyName);
end;

function TCustomVersionInfo.GetFileDescription: string;
begin
  result := getversioninfo(vtFileDescription);
end;

function TCustomVersionInfo.GetFileVersion: string;
begin
  result := getversioninfo(vtFileVersion);
end;

function TCustomVersionInfo.GetInternalName: string;
begin
  result := getversioninfo(vtInternalName);
end;

function TCustomVersionInfo.GetLegalCopyright: string;
begin
  result := getversioninfo(vtLegalCopyright);
end;

function TCustomVersionInfo.GetLegalTradeMark: string;
begin
  result := getversioninfo(vtLegalTradeMark);
end;

function TCustomVersionInfo.GetOriginalFileName: string;
begin
  result := getversioninfo(vtOriginalFileName);
end;

function TCustomVersionInfo.GetProductName: string;
begin
  result := getversioninfo(vtProductName);
end;

function TCustomVersionInfo.GetProductVersion: string;
begin
  result := getversioninfo(vtProductVersion);
end;

function TCustomVersionInfo.GetComments: string;
begin
  result := getversioninfo(vtComments);
end;

function TCustomVersionInfo.GetVersionInfo(VersionType: TVersionType): string;
begin
  result := FVersionInfo[ord(VersionType)];
end;

procedure TCustomVersionInfo.SavetoFile(Filename: String);
begin

end;

procedure TCustomVersionInfo.SetIntFileVersion(Main, Sub, Release, Build: Word);
begin
  FIntVersionInfo[0].MainV := Main;
  FIntVersionInfo[0].SubV := Sub;
  FIntVersionInfo[0].Release := Release;
  FIntVersionInfo[0].Build := Build;
end;

procedure TCustomVersionInfo.SetIntProductVersion(Main, Sub, Release,
  Build: Word);
begin
  FIntVersionInfo[1].MainV := Main;
  FIntVersionInfo[1].SubV := Sub;
  FIntVersionInfo[1].Release := Release;
  FIntVersionInfo[1].Build := Build;
end;

procedure TCustomVersionInfo.SetVersionInfo(VersionType: TVersionType;
  const Value: String);
begin
  FVersionInfo[ord(VersionType)] := Value;
end;

{ TxxVersionInfo }

procedure TPEVersionInfo.LoadFromFile(Filename: string);
var
  sVersionType, rootinfo: string;
  VersionInfoSize, VersionInfoValueSize: DWord;
  i: Integer;
  VersionInfo, VersionInfoValue, Key: pointer;
begin
  VersionInfoSize := GetFileVersionInfoSize(PChar(Filename), VersionInfoSize);
  if (VersionInfoSize > 0) then
  begin
    VersionInfo := AllocMem(VersionInfoSize);
    GetFileVersionInfo(PChar(Filename), 0, VersionInfoSize, VersionInfo);
    VerQueryValue(VersionInfo, '\', VersionInfoValue, VersionInfoValueSize);
    if VersionInfoValueSize <> 0 then
    begin
      with TVSFixedFileInfo(VersionInfoValue^) do
      begin
        FIntVersionInfo[0].MainV := HiWord(dwFileVersionMS);
        FIntVersionInfo[0].SubV := LoWord(dwFileVersionMS);
        FIntVersionInfo[0].Release := HiWord(dwFileVersionLS);
        FIntVersionInfo[0].Build := LoWord(dwFileVersionLS);
        FIntVersionInfo[1].MainV := HiWord(dwProductVersionMS);
        FIntVersionInfo[1].SubV := LoWord(dwProductVersionMS);
        FIntVersionInfo[1].Release := HiWord(dwProductVersionLS);
        FIntVersionInfo[1].Build := LoWord(dwProductVersionLS);
        FFileOS := dwFileOS;
        FFileType := dwFileType;
        FFileFlags := dwFileFlags;
      end;
    end;
    if VerQueryValue(VersionInfo, '\VarFileInfo\Translation', VersionInfoValue,
      VersionInfoValueSize) then
    begin
      FLangID := LoWord(VersionInfoValue^);
      FCharset := HiWord(Integer(VersionInfoValue^));
      rootinfo := 'StringFileInfo\' + inttohex(FLangID, 4) + inttohex(FCharset, 4) + '\';
      for i := 0 to ord( High(TVersionType)) do
      begin
        sVersionType := GetEnumName(TypeInfo(TVersionType), i);
        sVersionType := Copy(sVersionType, 3, length(sVersionType));
        Key := PChar(rootinfo + sVersionType);
        if VerQueryValue(VersionInfo, Key, pointer(VersionInfoValue), VersionInfoValueSize) then
          FVersionInfo[i] := PChar(VersionInfoValue);
      end;
      FreeMem(VersionInfo, VersionInfoSize);
    end;
  end;
end;

{ TRCVersionInfo }

constructor TRCVersionInfo.Create;
begin
  inherited;
  InnerText := TStringList.Create;
end;

destructor TRCVersionInfo.Destroy;
begin
  InnerText.Free;
  inherited;
end;

procedure TRCVersionInfo.LoadFromFile(Filename: String);
var
  sl: TStringlist;
  i, p, blockstart, blockend: Integer;
  s1, s2, s3: String;
begin
  sl := TStringlist.Create;
  InnerText.Clear;
  try
    sl.LoadFromFile(Filename);
    i := 0;
    while i < sl.Count do
    begin
      s3 := trim(sl.strings[i]);
      p := pos(' ', s3);
      s1 := trim(Copy(s3, 1, p - 1));
      delete(s3, 1, p);
      p := pos(' ', s3);
      s2 := trim(Copy(s3, 1, p - 1));
      delete(s3, 1, p);
      s3 := trim(s3);
      if s3 <> '' then
      begin
        if lowercase(s3) = 'versioninfo' then
        begin
          blockstart := 0;
          blockend := 0;
          inc(i);
          while ((blockstart = 0) or (blockstart <> blockend)) and (i<sl.Count) do
          begin
            InnerText.Add(sl[i]);
            s3 := lowercase(trim(sl[i]));
            if (s3 = 'begin') or (s3 = '{') then
              inc(blockstart);
            if (s3 = 'end') or (s3 = '}') then
              inc(blockend);
            inc(i);
          end;
          ParseVersion;
        end
      end;
      inc(i);
    end;
  finally
    sl.free;
  end;
end;

function TRCVersionInfo.ParseVersionNumber(vStr: string): TIntVersionInfo;
var
  i, p, v: integer;
  s: string;
begin
  s := vStr;
  try
    for i := 0 to 3 do
    begin
      p := pos(',', s);
      if p = 0 then
        p := length(s) + 1;

      v := strToInt(trim(copy(s, 1, p - 1)));
      case i of
        0: Result.MainV := v;
        1: Result.SubV := v;
        2: Result.Release := v;
        3: Result.Build := v;
      end;
      delete(s, 1, p);
    end;
  except
    raise Exception.CreateFmt( StrInvalidVersionstring, [Classname, vStr]);
  end;
end;

procedure TRCVersionInfo.ParseVersion;
var
  i, p, p2: Integer;
  s, s2: string;
  ba: TIntVersionInfo;
begin
  for i := 0 to InnerText.Count - 1 do
  begin
    s := trim(InnerText[i]);
    p := pos('\0', s);
    if p=0 then begin
      p := Length( s);
      if s[p] <> '"' then
        inc(p);
    end;

    if uppercase(Copy(s, 1, 11)) = 'FILEVERSION' then
    begin
      p2 := pos(' ', s);
      ba := ParseVersionNumber(Copy(s, p2 + 1, length(s) - p2));
      FIntVersionInfo[0] := ba;
    end
    else if uppercase(Copy(s, 1, 14)) = 'PRODUCTVERSION' then
    begin
      p2 := pos(' ', s);
      ba := ParseVersionNumber(Copy(s, p2 + 1, length(s) - p2));
      FIntVersionInfo[1] := ba;
    end
    else if uppercase(Copy(s, 8, 11)) = 'PRODUCTNAME' then
      VersionInfo[ vtProductName] := trim(Copy(s, 23, p - 23))
    else if uppercase(Copy(s, 8, 11)) = 'COMPANYNAME' then
      VersionInfo[ vtCompanyName] := trim(Copy(s, 23, p - 23))
    else if uppercase(Copy(s, 8, 15)) = 'FILEDESCRIPTION' then
      VersionInfo[ vtFileDescription] := trim(Copy(s, 27, p - 27))
    else if uppercase(Copy(s, 8, 12)) = 'INTERNALNAME' then
      VersionInfo[ vtInternalName] := trim(Copy(s, 24, p - 24))
    else if uppercase(Copy(s, 8, 14)) = 'LEGALCOPYRIGHT' then
      VersionInfo[ vtLegalCopyright] := trim(Copy(s, 26, p - 26))
    else if uppercase(Copy(s, 8, 16)) = 'ORIGINALFILENAME' then
      VersionInfo[ vtOriginalFileName] := trim(Copy(s, 28, p - 28))
    else if uppercase(Copy(s, 8, 11)) = 'TRANSLATION' then;
  end;
end;

procedure TRCVersionInfo.SavetoFile(Filename: String);
var sl:TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add(         'LANGUAGE 0, 0');
    sl.Add(         '1 VERSIONINFO');
    sl.Add( Format( 'FILEVERSION %d,%d,%d,%d', [FIntVersionInfo[0].MainV,FIntVersionInfo[0].SubV, FIntVersionInfo[0].Release, FIntVersionInfo[0].Build]));
    sl.Add( Format( 'PRODUCTVERSION %d,%d,%d,%d', [FIntVersionInfo[1].MainV,FIntVersionInfo[1].SubV, FIntVersionInfo[1].Release, FIntVersionInfo[1].Build]));
    sl.Add(         'BEGIN');
    sl.Add(         '  BLOCK "StringFileInfo"');
    sl.Add(         '  BEGIN');
    sl.Add(         '    BLOCK "040904E4"');
    sl.Add(         '    BEGIN');
    sl.Add( Format( '      VALUE "FileVersion", "%d.%d.%d.%d\0"', [FIntVersionInfo[0].MainV,FIntVersionInfo[0].SubV, FIntVersionInfo[0].Release, FIntVersionInfo[0].Build]));
    sl.Add( Format( '      VALUE "ProductVersion", "%d.%d.%d.%d\0"', [FIntVersionInfo[1].MainV,FIntVersionInfo[1].SubV, FIntVersionInfo[1].Release, FIntVersionInfo[1].Build]));
    sl.Add( Format( '      VALUE "ProductName", "%s\0"', [ VersionInfo[ vtProductName]]));
    sl.Add( Format( '      VALUE "CompanyName", "%s\0"', [ VersionInfo[ vtCompanyName]]));
    sl.Add( Format( '      VALUE "FileDescription", "%s\0"', [ VersionInfo[ vtFileDescription]]));
    sl.Add( Format( '      VALUE "InternalName", "%s\0"', [ VersionInfo[ vtInternalName]]));
    sl.Add( Format( '      VALUE "LegalCopyright", "%s\0"', [ VersionInfo[ vtLegalCopyright]]));
    sl.Add( Format( '      VALUE "OriginalFilename", "%s\0"', [ VersionInfo[ vtOriginalFileName]]));
    sl.Add(         '    END');
    sl.Add(         '  END');
    sl.Add(         '  BLOCK "VarFileInfo"');
    sl.Add(         '  BEGIN');
    sl.Add(         '    VALUE "Translation", 0x0409, 0x04E4');
    sl.Add(         '  END');
    sl.Add(         'END');

    sl.SaveToFile( Filename);
  finally
    sl.Free;
  end;
end;

procedure TRCVersionInfo.SetInnerText(const Value: TStrings);
begin
  FInnerText := Value;
end;

end.
