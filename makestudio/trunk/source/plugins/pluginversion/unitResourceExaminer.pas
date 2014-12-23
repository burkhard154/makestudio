(*======================================================================*
 | unitResourceExaminer                                                 |
 |                                                                      |
 | unit contains TResourceExaminer helper class for enumerating         |
 | resource modules.                                                    |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      11/06/2003  CPWW  Original                                  |
 *======================================================================*)

unit unitResourceExaminer;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses Windows, Classes, SysUtils, unitResourceDetails, contnrs, Graphics, unitPEFile;

type
TResourceExaminer = class;
TResExamType = class;
TResExamName = class;
TResExamLang = class;

//---------------------------------------------------------------------
// TResExamElement - Base class for resource examiner elements
TResExamElement = class
private
  function GetElement(idx: Integer): TResExamElement;
protected
  fOwner : TObject;
  fElements : TObjectList;
  function GetCount : Integer;
  function GetDisplayName: string; virtual; abstract;
public
  constructor Create; overload;
  destructor Destroy; override;
  property Count : Integer read GetCount;
  property Element [idx : Integer] : TResExamElement read GetElement;
  property DisplayName : string read GetDisplayName;
end;

//---------------------------------------------------------------------
// TResExamResource - Base class for resource examiner elements that
// reference resource details.
TResExamResource = class (TResExamElement)
private
  fResource : TResourceDetails;
public
  constructor Create (AOwner : TObject; AResource : TResourceDetails);
  property ResourceDetails : TResourceDetails read fResource;
end;

TResExamImport = class (TResExamElement)
private
  fBaseAddr : PChar;
  fImageImportDirectory : PImageImportDirectory;
  function GetImportName: string;
  function GetExaminer: TResourceExaminer;
protected
  function GetDisplayName : string; override;
public
  constructor Create (AOwner : TObject; ABaseAddr : PChar; AImageImportDirectory : PImageImportDirectory);
  property ImportDirectory : PImageImportDirectory read fImageImportDirectory;
  property ImportName : string read GetImportName;
  property Examiner : TResourceExaminer read GetExaminer;

  property BaseAddress : PChar read fBaseAddr;
end;

//---------------------------------------------------------------------
// TResExamIconCursor - Resource examiner element for icons & cursors
TResExamIconCursor = class (TResExamResource)
private
  function GetOwner: TResExamLang;
protected
  function GetDisplayName: string; override;
public
  property Owner : TResExamLang read GetOwner;
end;

//---------------------------------------------------------------------
// TResExamLang - Resource examiner element language
TResExamLang = class (TResExamResource)
private
  function GetOwner: TResExamName;
  function GetName: LCID;
protected
  function GetDisplayName : string; override;
public
  property Name : LCID read GetName;
  property Owner : TResExamName read GetOwner;
end;

//---------------------------------------------------------------------
// TResExamName - Resource examiner element name
TResExamName = class (TResExamElement)
private
  fName : string;
  function GetResExamLang(idx: Integer): TResExamLang;
  function GetOwner: TResExamType;
protected
  function GetDisplayName : string; override;
public
  constructor Create (AOwner : TResExamType; const AName : string);

  property Name : string read fName;
  property Owner : TResExamType read GetOwner;
  property ResExamLang [idx : Integer] : TResExamLang read GetResExamLang;
end;

//---------------------------------------------------------------------
// TResExamName - Resource examiner element type
TResExamType = class (TResExamElement)
private
  fName : string;
  function GetResExamName(idx: Integer): TResExamName;
  function GetOwner: TResourceExaminer;
protected
  function GetDisplayName : string; override;
public
  constructor Create (AOwner : TResourceExaminer; const AName : string);
  property Name : string read fName;
  property Owner : TResourceExaminer read GetOwner;
  property ResExamName [idx : Integer] : TResExamName read GetResExamName;
end;

TResExamSection = class (TResExamElement)
private
  fName: string;
protected
  function GetDisplayName: string; override;
public
  constructor Create (const AName : string);
  property Name : string read fName write fName;
end;

TExportResExamSection = class (TResExamSection)
private
  fBaseAddr : PChar;
  function GetExaminer: TResourceExaminer;
  function GetExportCount: Integer;
  function GetExportName(idx: Integer): string;
  function GetExportOrdinal(idx: Integer): Integer;
public
  constructor Create (AOwner : TObject; BaseAddr : PChar);

  property ExportCount : Integer read GetExportCount;
  property ExportName [idx : Integer] : string read GetExportName;
  property ExportOrdinal [idx : Integer] : Integer read GetExportOrdinal;
end;

//---------------------------------------------------------------------
// TResourceExaminer class
TResourceExaminer = class
private
  fResourceModule : TResourceModule;
  fSections : TObjectList;

  fOwnsModule : boolean;
  function GetResource(idx: Integer): TResourceDetails;
  function GetResourceCount: Integer;
  function GetSection(idx: Integer): TResExamSection;
  function GetSectionCount: Integer;
  function GetExportCount: Integer;
  function GetImportCount: Integer;
  function GetImport (idx : Integer) : PImageImportDirectory;
  function GetExportName(idx: Integer): string;
  function GetExportOrdinal(idx: Integer): Integer;
protected
  constructor Create (AResourceModule : TResourceModule; AOwnsModule : boolean; DontExamine : boolean); overload;

  property ResourceCount : Integer read GetResourceCount;
  property Resource [idx : Integer] : TResourceDetails read GetResource;
  property ResourceModule : TResourceModule read fResourceModule;

  property ExportCount : Integer read GetExportCount;
  property ExportName [idx : Integer] : string read GetExportName;
  property ExportOrdinal [idx : Integer] : Integer read GetExportOrdinal;

  property ImportCount : Integer read GetImportCount;
  property Import [idx : Integer] : PImageImportDirectory read GetImport;
public
  constructor Create (AResourceModule : TResourceModule); overload;
  constructor Create (const FileName : string); overload;

  destructor Destroy; override;

  procedure Examine;

  property SectionCount : Integer read GetSectionCount;
  property Section [idx : Integer] : TResExamSection read GetSection;
end;

function PixelFormatToString (pf : TPixelFormat) : string;

implementation

uses unitResourceGraphics;

const
  RT_HTML = MakeIntResource(23);
  RT_XPMANIFEST = MakeIntResource (24);

resourcestring
  rstLanguageNeutral = 'Language Neutral';
  rstBitmap       = 'Bitmap';
  rstIcon         = 'Icon';
  rstCursor       = 'Cursor';
  rstMenu         = 'Menu';
  rstDialog       = 'Dialog';
  rstAccelerator  = 'Accelerator';
  rstString       = 'String Table';
  rstRCData       = 'RC Data';
  rstMessageTable = 'MessageTable';
  rstVersion      = 'Version';
  rstGroupCursor  = 'Cursor Group';
  rstGroupIcon    = 'Icon Group';
  rstHTML         = 'HTML';
  rstXPManifest   = 'XP Theme Manifest';

  rst1Bit         = '2 Colour';
  rst4Bit         = '16 Colour';
  rst8Bit         = '256 Colour';
  rst15Bit        = '15 Bit Colour';
  rst16Bit        = '16 Bit Colour';
  rst24Bit        = '24 Bit Colour';
  rst32Bit        = '32 Bit Colour';


(*----------------------------------------------------------------------*
 | function PixelFormatToString () : string                             |
 |                                                                      |
 | Return string representation of a TPixelFormat                       |
 |                                                                      |
 | Parameters:                                                          |
 |   pf : TPixelFormat          The pixel format to use                 |
 *----------------------------------------------------------------------*)
function PixelFormatToString (pf : TPixelFormat) : string;
begin
  case pf of
    pf1Bit : result := rst1Bit;
    pf4Bit : result := rst4Bit;
    pf8Bit : result := rst8Bit;
    pf15Bit : result := rst15Bit;
    pf16Bit : result := rst16Bit;
    pf32Bit : result := rst32Bit
  end
end;

(*----------------------------------------------------------------------*
 | function GetTypeName () : string                                     |
 |                                                                      |
 | Return display name for a resource type                              |
 |                                                                      |
 | Parameters:                                                          |
 |   const tp : string          The resource type                       |
 *----------------------------------------------------------------------*)
function GetTypeName (const tp : string) : string;
var
  i : Integer;
begin
  i := ResourceNameToInt (tp);

  case i of
    Integer (RT_BITMAP)       : result := rstBitmap;
    Integer (RT_ICON)         : result := rstIcon;
    Integer (RT_CURSOR)       : result := rstCursor;
    Integer (RT_MENU)         : result := rstMenu;
    Integer (RT_DIALOG)       : result := rstDialog;
    Integer (RT_STRING)       : result := rstString;
    Integer (RT_ACCELERATOR)  : Result := rstAccelerator;
    Integer (RT_RCDATA)       : result := rstRCData;
    Integer (RT_MESSAGETABLE) : result := rstMessageTable;
    Integer (RT_VERSION)      : result := rstVersion;
    Integer (RT_GROUP_CURSOR) : result := rstGroupCursor;
    Integer (RT_GROUP_ICON)   : result := rstGroupIcon;
    Integer (RT_XPMANIFEST)   : result := rstXPManifest;
    Integer (RT_HTML)         : result := rstHTML;
    else
      result := tp
  end
end;

(*----------------------------------------------------------------------*
 | function GetLangName () : string                                     |
 |                                                                      |
 | Return display name for a language ID                                |
 |                                                                      |
 | Parameters:                                                          |
 |   language : Integer                 The language ID                 |
 *----------------------------------------------------------------------*)
function GetLangName (language : Integer) : string;
begin
  if language = 0 then
    result := rstLanguageNeutral
  else
    result := Languages.NameFromLocaleID [language]
end;

{ TResourceExaminer }

(*----------------------------------------------------------------------*
 | constructor TResourceExaminer.Create                                 |
 |                                                                      |
 | protected base constructor                                           |
 |                                                                      |
 | Parameters:                                                          |
 |   AResourceModule: TResourceModule   The module to examine           |
 |                                                                      |
 |   AOwnsModule : boolean              If this is true we take         |
 |                                      ownership of the module, and    |
 |                                      delete it when we're deleted.   |
 |                                                                      |
 |  DontExamine : boolean               If this is true, dont examine   |
 |                                      the module in the constructor.  |
 *----------------------------------------------------------------------*)
constructor TResourceExaminer.Create(AResourceModule: TResourceModule; AOwnsModule : boolean; DontExamine : boolean);
begin
  fSections := TObjectList.Create;
  fResourceModule := AResourceModule;
  fOwnsModule := AOwnsModule;
  if not DontExamine then
    Examine;
end;

(*----------------------------------------------------------------------*
 | constructor TResourceExaminer.Create                                 |
 |                                                                      |
 | public constructor #1.  Initialize a TResourceExaminer from          |
 | resources in a PE file.                                              |
 |                                                                      |
 | Parameters:                                                          |
 |   const FileName: string     The name of the PE file                 |
 *----------------------------------------------------------------------*)
constructor TResourceExaminer.Create(const FileName: string);
begin
  Create (TPEResourceModule.Create, True, True);
  fResourceModule.LoadFromFile(FileName);
  Examine;
end;

(*----------------------------------------------------------------------*
 | constructor TResourceExaminer.Create                                 |
 |                                                                      |
 | public constructor #2.  Initialize a TResourceExaminer from a        |
 | TResourceModule                                                      |
 |                                                                      |
 | Parameters:                                                          |
 |   const AResourceModule      The resource module to examinme         |
 *----------------------------------------------------------------------*)
constructor TResourceExaminer.Create(AResourceModule: TResourceModule);
begin
  Create (AResourceModule, False, False);
end;

destructor TResourceExaminer.Destroy;
begin
  fSections.Free;
  if fOwnsModule then
    fResourceModule.Free;
  inherited;
end;

procedure TResourceExaminer.Examine;
type
  TImageImportByName = record
    hint : WORD;
    name : array [0..0] of char;
  end;
  PImageImportByName = ^TImageImportByName;

var
  i, j : Integer;
  currentType : TResExamType;
  currentName : TResExamName;
  currentLang : TResExamLang;
  res : TResourceDetails;
  grp : TIconCursorGroupResourceDetails;
  section : TResExamSection;
  sectionData : PChar;
  imp : PImageImportDirectory;
  impSection : TResExamImport;

begin
  fSections.Clear;

  currentType := Nil;
  currentName := Nil;

  if ExportCount > 0 then
  begin
    sectionData := TPEModule (fResourceModule).ExportSectionData;
    section := TExportResExamSection.Create (self, sectionData);
    fSections.Add(section);
  end;

  if ImportCount > 0 then
  begin
    sectionData := TPEModule (fResourceModule).ImportSectionData;
    section := TResExamSection.Create ('Imported Functions');
    fSections.Add(section);

    for i := 0 to ImportCount - 1 do
    begin
      imp := Import [i];
      impSection := TResExamImport.Create(self, sectionData, imp);
      section.fElements.Add(impSection);
    end
  end;

  if ResourceCount > 0 then
  begin
    section := TResExamSection.Create ('Resources');
    fSections.Add(section);

    for i := 0 to ResourceCount - 1 do
    begin
      res := Resource [i];
      if (currentType = Nil) or (currentType.Name <> res.ResourceType) then
      begin
        currentName := Nil;
        if res is TIconCursorResourceDetails then
          Continue;
        currentType := TResExamType.Create(self, res.ResourceType);
        section.fElements.Add(currentType)
      end;

      if (currentName = Nil) or (currentName.Name <> res.ResourceName) then
      begin
        currentName := TResExamName.Create(currentType, res.ResourceName);
        currentType.fElements.Add(currentName)
      end;

      currentLang := TResExamLang.Create(currentName, res);
      currentName.fElements.Add(currentLang);

      if res is TIconCursorGroupResourceDetails then
      begin
        grp := TIconCursorGroupResourceDetails (res);

        currentLang.fElements := TObjectList.Create;
        for j := 0 to grp.ResourceCount - 1 do
          currentLang.fElements.Add(TResExamIconCursor.Create(currentLang, grp.ResourceDetails [j]))
      end
    end
  end
end;

function TResourceExaminer.GetExportCount: Integer;
begin
  if fResourceModule is TPEModule then
    result := TPEModule (fResourceModule).ExportCount
  else
    result := 0
end;

function TResourceExaminer.GetExportName(idx: Integer): string;
var
  ord : DWORD;
begin
  if fResourceModule is TPEModule then
    TPEModule (fResourceModule).GetExportDetails(idx, result, ord)
  else
    result := ''
end;

function TResourceExaminer.GetExportOrdinal(idx: Integer): Integer;
var
  nm : string;
  ord : DWORD;
begin
  if fResourceModule is TPEModule then
  begin
    TPEModule (fResourceModule).GetExportDetails(idx, nm, ord);
    result := ord
  end
  else
    result := 0
end;

function TResourceExaminer.GetImport(idx: Integer): PImageImportDirectory;
begin
  if fResourceModule is TPEModule then
    result := TPEModule (fResourceModule).Import [idx]
  else
    result := Nil
end;

function TResourceExaminer.GetImportCount: Integer;
begin
  if fResourceModule is TPEModule then
    result := TPEModule (fResourceModule).ImportCount
  else
    result := 0
end;

function TResourceExaminer.GetResource(idx: Integer): TResourceDetails;
begin
  result := fResourceModule.ResourceDetails [idx]
end;

function TResourceExaminer.GetResourceCount: Integer;
begin
  result := fResourceModule.ResourceCount
end;

{ TResExamLang }

function TResExamLang.GetDisplayName: string;
begin
  result := GetLangName (Name);
end;

function TResExamLang.GetName: LCID;
begin
  result := fResource.ResourceLanguage;
end;

function TResExamLang.GetOwner: TResExamName;
begin
  result := fOwner as TResExamName;
end;

{ TResExamName }

constructor TResExamName.Create(AOwner: TResExamType; const AName: string);
begin
  inherited Create;
  fOwner := AOwner;
  fName := AName;
end;

function TResExamName.GetDisplayName: string;
begin
  result := Name;
end;

function TResExamName.GetOwner: TResExamType;
begin
  result := TResExamType (fOwner);
end;

function TResExamName.GetResExamLang(idx: Integer): TResExamLang;
begin
  result := TResExamLang (fElements [idx])
end;

{ TResExamType }

constructor TResExamType.Create(AOwner: TResourceExaminer;
  const AName: string);
begin
  inherited Create;
  fOwner := AOwner;
  fName := AName;
end;

function TResExamType.GetDisplayName: string;
begin
  result := GetTypeName (Name);
end;

function TResExamType.GetOwner: TResourceExaminer;
begin
  result := TResourceExaminer (fOwner);
end;

function TResExamType.GetResExamName(idx: Integer): TResExamName;
begin
  result := TResExamName (fElements [idx]);
end;

{ TResExamElement }

constructor TResExamElement.Create;
begin
  fElements := TObjectList.Create;
end;

destructor TResExamElement.Destroy;
begin
  fElements.Free;
  inherited;
end;

function TResExamElement.GetCount: Integer;
begin
  if Assigned (fElements) then result := fElements.Count else result := 0
end;

function TResExamElement.GetElement(idx: Integer): TResExamElement;
begin
  if Assigned (fElements) then result := TResExamElement (fElements [idx]) else result := Nil
end;

{ TResExamResource }

constructor TResExamResource.Create(AOwner: TObject; AResource: TResourceDetails);
begin
  fOwner := AOwner;
  fResource := AResource;
  if fResource is TIconCursorGroupResourceDetails then
    fElements := TObjectList.Create
end;

{ TResExamIconCursor }

function TResExamIconCursor.GetDisplayName: string;
var
  res : TIconCursorResourceDetails;
  pf : string;
begin
  res := TIconCursorResourceDetails (fResource);

  pf := PixelFormatToString (res.PixelFormat);
  result := Format ('%dx%d %s', [res.Width, res.Height, pf]);
end;

function TResExamIconCursor.GetOwner: TResExamLang;
begin
  result := fOwner as TResExamLang
end;

function TResourceExaminer.GetSection(idx: Integer): TResExamSection;
begin
  result := TResExamSection (fSections [idx])
end;

function TResourceExaminer.GetSectionCount: Integer;
begin
  result := fSections.Count
end;

{ TResExamSection }

constructor TResExamSection.Create(const AName: string);
begin
  inherited Create;
  fName := AName;
end;

function TResExamSection.GetDisplayName: string;
begin
  result := Name
end;

{ TResExamImport }

constructor TResExamImport.Create(AOwner: TObject;ABaseAddr : PChar;
  AImageImportDirectory: PImageImportDirectory);
begin
  inherited Create;
  fOwner := AOwner;
  fBaseAddr := ABaseAddr;
  fImageImportDirectory := AImageImportDirectory
end;

function TResExamImport.GetDisplayName: string;
begin
  result := ImportName
end;

function TResExamImport.GetExaminer: TResourceExaminer;
begin
  result := TResourceExaminer (fOwner);
end;

function TResExamImport.GetImportName: string;
begin
  result := PChar (fBaseAddr) + fImageImportDirectory^.Name
end;

{ TExportResExamSection }

constructor TExportResExamSection.Create(AOwner: TObject; BaseAddr: PChar);
begin
  inherited Create ('Exported Functions');
  fOwner := AOwner;
  fBaseAddr := BaseAddr;
end;

function TExportResExamSection.GetExaminer: TResourceExaminer;
begin
  result := fOwner as TResourceExaminer;
end;

function TExportResExamSection.GetExportCount: Integer;
begin
  result := GetExaminer.ExportCount
end;

function TExportResExamSection.GetExportName(idx: Integer): string;
begin
  result := GetExaminer.ExportName [idx];
end;

function TExportResExamSection.GetExportOrdinal(idx: Integer): Integer;
begin
  result := GetExaminer.ExportOrdinal [idx];
end;

end.
