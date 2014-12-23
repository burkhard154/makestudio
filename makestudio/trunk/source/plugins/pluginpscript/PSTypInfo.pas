{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsplugintemplate_Module.pas

The Initial Developer of the original code (JEDI Make) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2006/04/21  BSchranz  - Plugin Script created
2006/05/01  BSchranz  - PSTypInfo and PSUtils added for browser functionallity
2005/05/27  BSchranz  - Released

------------------------------------------------------------------------------}
unit PSTypInfo;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
   Windows, Messages, SysUtils, Variants, Classes, uPSCompiler, uPSComponent,
  uPSUtils;

const
  //Image Index of Info list
  pstiFolder       = 0;
  pstiFunction     = 1;
  pstiProcedure    = 2;
  pstiProperty     = 3;
  pstiConstructor  = 4;
  pstiVariable     = 5;
  pstiConstant     = 6;
  pstiUnknown      = 7;
  pstiType         = 8;
  pstiClass        = 9;

  pstkProcedure = 'procedure';
  pstkFunction = 'function';
  pstkProperty = 'property';
  pstkConstructor = 'constructor';

type
  TPSTypInfoList = Class;

  TPSTypInfoItem = Class( TCollectionItem)
  private
    FItems: TPSTypInfoList;
    FImageIndex: Integer;
    FText : String;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ImageIndex:Integer read FImageIndex write FImageIndex;
    property Items:TPSTypInfoList read FItems write FItems;
    property Text:String read FText write FText;
  end;

  TPSTypInfoList = Class( TCollection)
  private
    function GetItem(Index: Integer): TPSTypInfoItem;
    procedure SetItem(Index: Integer; const Value: TPSTypInfoItem);
  public
    constructor Create;
    function Add: TPSTypInfoItem;
    procedure AddItems( aItems:TPSTypInfoList);
    function FindItemID(ID: Integer): TPSTypInfoItem;
    function Insert(Index: Integer): TPSTypInfoItem;
    property Items[ Index:Integer]:TPSTypInfoItem read GetItem write SetItem; default;
  end;

  TPSTypInfo = class( TPersistent)
  private
    fCompiler: TPSPascalCompiler;
    function BuildDeclStr( aType, ProcName: String; Decl: TPSParametersDecl): String;
    function PIfRVariantToStr( Value:PIfRVariant):String;
    function DeleteKeyWord(S: String): String;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function IsClass(aClass: String): Boolean;
    procedure GetProcList( aProcs:TPSTypInfoList);
    procedure GetTypeList( aTypes:TPSTypInfoList);
    procedure GetVarList( aVars:TPSTypInfoList);
    procedure GetConstList( aConsts:TPSTypInfoList);
    procedure GetClassMembers( aClass:String; aClassMembers:TPSTypInfoList); overload;
    procedure GetClassMembers( aClass:TPSCompileTimeClass; aClassMembers:TPSTypInfoList; ShowConstructor:Boolean); overload;
    procedure GetAllReferences( aReferences: TPSTypInfoList);
    property Compiler:TPSPascalCompiler read fCompiler write fCompiler;
  end;

implementation

{ TPSTypInfo }

constructor TPSTypInfo.Create;
begin
  inherited;
  Compiler := nil;
end;

function TPSTypInfo.BuildDeclStr( aType, ProcName:String; Decl:TPSParametersDecl):String;
var ii, kk:Integer;
    ResType:String;
begin
  //Get Function or Procedure and Return Type
  ResType := '';
  
  if Assigned( Decl.Result) then
    ResType := Decl.Result.OriginalName;

  if aType<>'' then
    Result := aType + ' ' + ProcName
  else
    Result := ProcName;

  if Decl.ParamCount>0 then begin
    Result := Result + '(';

    for ii:=0 to Decl.ParamCount-1 do begin

      if Assigned( Decl.Params[ii].aType) then begin
        if Assigned( Decl.Params[ii].aType.Attributes) then
          for kk:=0 to Decl.Params[ii].aType.Attributes.Count-1 do
            Result := Result + ' ' + Decl.Params[ii].aType.Attributes.Items[ kk].AType.OrgName;

        Result := Result + ' ' + Decl.Params[ii].OrgName + ':' + Decl.Params[ii].aType.OriginalName;

      end
      else begin
        Result := Result + ' ' + Decl.Params[ii].OrgName;
      end;

      if ii<Decl.ParamCount-1 then
        Result := Result + ';';
    end;

    Result := Result + ')';
  end;

  //Add function result
  if ResType<>'' then
    Result := Result + ':' + ResType;
end;

procedure TPSTypInfo.GetTypeList(aTypes: TPSTypInfoList);
var i:Integer;
    T:TPSType;
begin
  aTypes.Clear;
  for i:=0 to Compiler.GetTypeCount-1 do
    with aTypes.Add do begin
      T := Compiler.GetType( i);
      Text := Compiler.GetType( i).OriginalName;
      if T is TPSClassType then begin
        GetClassMembers( (T as TPSClassType).Cl, Items, true);
        ImageIndex := pstiClass;
      end
      else
        ImageIndex := pstiType;
    end;
end;

procedure TPSTypInfo.GetVarList(aVars: TPSTypInfoList);
var i:Integer;
begin
  aVars.Clear;
  for i:=0 to Compiler.GetVarCount-1 do begin
    With aVars.Add do begin
      Text := Compiler.GetVar( i).OrgName + ':' + Compiler.GetVar( i).aType.OriginalName;
      ImageIndex := pstiVariable;
    end;
  end;
end;

procedure TPSTypInfo.GetProcList(aProcs: TPSTypInfoList);
var i:Integer;
begin
  aProcs.Clear;
  for i:=0 to Compiler.GetRegProcCount-1 do
    with aProcs.Add do begin
      Text := BuildDeclStr( '', Compiler.GetRegProc( i).OrgName, Compiler.GetRegProc( i).Decl);
      if Assigned( Compiler.GetRegProc( i).Decl.Result) then
        ImageIndex := pstiFunction
      else
        ImageIndex := pstiProcedure;
    end;
end;

procedure TPSTypInfo.GetConstList(aConsts: TPSTypInfoList);
var i:Integer;
begin
  aConsts.Clear;
  for i:=0 to Compiler.GetConstCount-1 do
    with aConsts.Add do begin
      Text := Compiler.GetConst( i).OrgName + '=' +
         PIfRVariantToStr( Compiler.GetConst( i).Value);
      ImageIndex := pstiConstant;
    end;
end;

function TPSTypInfo.IsClass( aClass:String):Boolean;
begin
  Result := Compiler.FindClass( aClass)<>nil;
end;

procedure TPSTypInfo.GetClassMembers(aClass: TPSCompileTimeClass;
  aClassMembers: TPSTypInfoList; ShowConstructor:Boolean);
var i:Integer;
    sl:TPSTypInfoList;
begin
  aClassMembers.Clear;
  if aClass<>nil then begin
    for i:=0 to aClass.Count-1 do begin
      if aClass.Items[i] is TPSDelphiClassItemProperty then
        with aClassMembers.Add do begin
          Text := BuildDeclStr( '', aClass.Items[i].OrgName, aClass.Items[i].Decl);
          ImageIndex := pstiProperty;
        end;
      if aClass.Items[i] is TPSDelphiClassItemConstructor then
        if ShowConstructor then
          with aClassMembers.Add do begin
            Text := BuildDeclStr( 'constructor', aClass.Items[i].OrgName, aClass.Items[i].Decl);
            ImageIndex := pstiConstructor;
          end;
      if aClass.Items[i] is TPSDelphiClassItemMethod then
        with aClassMembers.Add do begin
          Text := BuildDeclStr( '', aClass.Items[i].OrgName, aClass.Items[i].Decl);
          if Assigned( aClass.Items[i].Decl.Result) then
            ImageIndex := pstiFunction
          else
            ImageIndex := pstiProcedure;
        end;
    end;

    //inherited classes
    sl := TPSTypInfoList.Create;
    try
      if aClass.ClassInheritsFrom<>nil then begin
        GetClassMembers( aClass.ClassInheritsFrom, sl, false);
        aClassMembers.AddItems( sl);
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TPSTypInfo.GetClassMembers(aClass: String; aClassMembers: TPSTypInfoList);
var c:TPSCompileTimeClass;
begin
  aClassMembers.Clear;
  c := Compiler.FindClass( aClass);
  if c<>nil then
    GetClassMembers( c, aClassMembers, true);
end;

destructor TPSTypInfo.Destroy;
begin

  inherited;
end;

function TPSTypInfo.PIfRVariantToStr(Value: PIfRVariant): String;
begin
  try
    case Value.FType.BaseType of
      btEnum: Result := IntToStr( Value.tu32);
      btU32, btS32: Result := IntToStr( Value.tu32);
      btU16, btS16: Result := IntToStr( Value.tu16);
      btU8, btS8: Result := IntToStr( Value.tu8);
      btSingle: Result := FloatToStr( Value.tsingle);
      btDouble: Result := FloatToStr( Value.tdouble);
      btExtended: Result := FloatToStr( Value.textended);
      btCurrency: Result := FloatToStr( Value.tcurrency);
      bts64: Result := IntToStr( Value.ts64);
      btChar: Result := Value.tchar;
      btString: Result := string(Value.tstring);
      btWideString: Result := widestring(Value.twidestring);
      else
        Result := '???';
    end;
  except
    On E:Exception do Result := E.Message;
  end;
end;

function TPSTypInfo.DeleteKeyWord( S:String):String;
begin
  Result := S;
  if pos( 'FUNCTION', UpperCase( S))>0 then
    Result := Copy( S, 10, Length( S)-9)
  else if pos( 'PROCEDURE', UpperCase( S))>0 then
    Result := Copy( S, 11, Length( S)-10)
  else if pos( 'CONSTRUCTOR', UpperCase( S))>0 then
    Result := Copy( S, 13, Length( S)-12)
  else if pos( 'PROPERTY', UpperCase( S))>0 then
    Result := Copy( S, 10, Length( S)-9);
end;

procedure TPSTypInfo.GetAllReferences(aReferences: TPSTypInfoList);
var it:TPSTypInfoItem;
begin
  aReferences.Clear;
  aReferences.BeginUpdate;
  try

    it := aReferences.Add;
    it.Text := 'Procedures and Functions';
    it.ImageIndex := pstiFolder;
    GetProcList( it.Items);

    it := aReferences.Add;
    it.Text := 'Types and Classes';
    it.ImageIndex := pstiFolder;
    GetTypeList( it.Items);

    it := aReferences.Add;
    it.Text := 'Variables';
    it.ImageIndex := pstiFolder;
    GetVarList( it.Items);

    it := aReferences.Add;
    it.Text := 'Constants';
    it.ImageIndex := pstiFolder;
    GetConstList( it.Items);

  finally
    aReferences.EndUpdate;
  end;
end;

{ TPSTypInfoItem }

procedure TPSTypInfoItem.Assign(Source: TPersistent);
begin
  if Source is TPSTypInfoItem then begin
    Text := (Source as TPSTypInfoItem).Text;
    ImageIndex := (Source as TPSTypInfoItem).ImageIndex;
    Items.Assign( (Source as TPSTypInfoItem).Items);
  end
  else
    Raise Exception.Create( 'Assign TPSTypInfoItem - '+ Source.ClassType.ClassName + ' cannot be assigned!');
end;

constructor TPSTypInfoItem.Create(Collection: TCollection);
begin
  inherited Create( Collection);
  FItems := TPSTypInfoList.Create;
  FText := '';
  FImageIndex := pstiUnknown;
end;

destructor TPSTypInfoItem.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TPSTypInfoList }

function TPSTypInfoList.Add: TPSTypInfoItem;
begin
  Result := inherited Add as TPSTypInfoItem;
end;

procedure TPSTypInfoList.AddItems(aItems: TPSTypInfoList);
var i:Integer;
begin
  for i:=0 to aItems.Count-1 do
    Add.Assign( aItems[i]);
end;

constructor TPSTypInfoList.Create;
begin
  inherited Create( TPSTypInfoItem);
end;

function TPSTypInfoList.FindItemID(ID: Integer): TPSTypInfoItem;
begin
  Result := inherited FindItemID( ID) as TPSTypInfoItem;
end;

function TPSTypInfoList.GetItem(Index: Integer): TPSTypInfoItem;
begin
  Result := inherited GetItem( Index) as TPSTypInfoItem;
end;

function TPSTypInfoList.Insert(Index: Integer): TPSTypInfoItem;
begin
  Result := inherited Insert( Index) as TPSTypInfoItem;
end;

procedure TPSTypInfoList.SetItem(Index: Integer; const Value: TPSTypInfoItem);
begin
  inherited SetItem( Index, Value);
end;

end.
