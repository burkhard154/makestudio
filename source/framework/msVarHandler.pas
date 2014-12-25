(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: msVarHandler.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
2005/02/04  USchuster - preparations for check in
2005/03/09  BSchranz  - function xVarToInt(V: Variant): Integer; added
2005/04/09  BSchranz  - Translated to englisch
-----------------------------------------------------------------------------*)

unit msVarHandler;

{$I jedi.inc}

interface

uses
  {$IFDEF DELPHI6_UP}
  Variants,
  {$ENDIF DELPHI6_UP}
  {$IFDEF DELPHIXE2_UP}
  System.UITypes,
  {$ENDIF}
  Classes, Contnrs, ActiveX, msTLB, SysUtils, ComObj, Dialogs, Forms, Controls, ExtCtrls;

const
  stcVarTextDelimiter = '%';

resourcestring
  stvDefaultVarname = 'Variable';
  stvvarEmpty    = 'Empty';
  stvvarNull     = 'Null';
  stvvarSmallint = 'Smallint';
  stvvarInteger  = 'Integer';
  stvvarSingle   = 'Float';
  stvvarDouble   = 'Double';
  stvvarCurrency = 'Currency';
  stvvarDate     = 'Date';
  stvvarOleStr   = 'OleStr';
  stvvarDispatch = 'Dispatch';
  stvvarError    = 'Error';
  stvvarBoolean  = 'Boolean';
  stvvarVariant  = 'Variant';
  stvvarUnknown  = 'Unknown';
  stvvarShortInt = 'ShortInt';
  stvvarByte     = 'Byte';
  stvvarWord     = 'Word';
  stvvarLongWord = 'LongWord';
  stvvarInt64    = 'Int64';
  stvvarStrArg   = 'StrArg';
  stvvarString   = 'String';
  stvvarAny      = 'Any';
  stvvarTypeMask = 'TypeMask';
  stvvarArray    = 'Array';
  stvvarByRef    = 'ByRef';
  stvvarDChannel = 'DChannel';
  stvvarDText    = 'DText';
  stvvarTrue     = 'True';
  stvvarFalse    = 'False';
  stvvarAll      = 'All';

  stverrNoInterface = 'GetVarOLEInfo - No interface supported';
  stverrCallback = '(INTERNAL) Callback aborted!';
  stverrAddedDefault = 'Generated automatically';
  stvvarhandler = 'Variable manager';
  stvDefaultCategory = 'Default';

type
  TxVarHandler = class;

  IVarCallback = interface(IDispatch)
    ['{268EBB70-5D64-47F1-B4A1-16643E71A77D}']
    function OnVarChanged(const Varname: WideString; const Value: OleVariant): HResult; stdcall;
    function OnVarCreated(const Varname: WideString): HResult; stdcall;
    function OnVarDeleted(const Varname: WideString): HResult; stdcall;
    function OnVarnameChanged(const OldVarname: WideString; const NewVarname: WideString): HResult; stdcall;
  end;

  TxVarCallbackItem = class(TCollectionItem)
  private
    FCallback: IVarCallback;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Callback: IVarCallback read FCallback write FCallback;
  end;

  TxVar = class(TCollectionItem)
  private
    FHint: string;
    FVarname: string;
    FFullFormatText: string;
    FContent: OleVariant;
    FCategory: string;
    FVarhandler: TxVarHandler;
    procedure SetContent(Value: OleVariant);
    procedure SetVarname(Value: string);
    procedure CheckVarname;
    function GetAsString: string;
    procedure SetAsString(AVal: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function GetFormatText: string;
    procedure Show;
    function DataTypeStr: string;

    {:Returns the Base Type of the Variable
      0=String;1=Bool;2=Integer;3=Float;4=IDispatch
      varBaseString = 0;
      varBaseBool = 1;
      varBaseInteger = 2;
      varBaseFloat = 3;
      varBaseIDispatch = 4;
    }
    function BaseDataType: Integer;


  published
    property Varname: string read FVarname write SetVarname;
    property Category: string read FCategory write FCategory;
    //:Any comment to the variable
    property Hint: string read FHint write FHint;
    //:Contains a Format String with at least one %s - formatted text can be retrieved with GetFormatText
    property FullFormatText: string read FFullFormatText write FFullFormatText;
    //:The content (Variant) of the variable
    property Content: OleVariant read FContent write SetContent;

    property AsString: string read GetAsString write SetAsString;
  end;

  TxVarHandler = class(TComponent, IVars)
  private
    FVars: TCollection;
    FCallbacks: TCollection;
  protected
    {IVars}
    function IVars.Get_Count = iiGet_Count;
    function IVars.Get_Values = iiGet_Values;
    procedure IVars.Set_Values = iiSet_Values;
    function IVars.Get_ValuesByIdx = iiGet_ValuesByIdx;
    procedure IVars.Set_ValuesByIdx = iiSet_ValuesByIdx;
    function IVars.Get_Names = iiGet_Names;
    procedure IVars.AddVar = iiAddVar;
    procedure IVars.DeleteVar = iiDeleteVar;
    function IVars.IdxOfVar = iiIdxOfVar;
    function IVars.VarExists = iiVarExists;

    function iiGet_Count: Integer; safecall;
    function iiGet_Values(const Varname: WideString): OleVariant; safecall;
    procedure iiSet_Values(const Varname: WideString; Value: OleVariant); safecall;
    function iiGet_ValuesByIdx(Index: Integer): OleVariant; safecall;
    procedure iiSet_ValuesByIdx(Index: Integer; Value: OleVariant); safecall;
    function iiGet_Names(Index: Integer): WideString; safecall;
    procedure iiAddVar(const Varname: WideString); safecall;
    procedure iiDeleteVar(const Varname: WideString); safecall;
    function iiIdxOfVar(const Varname: WideString): Integer; safecall;
    function iiVarExists(const Varname: WideString): WordBool; safecall;

    property iiCount: Integer read iiGet_Count;
    property iiValues[const Varname: WideString]: OleVariant read iiGet_Values write iiSet_Values;
    property iiValuesByIdx[Index: Integer]: OleVariant read iiGet_ValuesByIdx write iiSet_ValuesByIdx;
    property iiNames[Index: Integer]: WideString read iiGet_Names;

    procedure DoOnVarChanged(AVarname: string; Value: OleVariant);
    procedure DoOnVarnameChanged(OldVarname, NewVarname: string);
    procedure DoOnVarDeleted(AVarname: string);
    procedure DoOnVarCreated(AVarname: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //:IVars.BaseDataType
    function BaseDataType(const Varname: WideString): varBaseType; safecall;

    function FindXVar(AVarname: string): TxVar;
    //:set the value of a variable and creates the variable if it does not exist
    procedure SetVar(AVarname: string; Value: Variant);
    //:set the value of a variable - if then variable does not exist, it is created in the default category <stvDefault>
    procedure SetVarEx(AVarname: string; Value: Variant);
    //:Returns the value of a variable
    function GetVar(AVarname: string): OleVariant;
    //:Create a new variable
    function AddVar(AVarname, ACategory, AHint, AFullFormatText: string): TxVar; overload;
    //:Create a new variable
    function AddVar(AVarname, ACategory:String; Value:Variant): TxVar; overload;
    //:Create a new variable
    function AddVar(AVarname, ACategory:String): TxVar; overload;
    //:Create a new variable
    function AddVar(AVarname:String): TxVar; overload;
    //:Create a new variable
    function AddVar(AVarname:String; Value:Variant): TxVar; overload;
    //:Add a notifier to the notifier list
    procedure AddNotifier(ANotifier: IVarCallback);
    //:Delete a notifier from the notifier list
    procedure DeleteNotifier(ANotifier: IVarCallback);
    //:Check if a variable already exists
    function VarExists(AVarname: string): Boolean;
    //:Saves a whole category as a DGroup object - type cast only if it fits into a DText or DChannel
    procedure SaveVarCat(ACategory, Filename: string);
    //:Saves all Variables grouped by categories into a DFile
    procedure Save(Filename: string);
    //:Loads all Variables from a DFile
    procedure Load(Filename: string);
    //:Loads all Variables of a Category from a DFile
    procedure LoadVarCat(ACategory, Filename: string);
    //:Deletes a Variable - but not a protected one
    procedure DeleteVar(AVarname: string);
    //:Deletes a Variable - if DeleteProtected is false, protected variables are not deleted
    procedure DeleteVarEx(AVarname: string; DeleteProtected: Boolean);
    //:Deletes all variables - except the protected variables
    procedure ClearAllVars;
    //:Deletes all variables of a category - except the protected variables
    procedure ClearVarCat(ACategory: string);
    //:Returns the variables-inspector as a WinControl
    function GetDlgInspect: TForm;
    //:Shows the variables-inspector
    procedure ShowDlgInspect;
    //:Hides the variables-inspector
    procedure HideDlgInspect;
    {:Retrieves the Category-Varname structure from the varlist
    Returns a stringlist with categories - objects of stringlist.item are
    stringlists with the varname}
    procedure GetStructureList(AList: TStringList);
    procedure GetVarList(AList: TStrings);
    {:Replaces the Variable %%<Varname>%% within the given string ; IVars.ReplaceVarsInString}
    function ReplaceVarsInString(const Value: WideString): WideString; safecall;
  published
    //:The variables collection of TxVar
    property Vars: TCollection read FVars write FVars;
  end;

//:Returns the IID and the interface name of a variant if Type is IDispatch
function GetVarOLEInfo(V: Variant; var IID: TGUID; var IName: string): Boolean;
//:Returns the IID of a variant if Type is IDispatch
function GetVarOLEIID(V: Variant): TGUID;
//:Converts a variant into a valid Float
function xVarToFloat(V: Variant): Double;
//:Converts a variant into a valid Integer
function xVarToInt(V: Variant): Integer;
//:Converts a variant into a valid String
function xVarToString(V: Variant): string;
//:Converts a variant into a valid Bool
function xVarToBool(V: Variant): Boolean;
{:Returns the Base Type of the Variant
  0=String;1=Bool;2=Integer;3=Float;4=IDispatch
  varBaseString = 0;
  varBaseBool = 1;
  varBaseInteger = 2;
  varBaseFloat = 3;
  varBaseIDispatch = 4;
}
function xBaseDataType(V: Variant): Integer;


implementation

uses
  msVarListInspect;

function GetVarOLEIID(V: Variant): TGUID;
var
  iti: ITypeInfo;
  ita: PTypeAttr;
begin
  try
    IDispatch(V).GetTypeInfo(0, 0, iti);

    iti.GetTypeAttr(ita);
    Result := ita.guid;

    iti.ReleaseTypeAttr(ita);
  except
    Result := GUID_NULL;
    Exception.Create(stverrNoInterface);
  end;
end;

function GetVarOLEInfo(V: Variant; var IID: TGUID; var IName: string): Boolean;
var
  iti: ITypeInfo;
  ita: PTypeAttr;
begin
  Result := False;
  if IDispatch(V) <> nil then
  try
    IDispatch(V).GetTypeInfo(0, 0, iti);

    iti.GetTypeAttr(ita);
    IID := ita.guid;
    IName := GetRegStringValue('Interface\' + GUIDToString(ita.guid), '');

    iti.ReleaseTypeAttr(ita);
    Result := True;
  except
    Result := False;
    Exception.Create(stverrNoInterface);
  end;
end;

function uFloatToStr(Value: Double): string;
var
  dig: Integer;
begin
  //String formatieren
  if Abs(Value) < 0.01 then
    dig := 5
  else
  if Abs(Value) < 1.0 then
    dig := 3
  else
  if Abs(Value) < 10 then
    dig := 2
  else
  if Abs(Value) < 100 then
    dig := 1
  else
  if Abs(Value) < 1000 then
    dig := 1
  else
    dig := 0;

  if Abs(Value) > 1E9 then
    Result := FloatToStrF(Value, ffExponent, 6, 2)
  else
    Result := FloatToStrF(Value, ffFixed, 10, dig);
end;

function xVarToString(V: Variant): string;
var
  G: TGUID;
begin
  case VarType(V) of
    varEmpty    : Result := stvvarEmpty;
    varNull     : Result := stvvarNull;

    varSmallint,
    varInteger
    {$IFDEF DELPHI6_UP}
    , varShortInt
    {$ENDIF DELPHI6_UP}
    , varByte
    {$IFDEF DELPHI6_UP}
    , varWord
    , varLongWord
    , varInt64
    {$ENDIF DELPHI6_UP}
                : Result := IntToStr(V);

    varSingle   : Result := uFloatToStr(TVarData(V).VSingle);
    varDouble   : Result := uFloatToStr(TVarData(V).VDouble);
    varCurrency : Result := uFloatToStr(TVarData(V).VCurrency);
    varDate     : Result := DateTimeToStr(VarToDateTime(V));
    varOleStr   : Result := V;


    varDispatch :
      begin
        GetVarOLEInfo(V, G, Result);
      end;
    varError    : Result := stvvarError;
    varBoolean  :
      if TVarData(V).VBoolean then
        Result := stvvarTrue
      else
        Result := stvvarFalse;
    varVariant  : Result := stvvarVariant;
    varUnknown  : Result := stvvarUnknown;
    varStrArg   : Result := V;
    varString   : Result := V;
    varAny      : Result := stvvarAny;
    varTypeMask : Result := stvvarTypeMask;
    varArray    : Result := stvvarArray;
    varByRef    : Result := stvvarByRef;
  end;
end;

function xVarToFloat(V: Variant): Double;
var
  G: TGUID;
  S: string;
begin
  Result := 0.0;
  case VarType(V) of
    varSmallint,
    varInteger
    {$IFDEF DELPHI6_UP}
    , varShortInt
    {$ENDIF DELPHI6_UP}
    , varByte
    {$IFDEF DELPHI6_UP}
    , varWord
    , varLongWord
    , varInt64
    {$ENDIF DELPHI6_UP}
                : Result := V;

    varSingle   : Result := V;
    varDouble   : Result := V;
    varCurrency : Result := V;
    varDate     : Result := V;
    varString, varOleStr   : try Result := StrToFloat(V); except Result := 0.0 end;


    varDispatch :
      begin
        GetVarOLEInfo(V, G, S);
      end;
    varBoolean  :
      Result := Ord(TVarData(V).VBoolean);
  end;
end;

function xVarToInt(V: Variant): Integer;
begin
  Result := 0;
  case VarType(V) of
    varSmallint,
    varInteger
    {$IFDEF DELPHI6_UP}
    , varShortInt
    {$ENDIF DELPHI6_UP}
    , varByte
    {$IFDEF DELPHI6_UP}
    , varWord
    , varLongWord
    , varInt64
    {$ENDIF DELPHI6_UP}
                : Result := V;

    varSingle   : Result := Round(V);
    varDouble   : Result := Round(V);
    varCurrency : Result := Round(V);
    varDate     : Result := Round(V);
    varString, varOleStr   : try Result := StrToInt(V); except Result := 0 end;

    varBoolean  :
      Result := Ord(TVarData(V).VBoolean);
  end;
end;

function xVarToBool(V: Variant): Boolean;
var
  G: TGUID;
  S: string;
begin
  Result := False;
  case VarType(V) of
    varSmallint,
    varInteger
    {$IFDEF DELPHI6_UP}
    , varShortInt
    {$ENDIF DELPHI6_UP}
    , varByte
    {$IFDEF DELPHI6_UP}
    , varWord
    , varLongWord
    , varInt64
    {$ENDIF DELPHI6_UP}
                : Result := V <> 0;

    varSingle   : Result := (V > 0.0001) or (V < -0.001);
    varDouble   : Result := (V > 0.0001) or (V < -0.001);
    varCurrency : Result := (V > 0.0001) or (V < -0.001);
    varDate     : Result := (V > 0.0001) or (V < -0.001);


    varDispatch :
      begin
        GetVarOLEInfo(V, G, S);
      end;
    varString, varOleStr   : try Result := StrToFloat(V) <> 0; except Result := False end;
    varBoolean  :
      Result := V;
  end;
end;

function xBaseDataType(V: Variant): Integer;
begin
  Result := varBaseString;
  case VarType(V) of
    varEmpty    : Result := varBaseString;
    varNull     : Result := varBaseString;
    varSmallint : Result := varBaseInteger;
    varInteger  : Result := varBaseInteger;
    varSingle   : Result := varBaseFloat;
    varDouble   : Result := varBaseFloat;
    varCurrency : Result := varBaseFloat;
    varDate     : Result := varBaseFloat;
    varOleStr   : Result := varBaseString;
    varDispatch : Result := varBaseIDispatch;
    varError    : Result := varBaseString;
    varBoolean  : Result := varBaseBool;
    varVariant  : Result := varBaseString;
    varUnknown  : Result := varBaseString;
    {$IFDEF DELPHI6_UP}
    varShortInt : Result := varBaseInteger;
    {$ENDIF DELPHI6_UP}
    varByte     : Result := varBaseInteger;
    {$IFDEF DELPHI6_UP}
    varWord     : Result := varBaseInteger;
    varLongWord : Result := varBaseInteger;
    varInt64    : Result := varBaseInteger;
    {$ENDIF DELPHI6_UP}
    varStrArg   : Result := varBaseString;
    varString   : Result := varBaseString;
    varAny      : Result := varBaseString;
    varTypeMask : Result := varBaseString;
    varArray    : Result := varBaseString;
    varByRef    : Result := varBaseString;
  end;
end;

{TxVarCallbackItem}
constructor TxVarCallbackItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCallback := nil;
end;

destructor TxVarCallbackItem.Destroy;
begin
  FCallback := nil;
  inherited Destroy;
end;

{TxVar}
function TxVar.GetFormatText: string;
begin
  if Pos('%s', FullFormatText) <> 0 then
  begin
    try
      Result := Format(FullFormatText, [AsString]);
    except
      Result := Varname + ' = ' + AsString;
    end;
  end
  else
    Result := Varname + ' = ' + AsString;
end;

function TxVar.GetAsString: string;
var
  G: TGUID;
begin
  case VarType(Content) of
    varEmpty    : Result := stvvarEmpty;
    varNull     : Result := stvvarNull;

    varSmallint,
    varInteger
    {$IFDEF DELPHI6_UP}
    , varShortInt
    {$ENDIF DELPHI6_UP}
    , varByte
    {$IFDEF DELPHI6_UP}
    , varWord
    , varLongWord
    , varInt64
    {$ENDIF DELPHI6_UP}
                : Result := IntToStr(Content);

    varSingle   : Result := uFloatToStr(TVarData(Content).VSingle);
    varDouble   : Result := uFloatToStr(TVarData(Content).VDouble);
    varCurrency : Result := uFloatToStr(TVarData(Content).VCurrency);
    varDate     : Result := DateTimeToStr(VarToDateTime(Content));
    varOleStr   : Result := Content;


    varDispatch :
      begin
        GetVarOLEInfo(Content, G, Result);
      end;
    varError    : Result := stvvarError;
    varBoolean  :
      if TVarData(Content).VBoolean then
        Result := stvvarTrue
      else
        Result := stvvarFalse;
    varVariant  : Result := stvvarVariant;
    varUnknown  : Result := stvvarUnknown;
    varStrArg   : Result := Content;
    varString   : Result := Content;
    varAny      : Result := stvvarAny;
    varTypeMask : Result := stvvarTypeMask;
    varArray    : Result := stvvarArray;
    varByRef    : Result := stvvarByRef;
  end;
end;

procedure TxVar.SetAsString(AVal: string);
begin
  SetContent(AVal);
end;

procedure TxVar.Show;
begin
  MessageDlg(GetFormatText, mtInformation, [mbOK], 0);
end;

constructor TxVar.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FHint           := '';
  FVarname        := '';
  FFullFormatText := '';
  VarClear(FContent);
  FCategory       := '';
  FVarhandler := nil;
  CheckVarname;
end;

destructor TxVar.Destroy;
begin
  FVarhandler.DoOnVarDeleted(Varname);
  VarClear(FContent);
  inherited Destroy;
end;

function TxVar.DataTypeStr: string;
var
  G: TGUID;
begin
  case VarType(Content) of
    varEmpty    : Result := stvvarEmpty;
    varNull     : Result := stvvarNull;
    varSmallint : Result := stvvarSmallint;
    varInteger  : Result := stvvarInteger;
    varSingle   : Result := stvvarSingle;
    varDouble   : Result := stvvarDouble;
    varCurrency : Result := stvvarCurrency;
    varDate     : Result := stvvarDate;
    varOleStr   : Result := stvvarOleStr;
    varDispatch : GetVarOLEInfo(Content, G, Result);
    varError    : Result := stvvarError;
    varBoolean  : Result := stvvarBoolean;
    varVariant  : Result := stvvarVariant;
    varUnknown  : Result := stvvarUnknown;
    {$IFDEF DELPHI6_UP}
    varShortInt : Result := stvvarShortInt;
    {$ENDIF DELPHI6_UP}
    varByte     : Result := stvvarByte;
    {$IFDEF DELPHI6_UP}
    varWord     : Result := stvvarWord;
    varLongWord : Result := stvvarLongWord;
    varInt64    : Result := stvvarInt64;
    {$ENDIF DELPHI6_UP}
    varStrArg   : Result := stvvarStrArg;
    varString   : Result := stvvarString;
    varAny      : Result := stvvarAny;
    varTypeMask : Result := stvvarTypeMask;
    varArray    : Result := stvvarArray;
    varByRef    : Result := stvvarByRef;
  end;
end;

function TxVar.BaseDataType: Integer;
begin
  Result := xBaseDataType(Content)
end;

function TxVar.GetDisplayName: string;
begin
  if Varname = '' then
    Result := inherited GetDisplayName
  else
    Result := Varname;
end;

procedure TxVar.CheckVarname;
var
  C, I: Integer;
  S: string;
begin
  if FVarname = '' then
    FVarname := stvDefaultVarname + IntToStr(ID + 1);

  I := 0;
  C := ID + 1;
  while I < Collection.Count do
  begin
    if Collection.Items[I] <> Self then
      if CompareText(TxVar(Collection.Items[I]).FVarname, S) = 0 then
      begin
        S := FVarname + IntToStr(C);
        Inc(C, Collection.Count);
        I := -1;
      end;
    Inc(I);
  end;
  FVarname := S;
end;

procedure TxVar.SetVarname(Value: string);
begin
  if CompareText(Value, Varname) <> 0 then
  begin
    FVarhandler.DoOnVarnameChanged(FVarname, Value);
    FVarname := Value;
  end;
//    CheckVarname;
end;

procedure TxVar.SetContent(Value: OleVariant);
begin
  FContent := Value;
  FVarhandler.DoOnVarChanged(Varname, Value);
end;

{TxVarHandler}
function TxVarHandler.AddVar(AVarname, ACategory, AHint,
  AFullFormatText: string): TxVar;
begin
  Result := FindXVar(AVarname);
  if Result = nil then
  begin
    Result := FVars.Add as TxVar;
    with Result do
    begin
      FVarhandler := Self;
      Category := ACategory;
      Hint := AHint;
      FullFormatText := AFullFormatText;
      Varname := AVarname;
      DoOnVarCreated(Varname);
    end;
  end;
end;

procedure TxVarHandler.ClearAllVars;
var
  I: Integer;
begin
  I := 0;
  while I < FVars.Count do
  begin
    FVars.Delete(I);
  end;
end;

procedure TxVarHandler.ClearVarCat(ACategory: string);
var
  I: Integer;
begin
  I := 0;
  while I < FVars.Count do
  begin
    if CompareText(ACategory, TxVar(FVars.Items[I]).Category) = 0 then
    begin
      FVars.Delete(I);
    end
    else
      Inc(I);
  end;
end;

constructor TxVarHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVars := TCollection.Create(TxVar);
  FCallbacks := TCollection.Create(TxVarCallbackItem);
end;

procedure TxVarHandler.DeleteVar(AVarname: string);
begin
  DeleteVarEx(AVarname, False);
end;

procedure TxVarHandler.DeleteVarEx(AVarname: string; DeleteProtected: Boolean);
var
  I: Integer;
begin
  for I := 0 to FVars.Count - 1 do
    if CompareText(AVarname, TxVar(FVars.Items[I]).Varname) = 0 then
    begin
      FVars.Delete(I);
      Break;
    end;
end;

destructor TxVarHandler.Destroy;
begin
  //do not notify on destroy!
  FCallbacks.Clear;
  FCallbacks.Free;
  FCallbacks := nil;

  FVars.Clear;
  FVars.Free;

  inherited Destroy;
end;

function TxVarHandler.GetDlgInspect: TForm;
begin
  Result := nil;
end;

function TxVarHandler.FindXVar(AVarname: string): TxVar;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FVars.Count - 1 do
    if CompareText(AVarname, TxVar(FVars.Items[I]).Varname) = 0 then
    begin
      Result := TxVar(FVars.Items[I]);
      Break;
    end;
end;

function TxVarHandler.GetVar(AVarname: string): OleVariant;
var
  V: TxVar;
begin
  VarClear(Result);

  V := FindXVar(AVarname);
  if V <> nil then
    Result := V.Content;
end;

procedure TxVarHandler.HideDlgInspect;
begin
  //not implemented
end;

procedure TxVarHandler.Load(Filename: string);
begin
end;

procedure TxVarHandler.LoadVarCat(ACategory, Filename: string);
begin
end;

procedure TxVarHandler.Save(Filename: string);
begin
end;

procedure TxVarHandler.SaveVarCat(ACategory, Filename: string);
begin
end;

procedure TxVarHandler.SetVar(AVarname: string; Value: Variant);
var
  V: TxVar;
begin
  V := FindXVar(AVarname);
  if V <> nil then
    V.Content := Value
  else
  begin//Variable erzeugen
    AddVar(AVarname, stvDefaultCategory, '', '').Content := Value;
  end;
end;

procedure TxVarHandler.SetVarEx(AVarname: string; Value: Variant);
var
  V: TxVar;
begin
  V := AddVar(AVarname, stvDefaultCategory, stverrAddedDefault, '');
  if V <> nil then
    V.Content := Value
  else
  begin
    //Warnung ausgeben
  end;
end;

procedure TxVarHandler.ShowDlgInspect;
begin
  //not implemented
end;

function TxVarHandler.VarExists(AVarname: string): Boolean;
begin
  Result := FindXVar(AVarname) <> nil;
end;

procedure TxVarHandler.GetStructureList(AList: TStringList);
var
  I, idx: Integer;
begin
  if AList = nil then Exit;

  for I := 0 to FVars.Count - 1 do
  begin
    idx := AList.IndexOf(TxVar(FVars.Items[I]).Category);

    if idx = -1 then
      idx := AList.AddObject(TxVar(FVars.Items[I]).Category, TStringList.Create);

    TStringList(AList.Objects[idx]).Add(TxVar(FVars.Items[I]).Varname);
  end;
end;

procedure TxVarHandler.GetVarList(AList: TStrings);
var
  I: Integer;
begin
  if AList = nil then Exit;

  for I := 0 to FVars.Count - 1 do
    AList.Add(TxVar(FVars.Items[I]).Varname);
end;

function TxVarHandler.ReplaceVarsInString(const Value: WideString): WideString;
var s, _var:String;
    Positions : TList;
    i:Integer;
begin
  //lookup for all strings coverd by stcVarTextDelimiter
  s := StringReplace( Value, stcVarTextDelimiter, #3, [rfReplaceAll]);
  Result := Value;

  Positions := TList.Create;
  try
    for i:=1 to Length( s) do
      if s[i] = #3 then
        Positions.Add( Pointer(i));

    if Positions.Count>1 then
      for i:=1 to (Positions.Count div 2) do begin
        _var := Copy( s, Integer( Positions[ i*2-2])+1,
                      Integer( Positions[ i*2-1])-Integer( Positions[ i*2-2])-1);
        if VarExists( _var) then
          Result := StringReplace( Result, stcVarTextDelimiter+_var+stcVarTextDelimiter,
                    FindXVar( _var).AsString, [rfReplaceAll]);
      end;

  finally
    Positions.Free;
  end;
end;

function TxVarHandler.iiGet_Count: Integer;
begin
  Result := Vars.Count;
end;

function TxVarHandler.iiGet_Values(const Varname: WideString): OleVariant;
begin
  Result := '';
  if VarExists(Varname) then
    Result := GetVar(Varname);
end;

procedure TxVarHandler.iiSet_Values(const Varname: WideString; Value: OleVariant);
begin
  if not VarExists(Varname) then
    AddVar(Varname, stvDefaultCategory, '', '');
  SetVar(Varname, Value);
end;

function TxVarHandler.iiGet_ValuesByIdx(Index: Integer): OleVariant;
begin
  Result := TxVar(Vars.Items[Index]).Content;
end;

procedure TxVarHandler.iiSet_ValuesByIdx(Index: Integer; Value: OleVariant);
begin
  TxVar(Vars.Items[Index]).Content := Value;
end;

function TxVarHandler.iiGet_Names(Index: Integer): WideString;
begin
  Result := TxVar(Vars.Items[Index]).Varname;
end;

procedure TxVarHandler.iiAddVar(const Varname: WideString);
begin
  AddVar(Varname, stvDefaultCategory, '', '');
end;

procedure TxVarHandler.iiDeleteVar(const Varname: WideString);
begin
  DeleteVar(Varname);
end;

function TxVarHandler.iiIdxOfVar(const Varname: WideString): Integer;
begin
  Result := -1;
  if VarExists(Varname) then
    Result := FindXVar(Varname).Index;
end;

function TxVarHandler.iiVarExists(const Varname: WideString): WordBool;
begin
  Result := VarExists(Varname);
end;

function TxVarHandler.BaseDataType(const Varname: WideString): varBaseType;
begin
  Result := varBaseString;
  if VarExists(Varname) then
    Result := FindXVar(Varname).BaseDataType;
end;

procedure TxVarHandler.DoOnVarChanged(AVarname: string; Value: OleVariant);
var
  I: Integer;
begin
  if FCallbacks = nil then Exit;
  try
    for I := 0 to FCallbacks.Count - 1 do
      TxVarCallbackItem(FCallbacks.Items[I]).Callback.OnVarChanged(AVarname, Value);
  except
    raise Exception.Create(stverrCallback);
  end;
end;

procedure TxVarHandler.DoOnVarnameChanged(OldVarname, NewVarname: string);
var
  I: Integer;
begin
  if FCallbacks = nil then Exit;
  try
    for I := 0 to FCallbacks.Count - 1 do
      TxVarCallbackItem(FCallbacks.Items[I]).Callback.OnVarnameChanged(OldVarname, NewVarname);
  except
    raise Exception.Create(stverrCallback);
  end;
end;

procedure TxVarHandler.DoOnVarDeleted(AVarname: string);
var
  I: Integer;
begin
  if FCallbacks = nil then Exit;
  try
    for I := 0 to FCallbacks.Count - 1 do
      TxVarCallbackItem(FCallbacks.Items[I]).Callback.OnVarDeleted(AVarname);
  except
    raise Exception.Create(stverrCallback);
  end;
end;

procedure TxVarHandler.DoOnVarCreated(AVarname: string);
var
  I: Integer;
begin
  if FCallbacks = nil then Exit;
  try
    for I := 0 to FCallbacks.Count - 1 do
      TxVarCallbackItem(FCallbacks.Items[I]).Callback.OnVarCreated(AVarname);
  except
    raise Exception.Create(stverrCallback);
  end;
end;

procedure TxVarHandler.AddNotifier(ANotifier: IVarCallback);
begin
  TxVarCallbackItem(FCallbacks.Add).FCallback := ANotifier;
end;

procedure TxVarHandler.DeleteNotifier(ANotifier: IVarCallback);
var
  I: Integer;
begin
  for I := 0 to FCallbacks.Count - 1 do
    if TxVarCallbackItem(FCallbacks.Items[I]).FCallback = ANotifier then
    begin
      FCallbacks.Delete(I);
      Break;
    end;
end;

function TxVarHandler.AddVar(AVarname: String; Value: Variant): TxVar;
begin
  Result := AddVar( AVarname, stvDefaultCategory, Value);
end;

function TxVarHandler.AddVar(AVarname: String): TxVar;
begin
  Result := AddVar( AVarname, stvDefaultCategory, '', '');
end;

function TxVarHandler.AddVar(AVarname, ACategory: String): TxVar;
begin
  Result := AddVar( AVarname, stvDefaultCategory, '', '');
end;

function TxVarHandler.AddVar(AVarname, ACategory: String;
  Value: Variant): TxVar;
begin
  Result := AddVar( AVarname, stvDefaultCategory, '', '');
  if Result <>nil then
    Result.Content := Value;
end;

end.
