(*
 ***************************************************************************
 optiMEAS GmbH
 written by Burkhard Schranz
 copyright © 2014 -
 Email : info@optimeas.de
 Web : http://www.optimeas.de
 http://www.makestudio.de
 http://www.mobiconn.de
 The source code is given as is. The author is not responsible
 for any possible damage done due to the use of this code.
 The component can be freely used in any application.
   source code remains property of the author and may not be distributed,
 published, given or sold in any form as such. No parts of the source
 code can be included in any other component or application without
 written authorization of optiMEAS GmbH


  Unit history:                                                                                    }

  2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
  2005/02/04  USchuster - preparations for check in
  2005/02/19  USchuster - changes for commandline version
  2005/03/05  USchuster - changed some interface typecasts from I..() to .. as I..
  because the hard I..() typecasts don't work for assemblies
  2005/08/12  BSchranz  - Command "SaveLog" added, command line version "jmak.exe" added
  2005/08/27  BSchranz  - Get rid of all cross references like Menus, Treeview, etc
  2005/08/28  BSchranz  - Callback Interface for Program events implemented
  2005/09/02  BSchranz  - Added ICommand2
  2005/09/02  BSchranz  - Fixed error in "FOR"
  2005/09/04  BSchranz  - Translated to englisch
  2005/09/12  USchuster - D5 fix and minor style cleaning
  2006/02/05  BSchranz  - Include Support Added
  2013/12/28  BSchranz  - Comment added
  2013/12/28  BSchranz  - Activate/Deactivate Command support

  ----------------------------------------------------------------------------- *)
// collects all external (compiler)Commands from plugins
unit msProgram;

{$I jedi.inc}

interface

uses
  ActiveX, Classes, ComObj, Contnrs, makestudio_TLB,
{$IFDEF DELPHI6_UP}
  Variants, Types,
{$ENDIF DELPHI6_UP}
  SysUtils, Windows, Graphics, msvarhandler, Clipbrd, Forms,
  Dialogs, Controls, msResources;

{
  ***************
  Used Interfaces

  ICommand = interface(IDispatch)
  function MeasureItem(Handle: Integer; BriefView: WordBool): Integer; safecall;
  function EditItem: WordBool; safecall;
  function ExecuteItem: WordBool; safecall;
  function DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
  Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool; safecall;
  procedure SetFilename(const Filename: WideString); safecall;
  function Get_Caption: WideString; safecall;
  procedure Set_Caption(const Value: WideString); safecall;
  function Get_ParamValues(const ParamName: WideString): WideString; safecall;
  procedure Set_ParamValues(const ParamName: WideString; const Value: WideString); safecall;
  function Get_ParamNames(Index: Integer): WideString; safecall;
  function Get_ParamCount: Integer; safecall;
  property Caption: WideString read Get_Caption write Set_Caption;
  property ParamValues[const ParamName: WideString]: WideString read Get_ParamValues write Set_ParamValues;
  property ParamNames[Index: Integer]: WideString read Get_ParamNames;
  property ParamCount: Integer read Get_ParamCount;
  end;

  ICommand2 = interface(ICommand)
  function Get_OwnerDraw: WordBool; safecall;
  function Get_PreviewText: WideString; safecall;
  function Notify(const Notification: WideString; Parameter: OleVariant): OleVariant; safecall;
  property OwnerDraw: WordBool read Get_OwnerDraw;
  property PreviewText: WideString read Get_PreviewText;
  end;

  ICommandCallback = interface(IDispatch)
  function CreateCommand: IDispatch; safecall;
  procedure SetCanceled(aCanceled: WordBool); safecall;
  function GetIdentifier: WideString; safecall;
  end;
}

{ ***************System Commands*********************************************** }
type
  // Block Condition Types
  TBlockCondition1 = (bcEqual, bcGreater, bcLess, bcGreaterEqual, bcLessEqual,
    bcNotEqual, bcContains, bcNotContains, bcFileExists, bcPathExists);

  TIFBlock = class(TComponent, ICommand2)
  private
    FCaption: string;
    FCondition: TBlockCondition1;
    FVarname: string;
    FValue: OleVariant;
  protected
    function MeasureItem(Handle: Integer; BriefView: WordBool)
      : Integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer;
      Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
      BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString;
      const Value: WideString); safecall;
    function Get_ParamNames(Index: Integer): WideString; safecall;
    function Get_ParamCount: Integer; safecall;
    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant)
      : OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;

    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString
      read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Condition: TBlockCondition1 read FCondition write FCondition;
    property Varname: string read FVarname write FVarname;
    property VarValue: OleVariant read FValue write FValue;
  end;

  TELSEBlock = class(TComponent, ICommand2)
  private
    FCaption: string;
    FCondition: string;
  protected
    function MeasureItem(Handle: Integer; BriefView: WordBool)
      : Integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer;
      Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
      BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString;
      const Value: WideString); safecall;
    function Get_ParamNames(Index: Integer): WideString; safecall;
    function Get_ParamCount: Integer; safecall;
    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant)
      : OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;

    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString
      read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TENDBlock = class(TComponent, ICommand2)
  private
    FCaption: string;
    FCondition: string;
  protected
    function MeasureItem(Handle: Integer; BriefView: WordBool)
      : Integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer;
      Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
      BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString;
      const Value: WideString); safecall;
    function Get_ParamNames(Index: Integer): WideString; safecall;
    function Get_ParamCount: Integer; safecall;
    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant)
      : OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;

    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString
      read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TFORBlock = class(TComponent, ICommand2)
  private
    FCaption: string;
    FStartValue: Integer;
    FEndValue: Integer;
    FVarname: string;
    // FCounter: Integer;
  protected
    function MeasureItem(Handle: Integer; BriefView: WordBool)
      : Integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer;
      Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
      BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString;
      const Value: WideString); safecall;
    function Get_ParamNames(Index: Integer): WideString; safecall;
    function Get_ParamCount: Integer; safecall;
    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant)
      : OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;

    property Properties: IDispatch read Get_Properties;
    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString
      read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    ///	<summary>
    ///	  Zähler der FOR Schleife zurücksetzen
    ///	</summary>
    procedure ResetCounter;

    property StartValue: Integer read FStartValue write FStartValue;
    property EndValue: Integer read FEndValue write FEndValue;
    property Varname: string read FVarname write FVarname;
  end;

  TWHILEBlock = class(TComponent, ICommand)
  private
    FCaption: string;
    FCondition: TBlockCondition1;
    FVarname: string;
    FValue: OleVariant;
  protected
    function MeasureItem(Handle: Integer; BriefView: WordBool)
      : Integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer;
      Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
      BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString;
      const Value: WideString); safecall;
    function Get_ParamNames(Index: Integer): WideString; safecall;
    function Get_ParamCount: Integer; safecall;
    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString
      read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Condition: TBlockCondition1 read FCondition write FCondition;
    property Varname: string read FVarname write FVarname;
    property VarValue: OleVariant read FValue write FValue;
  end;

  TINCLUDEBlock = class(TComponent, ICommand2)
  private
    // FCaption: string;
    FFilename: string;
  protected
    function MeasureItem(Handle: Integer; BriefView: WordBool)
      : Integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer;
      Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
      BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString;
      const Value: WideString); safecall;
    function Get_ParamNames(Index: Integer): WideString; safecall;
    function Get_ParamCount: Integer; safecall;

    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant)
      : OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;

    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString
      read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Filename: string read FFilename write FFilename;
  end;

  TCOMMENTBlock = class(TComponent, ICommand2)
  private
    FComment: string;
    procedure SetComment(const Value: String);
  protected
    function MeasureItem(Handle: Integer; BriefView: WordBool)
      : Integer; safecall;
    function EditItem: WordBool; safecall;
    function ExecuteItem: WordBool; safecall;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer;
      Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
      BkColor: OLE_COLOR): WordBool; safecall;
    procedure SetFilename(const Filename: WideString); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_ParamValues(const ParamName: WideString): WideString; safecall;
    procedure Set_ParamValues(const ParamName: WideString;
      const Value: WideString); safecall;
    function Get_ParamNames(Index: Integer): WideString; safecall;
    function Get_ParamCount: Integer; safecall;

    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant)
      : OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;

    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
    property Caption: WideString read Get_Caption write Set_Caption;
    property ParamValues[const ParamName: WideString]: WideString
      read Get_ParamValues write Set_ParamValues;
    property ParamNames[Index: Integer]: WideString read Get_ParamNames;
    property ParamCount: Integer read Get_ParamCount;
  public
    constructor Create(AOwner: TComponent); override;
    property Comment: String read FComment write SetComment;
  end;

  // Callback to create an instance of the ICommand
  TIFBlockCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

  TELSEBlockCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

  TENDBlockCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

  TFORBlockCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

  TWHILEBlockCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

  TINCLUDEBlockCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

  TCOMMENTBlockCallback = class(TComponent, ICommandCallback)
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(ACanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

  { ***********End System Commands*********************************************** }

  { ***************Program Class Structure*************************************** }

type

  TProgram = class;

  // stores the information given by "registerCommand"
  TCommandTypeItem = class(TCollectionItem)
  private
    FName: string;
    FHint: string;
    FCategory: string;
    FBitmap: TBitmap;
    FCompatibilityIndex: Integer;
    // for compatibility with older file format versions
    FFileExtensions: string;
    FCallback: ICommandCallback;

    function GetIdentifier: string;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Bitmap: TBitmap read FBitmap write FBitmap;
    property Name: string read FName write FName;
    property Identifier: string read GetIdentifier;
    property Hint: string read FHint write FHint;
    property Category: string read FCategory write FCategory;
    property CompatibilityIndex: Integer read FCompatibilityIndex
      write FCompatibilityIndex;
    property FileExtensions: string read FFileExtensions write FFileExtensions;
    property Callback: ICommandCallback read FCallback write FCallback;
  end;

  TCommandTypeItemList = class(TList)
  private
    function GetItem(Index: Integer): TCommandTypeItem;
    procedure SetItem(Index: Integer; const Value: TCommandTypeItem);
  public
    property Items[Index: Integer]: TCommandTypeItem read GetItem
      write SetItem; default;
  end;

  // collection of all registered plugin-Commands
  TCommandTypes = class(TCollection)
  private
    FIFBlockCallback: TIFBlockCallback;
    FELSEBlockCallback: TELSEBlockCallback;
    FENDBlockCallback: TENDBlockCallback;
    FFORBlockCallback: TFORBlockCallback;
    FWHILEBlockCallback: TWHILEBlockCallback;
    FINCLUDEBlockCallback: TINCLUDEBlockCallback;
    FCOMMENTBlockCallback: TCOMMENTBlockCallback;
    FImageHeight: Integer;
    FImageWidth: Integer;

    function GetCommandTypeItem(Value: Integer): TCommandTypeItem;
    procedure AddSystemCommands;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure Add(const CommandName: WideString; const CommandHint: WideString;
      const CommandCategory: WideString; const ModuleBitmap: TBitmap;
      const DragDropFileExtensions: WideString; CompatibilityIndex: Integer;
      const Callback: IDispatch); reintroduce; overload;
    function Add: TCommandTypeItem; reintroduce; overload;
    function GetItemByID(ID: string): TCommandTypeItem;
    function GetItemByExtension(AFileExtension: string): TCommandTypeItem;
    function GetItemCountByExtension(AFileExtension: string): Integer;
    function GetItemsByExtension(AFileExtension: string): TCommandTypeItemList;
    function GetItemByCompatibilityIndex(AIndex: Integer): TCommandTypeItem;
    property Items[Index: Integer]: TCommandTypeItem
      read GetCommandTypeItem; default;
    property ImageWidth: Integer read FImageWidth write FImageWidth;
    property ImageHeight: Integer read FImageHeight write FImageHeight;
  end;

  // generic Command
  TCustomCommand = class(TPersistent)
  private
    FIsIgnored: Boolean;
    FIgnoreExecutionError: Boolean;
    procedure SetBreakpoint(const Value: Boolean);
    procedure SetIgnoreExecutionError(const Value: Boolean);
    procedure SetIsIgnored(const Value: Boolean);
  protected
    FIdentifier: string;
    FCaption: string;
    FProgram: TProgram;
    FProgramLevel: Integer; // Level in Loops or if etc
    FIndent: Integer;
    FBreakpoint: Boolean;
    FSelected: Boolean;

    procedure SetIdentifier(Value: string); virtual;
    function GetIdentifier: string; virtual;
    function GetCaption: string; virtual;
    procedure SetCaption(Value: string); virtual;
    function GetIndent: Integer;
    function GetID: Integer;
    procedure SetSelected(const Value: Boolean);
  public
    constructor Create(AProgram: TProgram); reintroduce; virtual;
    destructor Destroy; override;
    procedure GetFromStringList(sl: TStringList); virtual;
    procedure WriteToStringList(sl: TStringList); virtual;
    function Edit: Boolean; virtual;
    function Run: Boolean; virtual;
    function GetDrawHeight(Canvas: TCanvas): Integer; virtual;
    // Height for drawing the Bitmap
    procedure Draw(Canvas: TCanvas; Rect: TRect; State: TOwnerDrawState;
      ABreakPoint: Integer); virtual;

    property Identifier: string read GetIdentifier write SetIdentifier;
    property Caption: string read GetCaption write SetCaption;
    property OwnerProgram: TProgram read FProgram write FProgram;
    property Indent: Integer read GetIndent;
    property ProgramLevel: Integer read FProgramLevel write FProgramLevel;
    property ID: Integer read GetID;
    property Breakpoint: Boolean read FBreakpoint write SetBreakpoint;
    property Selected: Boolean read FSelected write SetSelected;
    property IsIgnored: Boolean read FIsIgnored write SetIsIgnored;
    property IgnoreExecutionError: Boolean read FIgnoreExecutionError
      write SetIgnoreExecutionError;
  end;

  // instance of a Command registered by a plugin
  TCommand = class(TCustomCommand)
  private
    function GetOwnerDraw: Boolean;
    function GetPreviewText: string;
    function GetSupportsCommand2: Boolean;
  protected
    FCommand: ICommand;
    FCommand2: ICommand2;
    FCommandItem: TCommandTypeItem;

    procedure SetIdentifier(Value: string); override;
    function GetIdentifier: string; override;
    function GetCaption: string; override;
    procedure SetCaption(Value: string); override;
    function MakeIndent: string;
  public
    constructor Create(ACommandType: TCommandTypeItem; AProgram: TProgram);
      reintroduce; virtual;
    destructor Destroy; override;
    procedure SetFilename(AFilename: string);
    procedure GetFromStringList(sl: TStringList); override;
    procedure WriteToStringList(sl: TStringList); override;
    function Edit: Boolean; override;
    function Run: Boolean; override;
    function RunInclude(aCommand: ICommand2): Boolean; virtual;
    function GetDrawHeight(Canvas: TCanvas): Integer; override;
    // Height for drawing the Bitmap
    procedure Draw(Canvas: TCanvas; Rect: TRect; State: TOwnerDrawState;
      ABreakPoint: Integer); override;
    function Notifiy(Notification: WideString; Parameter: OleVariant)
      : OleVariant;
    property OwnerDraw: Boolean read GetOwnerDraw;
    property SupportsCommand2: Boolean read GetSupportsCommand2;
    property PreviewText: string read GetPreviewText;

    // property Command: ICommand read FCommand;
    property CommandItem: TCommandTypeItem read FCommandItem;

    property Identifier;
  end;

  IProgramNotification = interface(IDispatch)
    ['{C8A857BE-4566-418C-B532-006333B0D5B4}']

    // Start/Stop/Debug
    procedure OnBeforeStartProgram; stdcall;
    procedure OnAfterStopProgram; stdcall;
    procedure OnProgramPaused; stdcall;
    procedure OnSetProgramPosition(const ItemIndex: Integer); stdcall;
    procedure OnSetProgress(const ACaption: string;
      const ProgressMin, ProgressMax, ProgressPosition: Integer); stdcall;

    // List Notifications
    procedure OnClearItems; stdcall;
    procedure OnRefresh; stdcall;
    procedure OnAddItem(const ItemIndex: Integer); stdcall;
    procedure OnInsertItem(const InsertAt: Integer); stdcall;
    procedure OnDeleteItem(const ItemIndex: Integer); stdcall;
    procedure OnItemChanged(const ItemIndex: Integer); stdcall;
    procedure OnExchangeItem(const Index1, Index2: Integer); stdcall;
    procedure OnSelectionChanged(const ItemIndex: Integer); stdcall;

    // for future purpose - not used yet
    procedure OnNotify(const Notification: Integer;
      const Param: OleVariant); stdcall;
  end;

  TProgramNotificationItem = class(TCollectionItem)
  private
    FNotification: IProgramNotification;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Notification: IProgramNotification read FNotification
      write FNotification;
  end;

  // collection of all used Commands (MakeStudio-program)
  TProgram = class(TObjectList)
  private
    FNotificationList: TCollection;
    FFilename: string;
    FModified: Boolean;
    FCanceled: Boolean; // Cancel of Run process
    FBriefView: Boolean;
    FShowLines: Boolean;
    // FShowBreakpoints: Boolean;
    FBreakpointBmp: TBitmap;
    FPositionBmp: TBitmap;
    FCommentBmp: TBitmap;
    FIsRun: Boolean;
    FIsPaused: Boolean;
    FInitialized: Boolean;
    FHaltOnNextCommand: Boolean;
    FSelected: TCommand;
    FIncludeLevel: Integer;
    FClearLogbookOnStart: Boolean;
    FActiveProgram: TProgram;
    FRootProgram: TProgram;
    FIsInclude: Boolean; // to get the stack of INCLUDE Calls

    procedure SetCanceled(Value: Boolean);

    function GetItem(Index: Integer): TCommand;
    procedure SetItem(Index: Integer; Value: TCommand);
    function GetSelectedCount: Integer;
    procedure SetBriefView(const Value: Boolean);
    procedure SetShowLines(const Value: Boolean);
    function GetSelected: TCommand;
    procedure SetSelected(const Value: TCommand);
  protected
    procedure DoBeforeStartProgram;
    procedure DoAfterStopProgram;
    procedure DoProgramPaused;
    procedure DoSetProgramPosition(const ItemIndex: Integer);
    procedure DoSetProgress(const ACaption: string;
      const ProgressMin, ProgressMax, ProgressPosition: Integer);
    procedure DoClearItems;
    procedure DoRefresh;
    procedure DoAddItem(const ItemIndex: Integer);
    procedure DoInsertItem(const InsertAt: Integer);
    procedure DoDeleteItem(const ItemIndex: Integer);
    procedure DoItemChanged(const ItemIndex: Integer);
    procedure DoExchangeItem(const Index1, Index2: Integer);
    procedure DoSelectionChanged(const ItemIndex: Integer);
    procedure DoFullRefresh;

    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

    property IncludeLevel: Integer read FIncludeLevel write FIncludeLevel;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Program
    function GetCommandTypeFromStringList(sl: TStrings): string;
    function CreateCommandFromStringList(sl: TStrings; CommandType: string)
      : TCustomCommand;
    function LoadFromFile(AFilename: string): Boolean;
    function SaveToFile(AFilename: string): Boolean;

    // if Insert=true - InsertAt means the position - if InsertAt=-1 then Append
    function LoadFromStrings(sl: TStrings; Insert: Boolean; InsertAt: Integer)
      : Boolean; overload;
    function LoadFromStrings(sl: TStrings): Boolean; overload;
    function SaveToStrings(sl: TStrings; OnlySelected: Boolean)
      : Boolean; overload;
    function SaveToStrings(sl: TStrings): Boolean; overload;
    procedure New;
    function Execute: Boolean; overload;
    function Execute(StartIndex: Integer): Boolean; overload;
    function CheckSyntax: Boolean;
    procedure Refresh;
    function IsSystemCommand(aCommand: TCommand): Boolean;

    // Clipboard
    procedure SelectAll;
    procedure ClearSelection;
    procedure DeleteSelectedItems;
    procedure CopySelectionToClipboard;
    procedure CutSelectionToClipboard;
    function CanPaste: Boolean;
    function CanCopy: Boolean;
    procedure PasteFromClipboard;

    // Notifications
    procedure AddNotification(Notification: IProgramNotification);
    procedure RemoveNotification(Notification: IProgramNotification);
    procedure ClearNotifications;

    // inherited from objectlist
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function Add(AObject: TObject): Integer;
    function Remove(AObject: TObject): Integer;
    procedure Insert(Index: Integer; AObject: TObject);
    procedure Exchange(Index1, Index2: Integer);

    property Filename: string read FFilename write FFilename;
    property Modified: Boolean read FModified write FModified;
    property Canceled: Boolean read FCanceled write SetCanceled;
    property BriefView: Boolean read FBriefView write SetBriefView;
    property ShowLines: Boolean read FShowLines write SetShowLines;
    property BreakpointBmp: TBitmap read FBreakpointBmp;
    property CommentBmp: TBitmap read FCommentBmp;
    property IsRun: Boolean read FIsRun write FIsRun;
    property Selected: TCommand read GetSelected write SetSelected;
    property SelectedCount: Integer read GetSelectedCount;
    property IsPaused: Boolean read FIsPaused write FIsPaused;
    property HaltOnNextCommand: Boolean read FHaltOnNextCommand
      write FHaltOnNextCommand;
    property ClearLogbookOnStart: Boolean read FClearLogbookOnStart
      write FClearLogbookOnStart;
    property RootProgram: TProgram read FRootProgram write FRootProgram;
    property ActiveProgram: TProgram read FActiveProgram write FActiveProgram;
    property IsInclude: Boolean read FIsInclude write FIsInclude;

    property Items[Index: Integer]: TCommand read GetItem
      write SetItem; default;
  end;

const
  stdcCondition = 'Condition';
  stdcVarname = 'Varname';
  stdcFilename = 'Filename';
  stdcValue = 'Value';
  stdcDataType = 'DataType';
  stdcStartValue = 'StartValue';
  stdcEndValue = 'EndValue';

  dmmtNone = 'abstract.command';
  stcBegin = 'begin';
  stcObject = 'object';
  stcEnd = 'end';
  stdcCaption = 'Caption';
  stdcIgnore = 'Ignore';
  stdcCommandType = 'ModuleType';
  stdcVersion = '{796D9287-1850-46A8-9C28-24E627FBC446} Version';
  iProgramVersion = 3;

  DEFAULTINDENT = 32;

  // all reserved commands which have a begin and end
  // must start with block. in the identifier
  IDBlock = 'block.';
  IDIFBlock = IDBlock + 'if';
  IDELSEBlock = IDBlock + 'else';
  IDENDBlock = IDBlock + 'end';
  IDFORBlock = IDBlock + 'for';
  IDWHILEBlock = IDBlock + 'while';
  IDINCLUDEBlock = IDBlock + 'include';
  IDCOMMENT = 'command.comment';

var
  ProgramAddLog: procedure(S: string) = nil;
  ProgramClearLog: procedure = nil;
  ProgramDefaultExt: String = '.jmk';

  ProgramGutterWidth: Integer = 50;

  // :Global - just fill once
function CommandTypes: TCommandTypes;
procedure SetVarhandlerReference(aVarhandler: TxVarhandler);

resourcestring
  stdIFCaption = 'if';
  stdELSECaption = 'else';
  stdENDCaption = 'end';
  stdFORCaption = 'for';
  stdWHILECaption = 'while';
  stdINCLUDECaption = 'Include';
  stdCOMMENTCaption = 'Comment';
  stbcEqual = '=';
  stbcGreater = '>';
  stbcLess = '<';
  stbcGreaterEqual = '>=';
  stbcLessEqual = '<=';
  obcNotEqual = '<>';
  stbcContains = 'contains text';
  stbcNotContains = 'does not contain text';
  stbcFileExists = 'as file exists';
  stbcPathExists = 'as directory exists';
  stProgramControlCategory = 'Program control';
  stDefaultCategory = 'Default';
  stAbstracTCustomCommand = 'Abstract command';
  stdErrNoEditBaseCommand = 'Cannot edit base object';
  stdReady = '<<<<<< Ready! >>>>>>';
  stdCanceled = '<<<<<< Execution canceled! >>>>>>';
  stdError = '<<<<<< ERROR - Execution canceled! >>>>>>';
  stdBreak =
    '********************************************************************';
  stdErrNoRunBaseCommand = 'Cannot execute base object';
  stdErrCommandNotFound = 'Command not registered: ';
  stdErrTooManyEndingBlocks =
    'Insufficient number of End-instructions in the sequence!';
  stdErrTooFewEndingBlocks = 'Too many End-Instructions in the sequence!';
  stdErrNoIFBlockToElse = 'No belonging IF-instruction for "else"!';
  stdErrTooManyElseBlocks = 'Too many ELSE-instructions!';
  stdSyntaxOK = 'Syntax check successful';
  stdErrIncludeStackOverflow =
    'Too many recourse INCLUDE calls - Program aborded!';
  stdErrIncludeFileNotFoud = 'Include file "%s" not found!';
  stdDoInclude = 'Include "%s"...';
  stdIncludeStarted = 'Include file "%s" started...';

implementation

{$R program.res}

uses
{$IFDEF DELPHI5}
  FileCtrl,
{$ENDIF DELPHI5}
  msEditIf, msEditFor, msEditWhile, msEditInclude,
  msEditComment;

resourcestring
  StrErrorSettingParam = 'Error in "%s":Property=%s - %s';

var
  _CommandTypes: TCommandTypes = nil;
  VarhandlerRef: TxVarhandler = nil;

procedure SetVarhandlerReference(aVarhandler: TxVarhandler);
begin
  VarhandlerRef := aVarhandler;
end;

function FillSpaces(aCount: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to aCount do
    Result := Result + ' ';
end;

procedure DoLogbookMessage(const Value: string);
begin
  if Assigned(ProgramAddLog) then
    ProgramAddLog(Value);
end;

procedure DoClearLogbook;
begin
  if Assigned(ProgramClearLog) then
    ProgramClearLog;
end;

function CommandTypes: TCommandTypes;
begin
  if _CommandTypes = nil then
    _CommandTypes := TCommandTypes.Create;
  Result := _CommandTypes;
end;

// Helper
procedure TrimStringListLeft(sl: TStrings);
var
  I: Integer;
begin
  for I := 0 to sl.Count - 1 do
    sl[I] := TrimLeft(sl[I]);
end;

// Helper
function GetTokenList(Input: string; Tokens: TStrings;
  Separator: string): Integer;
var
  S: string;
  L: Integer;
begin
  Result := 0;
  Tokens.Clear;
  S := Trim(Input);
  L := Pos(Separator, S);
  while L > 0 do
  begin
    Tokens.Add(SYSTEM.Copy(S, 1, L - 1));
    SYSTEM.Delete(S, 1, L);
    L := Pos(Separator, S);
  end;
  Tokens.Add(S);
  Result := Tokens.Count;
end;

// Helper
function GetToken(Input: string; Token: Integer; Separator: string): string;
var
  sl: TStringList;
begin
  Result := '';
  sl := TStringList.Create;
  try
    if Token < GetTokenList(Input, sl, Separator) then
      Result := sl[Token];
  finally
    sl.Free;
  end;
end;

function GetCondition1Text(ACondition: TBlockCondition1): string;
begin
  Result := '???';
  case ACondition of
    bcEqual:
      Result := stbcEqual;
    bcGreater:
      Result := stbcGreater;
    bcLess:
      Result := stbcLess;
    bcGreaterEqual:
      Result := stbcGreaterEqual;
    bcLessEqual:
      Result := stbcLessEqual;
    bcNotEqual:
      Result := obcNotEqual;
    bcContains:
      Result := stbcContains;
    bcNotContains:
      Result := stbcNotContains;
    bcFileExists:
      Result := stbcFileExists;
    bcPathExists:
      Result := stbcPathExists;
  end;
end;

{$IFNDEF D5TODO}
{$IFDEF DELPHI5}

type
  TVariantRelationship = (vrEqual, vrLessThan, vrGreaterThan, vrNotEqual);

function VarCompareValue(const A, B: Variant): TVariantRelationship;
begin
  Result := vrEqual;
end;

function BoolToStr(ABoolean: Boolean; UseBoolStrs: Boolean = False): string;
begin
  if not UseBoolStrs then
  begin
    if ABoolean then
      Result := '1'
    else
      Result := '0';
  end
  else
  begin
    if ABoolean then
      Result := 'True'
    else
      Result := 'False';
  end;
end;
{$ENDIF DELPHI5}
{$ENDIF ~D5TODO}

{ TMakeStudioPluginCommandItem }
constructor TCommandTypeItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  FHint := '';
  FCategory := '';
  FCompatibilityIndex := -1;
  FFileExtensions := '';

  FCallback := nil;
  FBitmap := nil;
end;

destructor TCommandTypeItem.Destroy;
begin
  FCallback := nil;
  FBitmap.Free;
  inherited Destroy;
end;

function TCommandTypeItem.GetIdentifier: string;
begin
  if FCallback <> nil then
    Result := FCallback.GetIdentifier
  else
    Result := dmmtNone;
end;

{ TMakeStudioPluginCommandHandler }
constructor TCommandTypes.Create;
begin
  inherited Create(TCommandTypeItem);
  FIFBlockCallback := TIFBlockCallback.Create(nil);
  FELSEBlockCallback := TELSEBlockCallback.Create(nil);
  FENDBlockCallback := TENDBlockCallback.Create(nil);
  FFORBlockCallback := TFORBlockCallback.Create(nil);
  FWHILEBlockCallback := TWHILEBlockCallback.Create(nil);
  FINCLUDEBlockCallback := TINCLUDEBlockCallback.Create(nil);
  FCOMMENTBlockCallback := TCOMMENTBlockCallback.Create(nil);

  FImageWidth := 16;
  FImageHeight := 16;
  AddSystemCommands;
end;

destructor TCommandTypes.Destroy;
begin
  FIFBlockCallback.Free;
  FELSEBlockCallback.Free;
  FENDBlockCallback.Free;
  FFORBlockCallback.Free;
  FWHILEBlockCallback.Free;
  FINCLUDEBlockCallback.Free;
  inherited;
end;

procedure TCommandTypes.AddSystemCommands;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.LoadFromResourceName(HInstance, UpperCase(IDIFBlock));
    Add(stdIFCaption, '', stProgramControlCategory, bmp, '', -1,
      ICommandCallback(FIFBlockCallback));
    bmp.LoadFromResourceName(HInstance, UpperCase(IDELSEBlock));
    Add(stdELSECaption, '', stProgramControlCategory, bmp, '', -1,
      ICommandCallback(FELSEBlockCallback));
    bmp.LoadFromResourceName(HInstance, UpperCase(IDENDBlock));
    Add(stdENDCaption, '', stProgramControlCategory, bmp, '', -1,
      ICommandCallback(FENDBlockCallback));
    bmp.LoadFromResourceName(HInstance, UpperCase(IDFORBlock));
    Add(stdFORCaption, '', stProgramControlCategory, bmp, '', -1,
      ICommandCallback(FFORBlockCallback));
    bmp.LoadFromResourceName(HInstance, UpperCase(IDWHILEBlock));
    Add(stdWHILECaption, '', stProgramControlCategory, bmp, '', -1,
      ICommandCallback(FWHILEBlockCallback));
    bmp.LoadFromResourceName(HInstance, UpperCase(IDINCLUDEBlock));
    Add(stdINCLUDECaption, '', stProgramControlCategory, bmp, ProgramDefaultExt,
      -1, ICommandCallback(FINCLUDEBlockCallback));
    bmp.LoadFromResourceName(HInstance, UpperCase(IDCOMMENT));
    Add(stdCOMMENTCaption, '', stSystemCategory, bmp, ProgramDefaultExt, -1,
      ICommandCallback(FCOMMENTBlockCallback));
  finally
    bmp.Free;
  end;
end;

procedure TCommandTypes.Add(const CommandName: WideString;
  const CommandHint: WideString; const CommandCategory: WideString;
  const ModuleBitmap: TBitmap; const DragDropFileExtensions: WideString;
  CompatibilityIndex: Integer; const Callback: IDispatch);
var
  M: TCommandTypeItem;
begin
  M := Add;
  M.Name := CommandName;
  M.Hint := CommandHint;
  M.Category := CommandCategory;
  if M.Category = '' then
    M.Category := stDefaultCategory;
  M.FileExtensions := DragDropFileExtensions;
  M.CompatibilityIndex := CompatibilityIndex;
  M.Callback := Callback as ICommandCallback;

  // Add the bitmap
  if (ModuleBitmap <> nil) then
  begin
    M.Bitmap := TBitmap.Create;
    M.Bitmap.Assign(ModuleBitmap);
    M.Bitmap.PixelFormat := pf8bit;
  end;
end;

function TCommandTypes.GetCommandTypeItem(Value: Integer): TCommandTypeItem;
begin
  Result := TCommandTypeItem(inherited Items[Value]);
end;

function TCommandTypes.Add: TCommandTypeItem;
begin
  Result := TCommandTypeItem(inherited Add);
end;

function TCommandTypes.GetItemByID(ID: string): TCommandTypeItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if SameText(ID, Items[I].Identifier) then
    begin
      Result := Items[I];
      Break;
    end
  end;
end;

function TCommandTypes.GetItemByExtension(AFileExtension: string)
  : TCommandTypeItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Pos(UpperCase(AFileExtension), UpperCase(Items[I].FFileExtensions)) > 0
    then
    begin
      // return only the first found item
      Result := Items[I];
      Break;
    end
  end;
end;

function TCommandTypes.GetItemsByExtension(AFileExtension: string)
  : TCommandTypeItemList;
var
  I: Integer;
begin
  Result := TCommandTypeItemList.Create;
  for I := 0 to Count - 1 do
  begin
    if Pos(UpperCase(AFileExtension), UpperCase(Items[I].FFileExtensions)) > 0
    then
    begin
      Result.Add(Items[I]);
    end
  end;
end;

function TCommandTypes.GetItemCountByExtension(AFileExtension: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Pos(UpperCase(AFileExtension), UpperCase(Items[I].FFileExtensions)) > 0
    then
      Inc(Result);
end;

function TCommandTypes.GetItemByCompatibilityIndex(AIndex: Integer)
  : TCommandTypeItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if AIndex = Items[I].CompatibilityIndex then
    begin
      Result := Items[I];
      Break;
    end
  end;
end;

{ TCustomCommand }
constructor TCustomCommand.Create(AProgram: TProgram);
begin
  inherited Create;
  FIdentifier := dmmtNone;
  FCaption := stAbstracTCustomCommand;
  FProgram := AProgram;
  FIndent := 0;
  FProgramLevel := 0;
  FBreakpoint := False;
end;

procedure TCustomCommand.SetIdentifier(Value: string);
begin
  FIdentifier := Value;
end;

procedure TCustomCommand.SetIgnoreExecutionError(const Value: Boolean);
begin
  FIgnoreExecutionError := Value;
end;

procedure TCustomCommand.SetIsIgnored(const Value: Boolean);
begin
  if Value <> FIsIgnored then
  begin
    FIsIgnored := Value;
    OwnerProgram.DoItemChanged(OwnerProgram.IndexOf(Self));
  end;
end;

function TCustomCommand.GetIdentifier: string;
begin
  Result := FIdentifier;
end;

destructor TCustomCommand.Destroy;
begin
  inherited Destroy;
end;

function TCustomCommand.GetCaption: string;
begin
  Result := FCaption;
end;

procedure TCustomCommand.SetCaption(Value: string);
begin
  FCaption := Value;
end;

function TCustomCommand.GetIndent: Integer;
begin
  Result := ProgramLevel * DEFAULTINDENT;
  if Result < 0 then
    Result := 0;
end;

function TCustomCommand.GetID: Integer;
begin
  Result := FProgram.IndexOf(Self);
end;

procedure TCustomCommand.GetFromStringList(sl: TStringList);
begin
  // FIdentifier := Owner.GetCommandTypeFromStringList(sl);
  Caption := sl.Values[stdcCaption];
  IsIgnored := StrToBoolDef(sl.Values[stdcIgnore], False);
end;

procedure TCustomCommand.WriteToStringList(sl: TStringList);
begin
  sl.Clear;
  sl.Add(FillSpaces(ProgramLevel * 2 + 2) + stdcCaption + '=' + Caption);
  sl.Add(FillSpaces(ProgramLevel * 2 + 2) + stdcIgnore + '=' +
    BoolToStr(IsIgnored));
  // sl.Add('  ' + stdcCommandType + '=' + Identifier);
end;

function TCustomCommand.Edit: Boolean;
begin
  MessageDlg(stdErrNoEditBaseCommand, mtInformation, [mbOK], 0);
  Result := True;
end;

function TCustomCommand.Run: Boolean;
begin
  DoLogbookMessage(stdBreak);
  DoLogbookMessage(stdErrNoRunBaseCommand);
  DoLogbookMessage(stdBreak);
  Result := True;
end;

function TCustomCommand.GetDrawHeight(Canvas: TCanvas): Integer;
begin
  Result := 2;
  Canvas.Font.Style := [fsBold];
  Result := Result + Canvas.TextHeight('X' + Caption) + 2;
  Canvas.Font.Style := [];
  Result := Result + Canvas.TextHeight('X' + Caption) + 2;
end;

procedure TCustomCommand.Draw(Canvas: TCanvas; Rect: TRect;
  State: TOwnerDrawState; ABreakPoint: Integer);
var
  Offset: Integer;
begin
  if odSelected in State then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.FillRect(Rect);
  end
  else
  begin
    Canvas.Brush.Color := clWindow;
    Canvas.FillRect(Rect);
  end;
  Offset := 2;
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(2, Rect.Top + Offset, stdErrCommandNotFound + Caption);
  // Offset := Canvas.TextHeight(Caption) + 2;
  Canvas.Font.Style := [];
  Canvas.Font.Color := clBlue;
  // Canvas.TextOut(10, Rect.Top + Offset, stNoEdit);
end;

procedure TCustomCommand.SetSelected(const Value: Boolean);
begin
  if Value <> FSelected then
  begin
    FSelected := Value;
    OwnerProgram.DoSelectionChanged(OwnerProgram.IndexOf(Self));
  end;
end;

procedure TCustomCommand.SetBreakpoint(const Value: Boolean);
begin
  if Value <> FBreakpoint then
  begin
    FBreakpoint := Value;
    OwnerProgram.DoItemChanged(OwnerProgram.IndexOf(Self));
  end;
end;

{ TCommand }
constructor TCommand.Create(ACommandType: TCommandTypeItem; AProgram: TProgram);
var
  V: IDispatch;
  DummyIntf: IUnknown;
begin
  inherited Create(AProgram);
  FCommand := nil;
  FCommand2 := nil;
  FCommandItem := ACommandType;

  if FCommandItem <> nil then
  begin
    V := FCommandItem.Callback.CreateCommand;
    if Supports(V, IID_ICommand2, DummyIntf) then
    begin
      FCommand2 := V as ICommand2;
    end
    else if Supports(V, IID_ICommand, DummyIntf) then
      FCommand := V as ICommand;
  end;

  if (FCommand = nil) and (FCommand2 = nil) then
    FCommandItem := nil;
end;

destructor TCommand.Destroy;
begin
  FCommand := nil;
  FCommand2 := nil;
  inherited Destroy;
end;

procedure TCommand.SetIdentifier(Value: string);
begin
  // FIdentifier := Value;
end;

function TCommand.GetIdentifier: string;
begin
  if FCommand <> nil then
  begin
    Result := FCommandItem.Identifier;
  end;
  if FCommand2 <> nil then
  begin
    Result := FCommandItem.Identifier;
  end;
end;

function TCommand.GetCaption: string;
begin
  Result := FCaption;
  if FCommand <> nil then
    Result := FCommand.Get_Caption;
  if FCommand2 <> nil then
    Result := FCommand2.Get_Caption;
end;

procedure TCommand.SetCaption(Value: string);
begin
  if FCommand <> nil then
    FCommand.Set_Caption(Value)
  else if FCommand2 <> nil then
    FCommand2.Set_Caption(Value)
  else
    FCaption := Value;
end;

procedure TCommand.SetFilename(AFilename: string);
begin
  if FCommand <> nil then
    FCommand.SetFilename(AFilename);
  if FCommand2 <> nil then
    FCommand2.SetFilename(AFilename);
end;

function TCommand.MakeIndent: string;
begin
  Result := FillSpaces(ProgramLevel * 2 + 2);
end;

procedure TCommand.GetFromStringList(sl: TStringList);
var
  I: Integer;
  S: String;
begin
  inherited GetFromStringList(sl);
  TrimStringListLeft(sl);
  try
    for I := 0 to sl.Count - 1 do
    begin
      S := sl.Values[sl.Names[I]]; // for error handling
      if FCommand <> nil then
        FCommand.ParamValues[sl.Names[I]] := sl.Values[sl.Names[I]];
      if FCommand2 <> nil then
        FCommand2.ParamValues[sl.Names[I]] := sl.Values[sl.Names[I]];
    end;
  except
    On E: Exception do
    begin
      DoLogbookMessage(Format(StrErrorSettingParam, [FCommand.Caption, S,
        E.Message]));
    end;
  end;
end;

procedure TCommand.WriteToStringList(sl: TStringList);
var
  sl1: TStringList;
  I: Integer;
begin
  inherited WriteToStringList(sl);
  begin
    sl1 := TStringList.Create;
    try
      if FCommand <> nil then
        for I := 0 to FCommand.ParamCount - 1 do
          sl.Add(MakeIndent + FCommand.ParamNames[I] + '=' +
            FCommand.ParamValues[FCommand.ParamNames[I]]);

      if FCommand2 <> nil then
        for I := 0 to FCommand2.ParamCount - 1 do
          sl.Add(MakeIndent + FCommand2.ParamNames[I] + '=' +
            FCommand2.ParamValues[FCommand2.ParamNames[I]]);

      sl.AddStrings(sl1);
    finally
      sl1.Free;
    end;
  end;
end;

function TCommand.Edit: Boolean;
begin
  Result := False;
  if FCommand <> nil then
    Result := FCommand.EditItem;
  if FCommand2 <> nil then
    Result := FCommand2.EditItem;
  if Result then
  begin
    OwnerProgram.DoItemChanged(OwnerProgram.IndexOf(Self));
    OwnerProgram.Modified := True;
  end;
end;

function TCommand.Run: Boolean;
begin
  Result := False;
  if FCommand <> nil then
    Result := FCommand.ExecuteItem;
  if FCommand2 <> nil then
  begin

    // the only command which is handeled escpecially is INCLUDE
    // because a new instance of TProgram is required and the Level of INCLUDES has
    // to be counted to provide a stack overflow
    if CommandItem.Identifier = IDINCLUDEBlock then
    begin
      Result := RunInclude(FCommand2);
    end

    // else - every other command
    else
      Result := FCommand2.ExecuteItem;
  end;
end;

function TCommand.GetDrawHeight(Canvas: TCanvas): Integer;
var
  I: Integer;
  sl: TStringList;
begin
  Result := 20;
  if FCommand <> nil then
    Result := FCommand.MeasureItem(Canvas.Handle, OwnerProgram.BriefView);
  if FCommand2 <> nil then
  begin
    if FCommand2.OwnerDraw then
      Result := FCommand2.MeasureItem(Canvas.Handle, OwnerProgram.BriefView)
    else
      Result := -1;
  end;

  // nur Titel darstellen
  if IsIgnored then
    Result := -1;

  if Result < 0 then // auto
  begin
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(Caption) + 2;
    if (not FProgram.BriefView) and (not IsIgnored) then
    begin
      Canvas.Font.Style := [];

      if FCommand <> nil then
        for I := 0 to FCommand.ParamCount - 1 do
          Result := Result + Canvas.TextHeight(FCommand.ParamNames[I]) + 2;

      if FCommand2 <> nil then
      begin
        sl := TStringList.Create;
        try
          sl.Text := FCommand2.PreviewText;
          for I := 0 to sl.Count - 1 do
            Result := Result + Canvas.TextHeight(sl[I]) + 2;
        finally
          sl.Free;
        end;
      end;

    end;
  end;

  if Result < 20 then
  begin
    Result := 20;
  end;

  if CommandItem <> nil then
  begin
    if Result < CommandItem.Bitmap.Height + 4 then
    begin
      Result := CommandItem.Bitmap.Height + 4;
    end;
  end;
end;

procedure TCommand.Draw(Canvas: TCanvas; Rect: TRect; State: TOwnerDrawState;
  ABreakPoint: Integer);
var
  TextRect: TRect;
  CommandBitmapRect: TRect;
  GutterRect: TRect;
  sl: TStringList;

  Offset, I: Integer;
  InternalPaint: Boolean;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if odSelected in State then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Canvas.Font.Name := 'Tahoma';

  if ((FCommand = nil) and (FCommand2 = nil)) or (FCommandItem = nil) then
  begin
    inherited;
    Exit;
  end;

  GutterRect := Rect;
  CommandBitmapRect := Rect;
  TextRect := Rect;
  if OwnerProgram.ShowLines then
    GutterRect.Right := GutterRect.Left + ProgramGutterWidth;

  if OwnerProgram.ShowLines then
    CommandBitmapRect.Left := GutterRect.Right + 1 + Indent;
  CommandBitmapRect.Right := CommandBitmapRect.Left +
    CommandItem.Bitmap.Width + 4;

  TextRect.Left := CommandBitmapRect.Right + 1;

  if odSelected in State then
    Canvas.Brush.Color := clHighlight
  else
    Canvas.Brush.Color := clWindow;
  Canvas.FillRect(TextRect);

  if IsIgnored then
    InternalPaint := True
  else
  begin
    InternalPaint := False;
    if FCommand <> nil then
      InternalPaint := FCommand.DrawItem(Canvas.Handle, TextRect.Left,
        TextRect.Top, TextRect.Right, TextRect.Bottom, odSelected in State,
        OwnerProgram.BriefView, Canvas.Brush.Color);
    if FCommand2 <> nil then
    begin
      InternalPaint := not FCommand2.OwnerDraw;
      if FCommand2.OwnerDraw then
        InternalPaint := FCommand2.DrawItem(Canvas.Handle, TextRect.Left,
          TextRect.Top, TextRect.Right, TextRect.Bottom, odSelected in State,
          OwnerProgram.BriefView, Canvas.Brush.Color);
    end;
  end;

  if InternalPaint then
  begin

    Offset := 2;

    if IsIgnored then
    begin
      SetCanvasTextColor(clGrayText);
      Canvas.Font.Style := [fsItalic];
    end
    else
    begin
      SetCanvasTextColor(clMenuText);
      Canvas.Font.Style := [fsBold];
    end;

    Canvas.TextOut(TextRect.Left + 2, TextRect.Top + Offset, Caption);
    if (not FProgram.BriefView) and (not IsIgnored) then
    begin
      Offset := Canvas.TextHeight(Caption) + 2;

      if IsIgnored then
      begin
        Canvas.Font.Style := [fsItalic];
        SetCanvasTextColor(clGrayText);
      end
      else
      begin
        Canvas.Font.Style := [];
        SetCanvasTextColor(clBlue);
      end;

      if FCommand <> nil then
        for I := 0 to FCommand.ParamCount - 1 do
        begin
          Canvas.TextOut(TextRect.Left + 10, TextRect.Top + Offset,
            FCommand.ParamNames[I] + '=' + FCommand.ParamValues
            [FCommand.ParamNames[I]]);
          Offset := Offset + Canvas.TextHeight(FCommand.ParamNames[I] + '=' +
            FCommand.ParamValues[FCommand.ParamNames[I]]) + 2;
        end;
      if FCommand2 <> nil then
      begin
        sl := TStringList.Create;
        try
          sl.Text := FCommand2.PreviewText;
          for I := 0 to sl.Count - 1 do
          begin
            Canvas.TextOut(TextRect.Left + 10, TextRect.Top + Offset, sl[I]);
            Offset := Offset + Canvas.TextHeight(sl[I]) + 2;
          end;
        finally
          sl.Free;
        end;
      end;
    end;
  end;

  // paint bitmap
  if CommandItem.Bitmap <> nil then
  begin
    if CommandItem.Bitmap.Width > 0 then
    begin
      CommandItem.Bitmap.TransparentMode := tmAuto;
      CommandItem.Bitmap.Transparent := True;
      Canvas.Draw(CommandBitmapRect.Left + 2, CommandBitmapRect.Top + 2,
        CommandItem.Bitmap)
    end
  end;

  // Fill space between line number and indent
  { if ProgramLevel > 0 then
    begin
    bRect := Rect;
    if FProgram.ShowLines then
    bRect.Left := DEFAULTLINEINDENT;
    bRect.Right := Indent;
    Canvas.Brush.Color := clWindow;
    Canvas.FillRect(bRect);
    Canvas.Pen.Color := clBtnShadow;
    Canvas.Pen.Style := psDot;
    Canvas.MoveTo(bRect.Right - 8, bRect.Top);
    Canvas.LineTo(bRect.Right - 8, bRect.Bottom);
    end; }

  if Breakpoint then
    FProgram.ShowLines := True;

  // paint line number
  if FProgram.ShowLines then
  begin
    Canvas.Brush.Color := $00F4F4F4;
    Canvas.FillRect(GutterRect);
    Canvas.Font.Name := 'Courier New';
    Canvas.Font.Color := $00CC9999;
    // Canvas.Font.Style := [fsBold];
    Canvas.TextOut(GutterRect.Left + 2, GutterRect.Top + 2, IntToStr(ID + 1));
  end;

  // paint Breakpoint
  if Breakpoint then
  begin
    FProgram.BreakpointBmp.TransparentMode := tmAuto;
    FProgram.BreakpointBmp.Transparent := True;
    Canvas.Draw(GutterRect.Left + 2, GutterRect.Top + 2,
      FProgram.BreakpointBmp);
  end;

  // paint ignored line glyph
  if IsIgnored then
  begin
    FProgram.CommentBmp.TransparentMode := tmAuto;
    FProgram.CommentBmp.Transparent := True;
    Canvas.Draw(GutterRect.Left + 20, GutterRect.Top + 2, FProgram.CommentBmp);
  end;
end;

function TCommand.GetOwnerDraw: Boolean;
begin
  if SupportsCommand2 then
    Result := FCommand2.OwnerDraw
  else
    Result := True;
end;

function TCommand.GetPreviewText: string;
begin
  if SupportsCommand2 then
    Result := FCommand2.PreviewText
  else
    Result := '';
end;

function TCommand.Notifiy(Notification: WideString; Parameter: OleVariant)
  : OleVariant;
begin
  if SupportsCommand2 then
    Result := FCommand2.Notify(Notification, Parameter)
  else
    Result := varEmpty;
end;

function TCommand.GetSupportsCommand2: Boolean;
begin
  Result := FCommand2 <> nil;
end;

function TCommand.RunInclude(aCommand: ICommand2): Boolean;
var
  IncludeFilename: String;
  IncludeProgram: TProgram;
  I: Integer;
  OldFilename:String;
begin
  Result := False;
  IncludeFilename := VarhandlerRef.ReplaceVarsInString
    (FCommand2.ParamValues[stdcFilename]);
  if FileExists(IncludeFilename) then
  begin

    IncludeProgram := TProgram.Create;
    OldFilename := FProgram.Filename; //save actual filename
    try
      IncludeProgram.IncludeLevel := FProgram.IncludeLevel + 1;

      if IncludeProgram.IncludeLevel < 20 then
      begin

        // assign Active Program property
        if FProgram.RootProgram <> nil then
        begin
          FProgram.RootProgram.ActiveProgram := IncludeProgram;

          try
            // Set Root for include program
            IncludeProgram.RootProgram := FProgram.RootProgram;
            IncludeProgram.IsInclude := True;

            // assign all Notifications
            for I := 0 to FProgram.FNotificationList.Count - 1 do
              IncludeProgram.AddNotification
                (TProgramNotificationItem(FProgram.FNotificationList.Items[I])
                .Notification);

            // Load Program
            IncludeProgram.LoadFromFile(IncludeFilename);

            // Debugging and Logbook
            IncludeProgram.HaltOnNextCommand := FProgram.HaltOnNextCommand;
            IncludeProgram.ClearLogbookOnStart := False;

            // Run Program
            DoLogbookMessage(Format(stdIncludeStarted, [IncludeFilename]));
            Result := IncludeProgram.Execute;

          finally
            // assign active program to caller
            FProgram.RootProgram.ActiveProgram := FProgram;

            // prevent error on refreshing the active program - clear include
            // notification list
            IncludeProgram.FNotificationList.Clear;

            // Refresh previous program
            FProgram.DoFullRefresh;

            // Debugging
            FProgram.HaltOnNextCommand := IncludeProgram.HaltOnNextCommand;
          end;
        end;

      end
      else
      begin // Stack Overflow
        DoLogbookMessage(stdErrIncludeStackOverflow);
        MessageDlg(stdErrIncludeStackOverflow, mtError, [mbOK], 0);
      end;

    finally
      IncludeProgram.Free;
      //restore actual filename
      VarhandlerRef.SetVar( 'ActiveScriptFile', OldFilename);
    end;

  end
  else
    DoLogbookMessage(Format(stdErrIncludeFileNotFoud, [IncludeFilename]));
end;

{ TProgram }
constructor TProgram.Create;
begin
  inherited Create;
  FNotificationList := TCollection.Create(TProgramNotificationItem);
  FFilename := '';
  FModified := False;
  FCanceled := False;
  FBriefView := False;
  FShowLines := True;
  FBreakpointBmp := TBitmap.Create;
  FBreakpointBmp.LoadFromResourceName(HInstance, 'BULLETRED');
  FCommentBmp := TBitmap.Create;
  FCommentBmp.LoadFromResourceName(HInstance, 'COMMENT');
  FPositionBmp := TBitmap.Create;
  FPositionBmp.LoadFromResourceName(HInstance, 'ARROWGREEN');
  FClearLogbookOnStart := True;
  FRootProgram := nil;
  FActiveProgram := Self;
  IsInclude := False;

  FInitialized := False;
  FSelected := nil;
end;

destructor TProgram.Destroy;
begin
  Clear;

  ClearNotifications;
  FNotificationList.Free;

  FBreakpointBmp.Free;
  FCommentBmp.Free;
  FPositionBmp.Free;
  inherited Destroy;
end;

procedure TProgram.Notify(Ptr: Pointer; Action: TListNotification);
begin
  Refresh;

  inherited Notify(Ptr, Action);
end;

function TProgram.GetCommandTypeFromStringList(sl: TStrings): string;
begin
  Result := dmmtNone;
  TrimStringListLeft(sl);
  try
    if sl.Values[stdcCommandType] = '' then
    begin
      Result := dmmtNone;
      if CommandTypes.GetItemByCompatibilityIndex(1) <> nil then
      begin
        Result := CommandTypes.GetItemByCompatibilityIndex(1).Identifier
      end;
    end
    else if Length(sl.Values[stdcCommandType]) < 3 then
    begin
      if CommandTypes.GetItemByCompatibilityIndex
        (StrToInt(sl.Values[stdcCommandType])) <> nil then
      begin
        Result := CommandTypes.GetItemByCompatibilityIndex
          (StrToInt(sl.Values[stdcCommandType])).Identifier
      end;
    end
    else
    begin
      Result := sl.Values[stdcCommandType];
    end;
  except
    Result := dmmtNone;
  end;
end;

function TProgram.CreateCommandFromStringList(sl: TStrings; CommandType: string)
  : TCustomCommand;
var
  LCommandType: string;
begin
  Result := nil;

  if CommandType <> '' then
    LCommandType := CommandType
  else
    LCommandType := GetCommandTypeFromStringList(sl);

  if SameText(LCommandType, dmmtNone) then
    Result := TCustomCommand.Create(Self)
  else
  begin
    if CommandTypes.GetItemByID(LCommandType) <> nil then
      Result := TCommand.Create(CommandTypes.GetItemByID(LCommandType), Self)
    else
    begin
      Result := TCommand.Create(nil, Self);
      // Result.Caption := Format(stdErrCommandNotFound, [aCommandType]);
    end;
  end;

  if Result = nil then
    Result := TCustomCommand.Create(Self);
end;

function TProgram.LoadFromFile(AFilename: string): Boolean;
var
  sl: TStringList;
begin
  Result := False;
  Clear;
  sl := TStringList.Create;
  try
    try
      sl.LoadFromFile(AFilename);
      FFilename := AFilename;
      Modified := False;
      LoadFromStrings(sl);
      Result := True;
      VarhandlerRef.SetVar( 'ActiveScriptFile', FFilename);
    except
      On E: Exception do
        DoLogbookMessage(E.Message);
    end;
  finally
    sl.Free;
  end;
end;

function TProgram.SaveToFile(AFilename: string): Boolean;
var
  sl: TStringList;
begin
  Result := False;
  sl := TStringList.Create;
  try
    SaveToStrings(sl);
    FFilename := AFilename;
    Modified := False;
    sl.SaveToFile(AFilename);
    DoRefresh;
    Result := True;
  finally
    sl.Free;
  end;
end;

function TProgram.Execute: Boolean;
begin
  Result := Execute(0);
end;

function TProgram.Execute(StartIndex: Integer): Boolean;
var
  HadError: Boolean;

  function GetBlockForward(idx: Integer; Ident: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := idx + 1 to Count - 1 do
      if Items[I].ProgramLevel = Items[idx].ProgramLevel then
      begin
        if Items[I].Identifier = Ident then
          Result := I;
        Break;
      end;
  end;

  function GetBlockBackward(idx: Integer; Ident: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := idx - 1 downto 0 do
      if Items[I].ProgramLevel = Items[idx].ProgramLevel then
      begin
        if Items[I].Identifier = Ident then
          Result := I;
        Break;
      end;
  end;

  procedure DoRun(_StartIndex: Integer);
  var
    I, I1, li: Integer;
  begin
    I := _StartIndex;
    I1 := I;
    li := -1;
    while I < Count do
    begin
      DoSetProgress(Items[I].Caption, 0, Count, I);
      DoSetProgramPosition(I);

      if not Items[I].IsIgnored then
      begin

        DoLogbookMessage('Line: '+IntToStr(I+1));

        if (Items[I].Breakpoint) or HaltOnNextCommand then
        begin
          IsPaused := True;
          HaltOnNextCommand := False;
          while IsPaused and not Canceled do
          begin
            DoProgramPaused;
            Application.ProcessMessages;
          end;
        end;

        if Canceled then
          Exit;

        I1 := I;
        if IsSystemCommand(Items[I]) then
        begin
          if Items[I].Identifier = IDIFBlock then
          begin
            if Items[I].Run then
              Inc(I)
            else
            begin
              if GetBlockForward(I, IDELSEBlock) > 0 then
                I := GetBlockForward(I, IDELSEBlock) + 1
              else if GetBlockForward(I, IDENDBlock) > 0 then
                I := GetBlockForward(I, IDENDBlock) + 1;
            end;
          end
          else if Items[I].Identifier = IDELSEBlock then
          begin
            I := GetBlockForward(I, IDENDBlock);
          end
          else if Items[I].Identifier = IDFORBlock then
          begin
            if li < I then
            begin
              Items[I].Notifiy('Reset', 0);
            end;
            if Items[I].Run then
              Inc(I)
            else
              I := GetBlockForward(I, IDENDBlock) + 1;
          end
          else if Items[I].Identifier = IDWHILEBlock then
          begin
            if Items[I].Run then
              Inc(I)
            else
              I := GetBlockForward(I, IDENDBlock) + 1;
          end
          else if Items[I].Identifier = IDENDBlock then
          begin
            if GetBlockBackward(I, IDFORBlock) > -1 then
              I := GetBlockBackward(I, IDFORBlock)
            else if GetBlockBackward(I, IDWHILEBlock) > -1 then
              I := GetBlockBackward(I, IDWHILEBlock)
            else // If or Else
              Inc(I);
          end
        end // If IsSystemCommand
        else if not Items[I].Run then
        begin
          HadError := True;
          DoLogbookMessage('ERROR in Line: '+IntToStr(I+1));
          Break;
        end
        else
        begin
          DoSetProgress(Items[I].Caption, 0, Count, I + 1);
          Inc(I);
          if Canceled then
          begin
            Break;
          end;
        end;

      end // If not IsIgnored
      else
        Inc(I);

      li := I1;
    end;
  end;

begin
  Result := False;
  if not CheckSyntax then
    Exit;

  if ClearLogbookOnStart then
    DoClearLogbook;

  Canceled := False;
  HadError := False;

  try
    DoSetProgress('', StartIndex, Count, 1);
    IsPaused := False;
    IsRun := True;
    DoBeforeStartProgram;

    try
      DoRun(StartIndex);
    except
      on E: Exception do
      begin
        DoLogbookMessage(E.Message);
        HadError := True;
      end;
    end;

    IsRun := False;
    if not IsInclude then
    begin
      DoLogbookMessage('');
      DoLogbookMessage(stdBreak);
    end;

    if Canceled then
    begin
      DoLogbookMessage(stdCanceled);
    end
    else if HadError then
    begin
      if not IsInclude then
        DoLogbookMessage(stdError);
    end
    else
    begin
      if not IsInclude then
        DoLogbookMessage(stdReady);
    end;
    Result := True;
  finally
  end;

  if Canceled or HadError then
    Result := False
  else
    Result := True;

  if not IsInclude then
    DoAfterStopProgram;
end;

procedure TProgram.New;
begin
  Clear;
  FFilename := '';
  Modified := False;
  DoRefresh;
end;

function TProgram.LoadFromStrings(sl: TStrings; Insert: Boolean;
  InsertAt: Integer): Boolean;
var
  M: TCustomCommand;
  slm: TStringList;
  AddToCommandList: Boolean;
  ctype: string;

  procedure LoadFromStringsV0;
  var
    I: Integer;
  begin
    for I := 0 to sl.Count - 1 do
    begin
      if (CompareText(GetToken(sl[I], 0, ' '), stcBegin) = 0) or
        (CompareText(GetToken(sl[I], 0, ' '), stcObject) = 0) then
      begin
        slm.Clear;
        ctype := GetToken(sl[I], 1, ' ');
        AddToCommandList := True;
      end
      else if CompareText(GetToken(sl[I], 0, ' '), stcEnd) = 0 then
      begin
        M := CreateCommandFromStringList(slm, ctype);
        M.GetFromStringList(slm);

        if not(Insert) or (InsertAt < 0) or (InsertAt > Count) then
          Add(M)
        else
        begin
          Self.Insert(InsertAt, M);
          Inc(InsertAt);
        end;
        AddToCommandList := False;
      end
      else if AddToCommandList then
      begin
        slm.Add(sl[I]);
      end;
    end;
  end;

begin
  Result := False;
  if not Insert then
    Clear;
  ctype := '';
  slm := TStringList.Create;
  try
    AddToCommandList := False;
    TrimStringListLeft(sl);
    if Pos(stdcVersion, sl[0]) <> 0 then
    begin
      LoadFromStringsV0;
    end
    else
      LoadFromStringsV0;
  finally
    slm.Free;
  end;
  Refresh;
  Result := True;
end;

function TProgram.LoadFromStrings(sl: TStrings): Boolean;
begin
  Result := LoadFromStrings(sl, False, 0);
end;

function TProgram.SaveToStrings(sl: TStrings): Boolean;
begin
  Result := SaveToStrings(sl, False);
end;

function TProgram.SaveToStrings(sl: TStrings; OnlySelected: Boolean): Boolean;
var
  slm: TStringList;
  I: Integer;
begin
  Result := False;
  slm := TStringList.Create;
  try
    sl.Add(stdcVersion + '=' + IntToStr(iProgramVersion));
    for I := 0 to Count - 1 do
    begin
      if (OnlySelected and TCustomCommand(Items[I]).Selected) or
        (not OnlySelected) then
      begin
        sl.Add(FillSpaces(Items[I].ProgramLevel * 2) + stcBegin + ' ' +
          TCustomCommand(Items[I]).Identifier);
        TCustomCommand(Items[I]).WriteToStringList(slm);
        sl.AddStrings(slm);
        sl.Add(FillSpaces(Items[I].ProgramLevel * 2) + stcEnd);
      end;
    end;
  finally
    slm.Free;
  end;
  Result := True;
end;

procedure TProgram.DoSetProgress(const ACaption: string;
  const ProgressMin, ProgressMax, ProgressPosition: Integer);
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnSetProgress(ACaption, ProgressMin, ProgressMax,
      ProgressPosition);
end;

procedure TProgram.SetCanceled(Value: Boolean);
var
  I: Integer;
begin
  FCanceled := Value;

  // Set all Commandtypes for Canceled = Value;
  for I := 0 to CommandTypes.Count - 1 do
  begin
    if CommandTypes[I].Callback <> nil then
    begin
      CommandTypes[I].Callback.SetCanceled(FCanceled)
    end
  end;
end;

function TProgram.CheckSyntax: Boolean;
var
  IfLevels: array of Integer;
  Level, I: Integer;
begin
  Refresh;

  if Count = 0 then
  begin
    Result := True;
    Exit;
  end;

  Result := Items[Count - 1].ProgramLevel = 0;

  if not Result then
    if Items[Count - 1].ProgramLevel > 0 then
      MessageDlg(stdErrTooManyEndingBlocks, mtError, [mbOK], 0)
    else
      MessageDlg(stdErrTooFewEndingBlocks, mtError, [mbOK], 0);

  // check if else has corresponding if-statement
  if Result then
  begin
    Level := 0;
    SetLength(IfLevels, 0);
    for I := 0 to Count - 1 do
    begin
      if (Pos(IDBlock, Items[I].Identifier) <> 0) and
        not(SameText(Items[I].Identifier, IDINCLUDEBlock)) then
      begin
        if not Items[I].IsIgnored then
        begin

          if SameText(Items[I].Identifier, IDIFBlock) then
          begin
            SetLength(IfLevels, Length(IfLevels) + 1);
            IfLevels[Length(IfLevels) - 1] := Level;
            Inc(Level);
          end
          else if SameText(Items[I].Identifier, IDFORBlock) then
          begin
            Inc(Level);
          end
          else if SameText(Items[I].Identifier, IDWHILEBlock) then
          begin
            Inc(Level);
          end
          else if SameText(Items[I].Identifier, IDELSEBlock) then
          begin
            if Length(IfLevels) > 0 then
            begin
              if IfLevels[Length(IfLevels) - 1] <> Level - 1 then
              begin
                Result := False;
                MessageDlg(stdErrNoIFBlockToElse, mtError, [mbOK], 0)
              end
            end
            else
            begin
              Result := False;
              MessageDlg(stdErrTooManyElseBlocks, mtError, [mbOK], 0)
            end;
          end
          else
          begin
            Dec(Level);
            if Length(IfLevels) > 0 then
              if IfLevels[Length(IfLevels) - 1] = Level then
                SetLength(IfLevels, Length(IfLevels) - 1);
            Items[I].ProgramLevel := Level;
          end;

        end; // if not IsIgnored
      end
      else
        Items[I].ProgramLevel := Level;
    end;
  end;
end;

procedure TProgram.Refresh;
var
  I: Integer;
  Level: Integer;
begin
  Level := 0;

  for I := 0 to Count - 1 do
  begin
    if (Pos(IDBlock, Items[I].Identifier) <> 0) and
      not(SameText(Items[I].Identifier, IDINCLUDEBlock)) then
    begin
      if SameText(Items[I].Identifier, IDIFBlock) or
        SameText(Items[I].Identifier, IDFORBlock) or
        SameText(Items[I].Identifier, IDWHILEBlock) then
      begin
        Items[I].ProgramLevel := Level;
        if not Items[I].IsIgnored then
          Inc(Level);
      end
      else if SameText(Items[I].Identifier, IDELSEBlock) then
      begin
        if not Items[I].IsIgnored then
          Items[I].ProgramLevel := Level - 1;
      end
      else
      begin
        if not Items[I].IsIgnored then
          Dec(Level);
        Items[I].ProgramLevel := Level;
      end;
    end
    else
      Items[I].ProgramLevel := Level;
  end;

  DoRefresh;
end;

function TProgram.IsSystemCommand(aCommand: TCommand): Boolean;
begin
  Result := (Pos(IDBlock, aCommand.Identifier) <> 0) and
    not(SameText(aCommand.Identifier, IDINCLUDEBlock));
end;

function TProgram.GetItem(Index: Integer): TCommand;
begin
  Result := TCommand(inherited Items[Index]);
end;

procedure TProgram.SetItem(Index: Integer; Value: TCommand);
begin
  inherited Items[Index] := Value;
end;

procedure TProgram.RemoveNotification(Notification: IProgramNotification);
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    if TProgramNotificationItem(FNotificationList.Items[I]).Notification = Notification
    then
    begin
      FNotificationList.Delete(I);
      Break;
    end;
end;

procedure TProgram.AddNotification(Notification: IProgramNotification);
begin
  TProgramNotificationItem(FNotificationList.Add).Notification := Notification;
end;

procedure TProgram.ClearNotifications;
begin
  FNotificationList.Clear;
end;

procedure TProgram.CopySelectionToClipboard;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    SaveToStrings(sl, True);
    Clipboard.AsText := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TProgram.CutSelectionToClipboard;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    SaveToStrings(sl, True);
    Clipboard.AsText := sl.Text;
    DeleteSelectedItems;
  finally
    sl.Free;
  end;
end;

procedure TProgram.ClearSelection;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TCommand(Items[I]).Selected := False;
end;

function TProgram.CanCopy: Boolean;
begin
  Result := SelectedCount > 0;
end;

procedure TProgram.SelectAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TCommand(Items[I]).Selected := True;
end;

procedure TProgram.PasteFromClipboard;
var
  sl: TStringList;
begin
  if CanPaste then
  begin
    sl := TStringList.Create;
    try
      sl.Text := Clipboard.AsText;
      LoadFromStrings(sl, True, IndexOf(Selected));
    finally
      sl.Free;
    end;
  end;
end;

function TProgram.CanPaste: Boolean;
var
  S: string;
begin
  S := Clipboard.AsText;
  Result := (S <> '') and ((Pos(stcBegin, S) <> 0) or (Pos(stcObject, S) <> 0));
end;

function TProgram.Add(AObject: TObject): Integer;
begin
  Result := inherited Add(AObject);
  DoAddItem(IndexOf(AObject));

  if IsSystemCommand(AObject as TCommand) then
    Refresh;
end;

procedure TProgram.Exchange(Index1, Index2: Integer);
begin
  inherited Exchange(Index1, Index2);
  Modified := True;
  DoExchangeItem(Index1, Index2);
end;

procedure TProgram.DoFullRefresh;
var
  I: Integer;
  K: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
  begin
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnClearItems;
    for K := 0 to Count - 1 do
      TProgramNotificationItem(FNotificationList.Items[I])
        .Notification.OnAddItem(K);
  end;
end;

function TProgram.Remove(AObject: TObject): Integer;
begin
  DoDeleteItem(IndexOf(AObject));
  Result := inherited Remove(AObject);
end;

procedure TProgram.Delete(Index: Integer);
begin
  DoDeleteItem(Index);
  inherited Delete(Index);
end;

procedure TProgram.Clear;
begin
  DoClearItems;
  inherited Clear;
end;

procedure TProgram.Insert(Index: Integer; AObject: TObject);
begin
  inherited Insert(Index, AObject);
  DoInsertItem(Index);
end;

procedure TProgram.DoItemChanged(const ItemIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnItemChanged(ItemIndex);
end;

procedure TProgram.DoAfterStopProgram;
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnAfterStopProgram;
end;

procedure TProgram.DoInsertItem(const InsertAt: Integer);
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnInsertItem(InsertAt);
end;

procedure TProgram.DoAddItem(const ItemIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I]).Notification.OnAddItem
      (ItemIndex);
end;

procedure TProgram.DoProgramPaused;
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnProgramPaused;
end;

procedure TProgram.DoExchangeItem(const Index1, Index2: Integer);
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnExchangeItem(Index1, Index2);
end;

procedure TProgram.DoBeforeStartProgram;
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnBeforeStartProgram;
end;

procedure TProgram.DoSelectionChanged(const ItemIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnSelectionChanged(ItemIndex);
end;

procedure TProgram.DoClearItems;
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnClearItems;
end;

procedure TProgram.DoSetProgramPosition(const ItemIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnSetProgramPosition(ItemIndex);
end;

procedure TProgram.DoDeleteItem(const ItemIndex: Integer);
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I])
      .Notification.OnDeleteItem(ItemIndex);
end;

procedure TProgram.DoRefresh;
var
  I: Integer;
begin
  for I := 0 to FNotificationList.Count - 1 do
    TProgramNotificationItem(FNotificationList.Items[I]).Notification.OnRefresh;
end;

procedure TProgram.DeleteSelectedItems;
var
  I: Integer;
begin
  if SelectedCount > 0 then
    Modified := True;
  while SelectedCount > 0 do
  begin
    for I := 0 to Count do
      if Items[I].Selected then
      begin
        Delete(I);
        Break;
      end;
  end;
end;

function TProgram.GetSelectedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].Selected then
      Inc(Result);
end;

procedure TProgram.SetBriefView(const Value: Boolean);
begin
  if Value <> FBriefView then
  begin
    FBriefView := Value;
    Refresh;
  end;
end;

procedure TProgram.SetShowLines(const Value: Boolean);
begin
  if Value <> FShowLines then
  begin
    FShowLines := Value;
    Refresh;
  end;
end;

function TProgram.GetSelected: TCommand;
begin
  Result := nil;
  if IndexOf(FSelected) > -1 then
    Result := FSelected;
end;

procedure TProgram.SetSelected(const Value: TCommand);
begin
  if Value <> FSelected then
  begin
    FSelected := Value;
  end;
end;

{ TProgramNotificationItem }

constructor TProgramNotificationItem.Create(Collection: TCollection);
begin
  inherited;
  FNotification := nil;
end;

destructor TProgramNotificationItem.Destroy;
begin
  FNotification := nil;
  inherited;
end;

{ IF BLOCK ******************************************************** }
function TIFBlockCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TIFBlock.Create(nil));
end;

procedure TIFBlockCallback.SetCanceled(ACanceled: WordBool);
begin
  // Canceled := ACanceled;
end;

constructor TIFBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdIFCaption;
  FCondition := bcEqual;
  FVarname := '';
  FValue := '';
end;

destructor TIFBlock.Destroy;
begin
  inherited Destroy;
end;

function TIFBlock.EditItem: WordBool;
begin
  Result := DlgEditIf(Self);
end;

function TIFBlock.ExecuteItem: WordBool;
begin
  Result := False;

  if not VarhandlerRef.VarExists(Varname) then
    VarhandlerRef.SetVarEx(Varname, '');

  try
    case Condition of
      bcEqual:
        Result := VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrEqual;
      bcGreater:
        Result := VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrGreaterThan;
      bcLess:
        Result := VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrLessThan;
      bcGreaterEqual:
        Result := (VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrEqual) or (VarCompareValue(VarhandlerRef.GetVar(Varname),
          VarValue) = vrGreaterThan);
      bcLessEqual:
        Result := (VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrEqual) or (VarCompareValue(VarhandlerRef.GetVar(Varname),
          VarValue) = vrLessThan);
      bcNotEqual:
        Result := VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrNotEqual;
      bcContains:
        Result := Pos(VarValue, VarhandlerRef.GetVar(Varname)) > 0;
      bcNotContains:
        Result := Pos(VarValue, VarhandlerRef.GetVar(Varname)) = 0;
      bcFileExists:
        Result := FileExists(VarhandlerRef.GetVar(Varname));
      bcPathExists:
        Result := DirectoryExists(VarhandlerRef.GetVar(Varname));
    end;

    DoLogbookMessage(FCaption + ' ' + Varname + ' ' +
      GetCondition1Text(Condition) + ' ' + VarValue);
    DoLogbookMessage('->' + BoolToStr(Result, True));
  except
    on E: Exception do
    begin
      DoLogbookMessage('ERROR on IF...');
      DoLogbookMessage(E.Message);
      Result := False;
    end;
  end;
end;

function TIFBlock.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TIFBlock.Notify(const Notification: WideString; Parameter: OleVariant)
  : OleVariant;
begin
  Result := varEmpty;
end;

function TIFBlock.DrawItem(Handle: Integer; Left: Integer; Top: Integer;
  Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
  BkColor: OLE_COLOR): WordBool;
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := False;
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    aRect := Rect(Left, Top, Right, Bottom);

    if Selected then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(aRect);
    end
    else
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(aRect);
    end;

    Offset := 2;

    Canvas.Font.Style := [fsBold];
    SetCanvasTextColor(clWindowText);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, Caption);
    // Offset := Offset + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

procedure TIFBlock.SetFilename(const Filename: WideString);
begin
end;

function TIFBlock.Get_Caption: WideString;
begin
  Result := FCaption + ' ' + Varname + ' ' + GetCondition1Text(Condition) + ' '
    + VarValue;
end;

function TIFBlock.Get_OwnerDraw: WordBool;
begin
  Result := True;
end;

procedure TIFBlock.Set_Caption(const Value: WideString);
begin
  //Wird immer automatisch generiert
end;

function TIFBlock.Get_ParamValues(const ParamName: WideString): WideString;
begin
  if ParamName = stdcCondition then
    Result := IntToStr(Ord(FCondition))
  else if ParamName = stdcVarname then
    Result := FVarname
  else if ParamName = stdcValue then
    Result := FValue;
end;

function TIFBlock.Get_PreviewText: WideString;
begin
  Result := '';
end;

function TIFBlock.Get_Properties: IDispatch;
begin
  Result := nil;
end;

procedure TIFBlock.Set_ParamValues(const ParamName: WideString;
  const Value: WideString);
begin
  if ParamName = stdcCondition then
    FCondition := TBlockCondition1(StrToInt(Value))
  else if ParamName = stdcVarname then
    FVarname := Value
  else if ParamName = stdcValue then
    FValue := Value;
end;

function TIFBlock.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
  case Index of
    0:
      Result := stdcCondition;
    1:
      Result := stdcVarname;
    2:
      Result := stdcValue;
  end;
end;

function TIFBlock.Get_ParamCount: Integer;
begin
  Result := 3;
end;

function TIFBlockCallback.GetIdentifier: WideString;
begin
  Result := IDIFBlock;
end;

{ ELSE BLOCK ******************************************************** }
function TELSEBlockCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TELSEBlock.Create(nil));
end;

procedure TELSEBlockCallback.SetCanceled(ACanceled: WordBool);
begin
  // Canceled := ACanceled;
end;

constructor TELSEBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdELSECaption;
  FCondition := '';
end;

destructor TELSEBlock.Destroy;
begin
  inherited Destroy;
end;

function TELSEBlock.EditItem: WordBool;
begin
  Result := True;
end;

function TELSEBlock.ExecuteItem: WordBool;
begin
  Result := True;
  DoLogbookMessage('ELSE -> ');
end;

function TELSEBlock.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TELSEBlock.Notify(const Notification: WideString;
  Parameter: OleVariant): OleVariant;
begin
  Result := varEmpty;
end;

function TELSEBlock.DrawItem(Handle: Integer; Left: Integer; Top: Integer;
  Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
  BkColor: OLE_COLOR): WordBool;
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := False;
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    aRect := Rect(Left, Top, Right, Bottom);

    if Selected then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(aRect);
    end
    else
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(aRect);
    end;

    Offset := 2;

    Canvas.Font.Style := [fsBold];
    SetCanvasTextColor(clWindowText);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, FCaption);
    // Offset := Offset + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

procedure TELSEBlock.SetFilename(const Filename: WideString);
begin
end;

function TELSEBlock.Get_Caption: WideString;
begin
  Result := FCaption;
end;

function TELSEBlock.Get_OwnerDraw: WordBool;
begin
  Result := True;
end;

procedure TELSEBlock.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TELSEBlock.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
end;

function TELSEBlock.Get_PreviewText: WideString;
begin
  Result := '';
end;

function TELSEBlock.Get_Properties: IDispatch;
begin
  Result := nil;
end;

procedure TELSEBlock.Set_ParamValues(const ParamName: WideString;
  const Value: WideString);
begin

end;

function TELSEBlock.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
end;

function TELSEBlock.Get_ParamCount: Integer;
begin
  Result := 0;
end;

function TELSEBlockCallback.GetIdentifier: WideString;
begin
  Result := IDELSEBlock;
end;

{ END BLOCK ******************************************************** }
function TENDBlockCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TENDBlock.Create(nil));
end;

procedure TENDBlockCallback.SetCanceled(ACanceled: WordBool);
begin
  // Canceled := ACanceled;
end;

constructor TENDBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdENDCaption;
  FCondition := '';
end;

destructor TENDBlock.Destroy;
begin
  inherited Destroy;
end;

function TENDBlock.EditItem: WordBool;
begin
  Result := True;
end;

function TENDBlock.ExecuteItem: WordBool;
begin
  Result := True;
end;

function TENDBlock.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TENDBlock.Notify(const Notification: WideString; Parameter: OleVariant)
  : OleVariant;
begin
  Result := varEmpty;
end;

function TENDBlock.DrawItem(Handle: Integer; Left: Integer; Top: Integer;
  Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
  BkColor: OLE_COLOR): WordBool;
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := False;
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    aRect := Rect(Left, Top, Right, Bottom);

    if Selected then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(aRect);
    end
    else
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(aRect);
    end;

    Offset := 2;

    Canvas.Font.Style := [fsBold];
    SetCanvasTextColor(clWindowText);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, FCaption);
    // Offset := Offset + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

procedure TENDBlock.SetFilename(const Filename: WideString);
begin
end;

function TENDBlock.Get_Caption: WideString;
begin
  Result := FCaption;
end;

function TENDBlock.Get_OwnerDraw: WordBool;
begin
  Result := False;
end;

procedure TENDBlock.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TENDBlock.Get_ParamValues(const ParamName: WideString): WideString;
begin
  Result := '';
end;

function TENDBlock.Get_PreviewText: WideString;
begin
  Result := '';
end;

function TENDBlock.Get_Properties: IDispatch;
begin
  Result := nil;
end;

procedure TENDBlock.Set_ParamValues(const ParamName: WideString;
  const Value: WideString);
begin

end;

function TENDBlock.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
end;

function TENDBlock.Get_ParamCount: Integer;
begin
  Result := 0;
end;

function TENDBlockCallback.GetIdentifier: WideString;
begin
  Result := IDENDBlock;
end;

{ FOR BLOCK ******************************************************** }
function TFORBlockCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TFORBlock.Create(nil));
end;

procedure TFORBlockCallback.SetCanceled(ACanceled: WordBool);
begin
  // Canceled := ACanceled;
end;

constructor TFORBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdFORCaption;
  FStartValue := 0;
  FEndValue := 1;
  FVarname := 'i';
end;

destructor TFORBlock.Destroy;
begin
  inherited Destroy;
end;

procedure TFORBlock.ResetCounter;
begin
  VarhandlerRef.SetVarEx(Varname, FStartValue - 1);
end;

function TFORBlock.EditItem: WordBool;
begin
  Result := DlgEditFor(Self);
end;

function TFORBlock.ExecuteItem: WordBool;
var
  I: Integer;
begin
  I := VarhandlerRef.GetVar(Varname);
  Inc(I);
  Result := I <= EndValue;
  VarhandlerRef.SetVar(Varname, I);
end;

function TFORBlock.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TFORBlock.DrawItem(Handle: Integer; Left: Integer; Top: Integer;
  Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
  BkColor: OLE_COLOR): WordBool;
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := False;
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    aRect := Rect(Left, Top, Right, Bottom);

    if Selected then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(aRect);
    end
    else
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(aRect);
    end;

    Offset := 2;

    Canvas.Font.Style := [fsBold];
    SetCanvasTextColor(clWindowText);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, FCaption + ' ' + Varname
      + '=' + IntToStr(StartValue) + ' to ' + IntToStr(EndValue));
    // Offset := Offset + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

procedure TFORBlock.SetFilename(const Filename: WideString);
begin
end;

function TFORBlock.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TFORBlock.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TFORBlock.Get_ParamValues(const ParamName: WideString): WideString;
begin
  if ParamName = stdcVarname then
    Result := Varname
  else if ParamName = stdcStartValue then
    Result := IntToStr(StartValue)
  else if ParamName = stdcEndValue then
    Result := IntToStr(EndValue);
end;

procedure TFORBlock.Set_ParamValues(const ParamName: WideString;
  const Value: WideString);
begin
  if ParamName = stdcVarname then
    Varname := Value
  else if ParamName = stdcStartValue then
    StartValue := StrToInt(Value)
  else if ParamName = stdcEndValue then
    EndValue := StrToInt(Value);
end;

function TFORBlock.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
  case Index of
    0:
      Result := stdcVarname;
    1:
      Result := stdcStartValue;
    2:
      Result := stdcEndValue;
  end;
end;

function TFORBlock.Get_ParamCount: Integer;
begin
  Result := 3;
end;

function TFORBlockCallback.GetIdentifier: WideString;
begin
  Result := IDFORBlock;
end;

function TFORBlock.Notify(const Notification: WideString; Parameter: OleVariant)
  : OleVariant;
begin
  Result := False;
  if SameText('Reset', Notification) then
    ResetCounter;
end;

function TFORBlock.Get_PreviewText: WideString;
begin
  Result := '';
end;

function TFORBlock.Get_OwnerDraw: WordBool;
begin
  Result := True;
end;

{ WHILE BLOCK ******************************************************** }
function TWHILEBlockCallback.CreateCommand: IDispatch;
begin
  Result := ICommand(TWHILEBlock.Create(nil));
end;

procedure TWHILEBlockCallback.SetCanceled(ACanceled: WordBool);
begin
  // Canceled := ACanceled;
end;

constructor TWHILEBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := stdWHILECaption;
  FCondition := bcEqual;
  FVarname := '';
  FValue := '';
end;

destructor TWHILEBlock.Destroy;
begin
  inherited Destroy;
end;

function TWHILEBlock.EditItem: WordBool;
begin
  Result := True;
  Result := DlgEditWhile(Self);
end;

function TWHILEBlock.ExecuteItem: WordBool;
begin
  Result := False;

  if not VarhandlerRef.VarExists(Varname) then
    VarhandlerRef.SetVarEx(Varname, '');

  try
    case Condition of
      bcEqual:
        Result := VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrEqual;
      bcGreater:
        Result := VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrGreaterThan;
      bcLess:
        Result := VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrLessThan;
      bcGreaterEqual:
        Result := (VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrEqual) or (VarCompareValue(VarhandlerRef.GetVar(Varname),
          VarValue) = vrGreaterThan);
      bcLessEqual:
        Result := (VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrEqual) or (VarCompareValue(VarhandlerRef.GetVar(Varname),
          VarValue) = vrLessThan);
      bcNotEqual:
        Result := VarCompareValue(VarhandlerRef.GetVar(Varname), VarValue)
          = vrNotEqual;
      bcContains:
        Result := Pos(VarValue, VarhandlerRef.GetVar(Varname)) > 0;
      bcNotContains:
        Result := Pos(VarValue, VarhandlerRef.GetVar(Varname)) = 0;
      bcFileExists:
        Result := FileExists(VarhandlerRef.GetVar(Varname));
      bcPathExists:
        Result := DirectoryExists(VarhandlerRef.GetVar(Varname));
    end;

    DoLogbookMessage(FCaption + ' ' + Varname + ' ' +
      GetCondition1Text(Condition) + ' ' + VarValue);
    DoLogbookMessage('->' + BoolToStr(Result, True));
  except
    on E: Exception do
    begin
      DoLogbookMessage('ERROR on WHILE...');
      DoLogbookMessage(E.Message);
      Result := False;
    end;
  end;
end;

function TWHILEBlock.MeasureItem(Handle: Integer; BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TWHILEBlock.DrawItem(Handle: Integer; Left: Integer; Top: Integer;
  Right: Integer; Bottom: Integer; Selected: WordBool; BriefView: WordBool;
  BkColor: OLE_COLOR): WordBool;
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := False;
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    aRect := Rect(Left, Top, Right, Bottom);

    if Selected then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(aRect);
    end
    else
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(aRect);
    end;

    Offset := 2;

    Canvas.Font.Style := [fsBold];
    SetCanvasTextColor(clWindowText);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, FCaption + ' ' + Varname
      + ' ' + GetCondition1Text(Condition) + ' ' + VarValue);
    // Offset := Offset + Canvas.TextHeight(FCaption) + 2;
  finally
    Canvas.Free;
  end;
end;

procedure TWHILEBlock.SetFilename(const Filename: WideString);
begin
end;

function TWHILEBlock.Get_Caption: WideString;
begin
  Result := FCaption;
end;

procedure TWHILEBlock.Set_Caption(const Value: WideString);
begin
  FCaption := Value;
end;

function TWHILEBlock.Get_ParamValues(const ParamName: WideString): WideString;
begin
  if ParamName = stdcCondition then
    Result := IntToStr(Ord(FCondition))
  else if ParamName = stdcVarname then
    Result := FVarname
  else if ParamName = stdcValue then
    Result := FValue;
end;

procedure TWHILEBlock.Set_ParamValues(const ParamName: WideString;
  const Value: WideString);
begin
  if ParamName = stdcCondition then
    FCondition := TBlockCondition1(StrToInt(Value))
  else if ParamName = stdcVarname then
    FVarname := Value
  else if ParamName = stdcValue then
    FValue := Value;
end;

function TWHILEBlock.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
  case Index of
    0:
      Result := stdcCondition;
    1:
      Result := stdcVarname;
    2:
      Result := stdcValue;
  end;
end;

function TWHILEBlock.Get_ParamCount: Integer;
begin
  Result := 3;
end;

function TWHILEBlockCallback.GetIdentifier: WideString;
begin
  Result := IDWHILEBlock;
end;

function TFORBlock.Get_Properties: IDispatch;
begin
  Result := nil;
end;

{ ************** INCLUDE ************************************************** }

{ TIncludeCallback }

function TINCLUDEBlockCallback.GetIdentifier: WideString;
begin
  Result := IDINCLUDEBlock;
end;

procedure TINCLUDEBlockCallback.SetCanceled(ACanceled: WordBool);
begin
  //
end;

function TINCLUDEBlockCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TINCLUDEBlock.Create(nil));
end;

{ TInclude }

constructor TINCLUDEBlock.Create(AOwner: TComponent);
begin
  inherited;
  FFilename := '';
end;

function TINCLUDEBlock.Get_ParamValues(const ParamName: WideString): WideString;
begin
  if ParamName = stdcFilename then
    Result := Filename
  else
    Result := '';
end;

procedure TINCLUDEBlock.SetFilename(const Filename: WideString);
begin
  if SameText(ExtractFileExt(Filename), ProgramDefaultExt) then
    FFilename := Filename;
end;

procedure TINCLUDEBlock.Set_Caption(const Value: WideString);
begin
end;

function TINCLUDEBlock.MeasureItem(Handle: Integer;
  BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsBold];
    Result := Result + Canvas.TextHeight(Caption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TINCLUDEBlock.EditItem: WordBool;
begin
  Result := DlgEditInclude(Self);
end;

function TINCLUDEBlock.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
  case Index of
    0:
      Result := stdcFilename;
  end;

end;

procedure TINCLUDEBlock.Set_ParamValues(const ParamName, Value: WideString);
begin
  if ParamName = stdcFilename then
    Filename := Value;

end;

function TINCLUDEBlock.ExecuteItem: WordBool;
var
  f: String;
begin
  f := VarhandlerRef.ReplaceVarsInString(Filename);
  DoLogbookMessage(Format(stdDoInclude, [f]));
  Result := FileExists(f);

  if FileExists(f) then
  begin
  end;
end;

function TINCLUDEBlock.Get_Caption: WideString;
begin
  Result := stdINCLUDECaption + ' - <' + Filename + '>';
end;

destructor TINCLUDEBlock.Destroy;
begin

  inherited;
end;

function TINCLUDEBlock.DrawItem(Handle, Left, Top, Right, Bottom: Integer;
  Selected, BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
begin

end;

function TINCLUDEBlock.Get_ParamCount: Integer;
begin
  Result := 1;
end;

function TINCLUDEBlock.Notify(const Notification: WideString;
  Parameter: OleVariant): OleVariant;
begin
  Result := varNull;
end;

function TINCLUDEBlock.Get_PreviewText: WideString;
begin
  Result := '';
end;

function TINCLUDEBlock.Get_OwnerDraw: WordBool;
begin
  Result := False;
end;

function TINCLUDEBlock.Get_Properties: IDispatch;
begin
  Result := nil;
end;

{ TCommandTypeItemList }

function TCommandTypeItemList.GetItem(Index: Integer): TCommandTypeItem;
begin
  Result := TCommandTypeItem(inherited Items[Index]);
end;

procedure TCommandTypeItemList.SetItem(Index: Integer;
  const Value: TCommandTypeItem);
begin
  inherited Items[Index] := Value;
end;

{ TCOMMENTBlock }

constructor TCOMMENTBlock.Create(AOwner: TComponent);
begin
  inherited;
  FComment := '';
end;

function TCOMMENTBlock.DrawItem(Handle, Left, Top, Right, Bottom: Integer;
  Selected, BriefView: WordBool; BkColor: OLE_COLOR): WordBool;
var
  Offset: Integer;
  Canvas: TCanvas;
  aRect: TRect;

  procedure SetCanvasTextColor(Col: TColor);
  begin
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := Col;
  end;

begin
  Result := False;
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    aRect := Rect(Left, Top, Right, Bottom);

    if Selected then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.FillRect(aRect);
    end
    else
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(aRect);
    end;

    Offset := 2;

    Canvas.Font.Style := [fsItalic];
    SetCanvasTextColor(clGrayText);
    Canvas.TextOut(aRect.Left + 2, aRect.Top + Offset, Caption);
  finally
    Canvas.Free;
  end;
end;

function TCOMMENTBlock.EditItem: WordBool;
begin
  Result := DlgEditComment(Self);
end;

function TCOMMENTBlock.ExecuteItem: WordBool;
begin
  //
end;

function TCOMMENTBlock.Get_Caption: WideString;
begin
  Result := FComment;
end;

function TCOMMENTBlock.Get_OwnerDraw: WordBool;
begin
  Result := True;
end;

function TCOMMENTBlock.Get_ParamCount: Integer;
begin
  Result := 1;
end;

function TCOMMENTBlock.Get_ParamNames(Index: Integer): WideString;
begin
  Result := '';
  case Index of
    0:
      Result := stdcValue;
  end;
end;

function TCOMMENTBlock.Get_ParamValues(const ParamName: WideString): WideString;
begin
  if ParamName = stdcValue then
    Result := FComment
  else
    Result := '';
end;

function TCOMMENTBlock.Get_PreviewText: WideString;
begin
  Result := '';
end;

function TCOMMENTBlock.Get_Properties: IDispatch;
begin
  Result := nil;
end;

function TCOMMENTBlock.MeasureItem(Handle: Integer;
  BriefView: WordBool): Integer;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Result := 3;
    Canvas.Font.Style := [fsItalic];
    Result := Result + Canvas.TextHeight(Caption) + 2;
  finally
    Canvas.Free;
  end;
end;

function TCOMMENTBlock.Notify(const Notification: WideString;
  Parameter: OleVariant): OleVariant;
begin
  Result := varEmpty;
end;

procedure TCOMMENTBlock.SetComment(const Value: String);
begin
  FComment := Value;
end;

procedure TCOMMENTBlock.SetFilename(const Filename: WideString);
begin
  //
end;

procedure TCOMMENTBlock.Set_Caption(const Value: WideString);
begin
  //
end;

procedure TCOMMENTBlock.Set_ParamValues(const ParamName, Value: WideString);
begin
  if ParamName = stdcValue then
    FComment := Value;
end;

{ TCOMMENTBlockCallback }

function TCOMMENTBlockCallback.CreateCommand: IDispatch;
begin
  Result := ICommand2(TCOMMENTBlock.Create(nil));
end;

function TCOMMENTBlockCallback.GetIdentifier: WideString;
begin
  Result := IDCOMMENT;
end;

procedure TCOMMENTBlockCallback.SetCanceled(ACanceled: WordBool);
begin
  //
end;

initialization

finalization

if _CommandTypes <> nil then
  _CommandTypes.Free;

end.
