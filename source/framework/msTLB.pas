unit msTLB;

// ************************************************************************ //
// WARNUNG
// -------
// Die in dieser Datei deklarierten Typen wurden aus Daten einer Typbibliothek
// generiert. Wenn diese Typbibliothek explizit oder indirekt (über eine
// andere Typbibliothek) reimportiert wird oder wenn der Befehl
// 'Aktualisieren' im Typbibliotheks-Editor während des Bearbeitens der
// Typbibliothek aktiviert ist, wird der Inhalt dieser Datei neu generiert und
// alle manuell vorgenommenen Änderungen gehen verloren.
// ************************************************************************ //

// $Rev: 34747 $
// Datei am 28.12.2011 15:37:15 erzeugt aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib.: V:\projekte\burkhard\jmake\MakeStudio (1)
// LIBID: {09828B26-2D82-4B16-90D1-517D298B3612}
// LCID: 0
// Hilfedatei:
// Hilfe-String: MakeStudio Bibliothek
// Liste der Abhäng.:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit muss ohne Typüberprüfung für Zeiger compiliert werden.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;

// *********************************************************************//
// In der Typbibliothek deklarierte GUIDS. Die folgenden Präfixe werden verwendet:
//   Typbibliotheken      : LIBID_xxxx
//   CoClasses            : CLASS_xxxx
//   DISPInterfaces       : DIID_xxxx
//   Nicht-DISP-Interfaces: IID_xxxx
// *********************************************************************//
const
  // Haupt- und Nebenversionen der Typbibliothek
  MakeStudioMajorVersion = 1;
  MakeStudioMinorVersion = 0;

  LIBID_MakeStudio: TGUID = '{09828B26-2D82-4B16-90D1-517D298B3612}';

  IID_IJApplication: TGUID = '{2B09765A-9813-4C0C-B5A2-B8D250F7D006}';
  CLASS_JApplication: TGUID = '{7F8F8634-63D3-460A-BA05-4CED8E2A4CAD}';
  IID_IActionCallback: TGUID = '{D1697D20-4E2F-4B3C-B39C-C6B96C78D55B}';
  IID_ICommand: TGUID = '{59CD4BBC-FC46-4361-9C3A-A22A02FAF63E}';
  IID_ICommandCallback: TGUID = '{B9BFA24F-B70B-4FA2-AF6E-8BB796A0AE3E}';
  IID_IVars: TGUID = '{08E22200-8DFB-4F72-A339-DB2DDB258FE8}';
  IID_IExecCallback: TGUID = '{9722F0A3-D7EC-4DD6-880C-3C2DDE25259D}';
  IID_IPlugin: TGUID = '{584C09E1-6443-4181-87E3-2ED1248A7217}';
  IID_IJApplication2: TGUID = '{0073C47F-0B26-4D9B-9035-53ECF1C2E70F}';
  IID_ICommand2: TGUID = '{BA02E57D-A446-44AE-827B-35E275C80271}';

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten Aufzählungen
// *********************************************************************//
// Konstanten für enum varBaseType
type
  varBaseType = TOleEnum;
const
  varBaseString = $00000000;
  varBaseBool = $00000001;
  varBaseInteger = $00000002;
  varBaseFloat = $00000003;
  varBaseIDispatch = $00000004;

// Konstanten für enum EMakeKind
type
  EMakeKind = TOleEnum;
const
  mkGUI = $00000000;
  mkCommandLine = $00000001;
  mkServer = $00000002;

// Konstanten für enum EResultType
type
  EResultType = TOleEnum;
const
  jerOK = $00000000;
  jerWarning = $00000001;
  jerError = $00000002;

type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen
// *********************************************************************//
  IJApplication = interface;
  IJApplicationDisp = dispinterface;
  IActionCallback = interface;
  IActionCallbackDisp = dispinterface;
  ICommand = interface;
  ICommandDisp = dispinterface;
  ICommandCallback = interface;
  ICommandCallbackDisp = dispinterface;
  IVars = interface;
  IVarsDisp = dispinterface;
  IExecCallback = interface;
  IExecCallbackDisp = dispinterface;
  IPlugin = interface;
  IPluginDisp = dispinterface;
  IJApplication2 = interface;
  IJApplication2Disp = dispinterface;
  ICommand2 = interface;
  ICommand2Disp = dispinterface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses
// (HINWEIS: Hier wird jede CoClass ihrem Standard-Interface zugewiesen)
// *********************************************************************//
  JApplication = IJApplication;


// *********************************************************************//
// Interface: IJApplication
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2B09765A-9813-4C0C-B5A2-B8D250F7D006}
// *********************************************************************//
  IJApplication = interface(IDispatch)
    ['{2B09765A-9813-4C0C-B5A2-B8D250F7D006}']
    function LoadFromFile(const Filename: WideString): WordBool; safecall;
    procedure Run; safecall;
    procedure AddCommandType(const CommandName: WideString; const CommandHint: WideString;
                             const CommandCategory: WideString; const Bitmap: IPictureDisp;
                             const DragDropFileExtensions: WideString; CompatibilityIndex: Integer;
                             const Callback: IDispatch); safecall;
    procedure AddMenuAction(const ActionName: WideString; const Caption: WideString;
                            const Hint: WideString; const Bitmap: IPictureDisp;
                            const Callback: IDispatch); safecall;
    procedure LogMessage(const Value: WideString); safecall;
    procedure AddAdditionalInfo(const Value: WideString); safecall;
    procedure AddCreditInfo(const Value: WideString); safecall;
    function Get_ApplicationRegKey: WideString; safecall;
    function Get_ApplicationDataFolder: WideString; safecall;
    procedure AddCommandCategory(const aCaption: WideString; const aPicture: IPictureDisp); safecall;
    function Get_Variables: IVars; safecall;
    function ExecCmdLine(const App: WideString; const Args: WideString; const Dir: WideString;
                         const Callback: IExecCallback): Integer; safecall;
    procedure ShowHelp(const Topic: WideString); safecall;
    procedure SetStatus(const Text: WideString); safecall;
    procedure AddCommandByFile(const Filename: WideString); safecall;
    procedure AddCommand(const CommandID: WideString); safecall;
    property ApplicationRegKey: WideString read Get_ApplicationRegKey;
    property ApplicationDataFolder: WideString read Get_ApplicationDataFolder;
    property Variables: IVars read Get_Variables;
  end;

// *********************************************************************//
// DispIntf:  IJApplicationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2B09765A-9813-4C0C-B5A2-B8D250F7D006}
// *********************************************************************//
  IJApplicationDisp = dispinterface
    ['{2B09765A-9813-4C0C-B5A2-B8D250F7D006}']
    function LoadFromFile(const Filename: WideString): WordBool; dispid 201;
    procedure Run; dispid 202;
    procedure AddCommandType(const CommandName: WideString; const CommandHint: WideString;
                             const CommandCategory: WideString; const Bitmap: IPictureDisp;
                             const DragDropFileExtensions: WideString; CompatibilityIndex: Integer;
                             const Callback: IDispatch); dispid 212;
    procedure AddMenuAction(const ActionName: WideString; const Caption: WideString;
                            const Hint: WideString; const Bitmap: IPictureDisp;
                            const Callback: IDispatch); dispid 207;
    procedure LogMessage(const Value: WideString); dispid 204;
    procedure AddAdditionalInfo(const Value: WideString); dispid 205;
    procedure AddCreditInfo(const Value: WideString); dispid 206;
    property ApplicationRegKey: WideString readonly dispid 208;
    property ApplicationDataFolder: WideString readonly dispid 209;
    procedure AddCommandCategory(const aCaption: WideString; const aPicture: IPictureDisp); dispid 210;
    property Variables: IVars readonly dispid 211;
    function ExecCmdLine(const App: WideString; const Args: WideString; const Dir: WideString;
                         const Callback: IExecCallback): Integer; dispid 203;
    procedure ShowHelp(const Topic: WideString); dispid 213;
    procedure SetStatus(const Text: WideString); dispid 214;
    procedure AddCommandByFile(const Filename: WideString); dispid 215;
    procedure AddCommand(const CommandID: WideString); dispid 216;
  end;

// *********************************************************************//
// Interface: IActionCallback
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D1697D20-4E2F-4B3C-B39C-C6B96C78D55B}
// *********************************************************************//
  IActionCallback = interface(IDispatch)
    ['{D1697D20-4E2F-4B3C-B39C-C6B96C78D55B}']
    procedure Execute(const Action: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IActionCallbackDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D1697D20-4E2F-4B3C-B39C-C6B96C78D55B}
// *********************************************************************//
  IActionCallbackDisp = dispinterface
    ['{D1697D20-4E2F-4B3C-B39C-C6B96C78D55B}']
    procedure Execute(const Action: WideString); dispid 201;
  end;

// *********************************************************************//
// Interface: ICommand
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {59CD4BBC-FC46-4361-9C3A-A22A02FAF63E}
// *********************************************************************//
  ICommand = interface(IDispatch)
    ['{59CD4BBC-FC46-4361-9C3A-A22A02FAF63E}']
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

// *********************************************************************//
// DispIntf:  ICommandDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {59CD4BBC-FC46-4361-9C3A-A22A02FAF63E}
// *********************************************************************//
  ICommandDisp = dispinterface
    ['{59CD4BBC-FC46-4361-9C3A-A22A02FAF63E}']
    function MeasureItem(Handle: Integer; BriefView: WordBool): Integer; dispid 201;
    function EditItem: WordBool; dispid 202;
    function ExecuteItem: WordBool; dispid 205;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
                      Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool; dispid 206;
    procedure SetFilename(const Filename: WideString); dispid 207;
    property Caption: WideString dispid 208;
    property ParamValues[const ParamName: WideString]: WideString dispid 209;
    property ParamNames[Index: Integer]: WideString readonly dispid 210;
    property ParamCount: Integer readonly dispid 211;
  end;

// *********************************************************************//
// Interface: ICommandCallback
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B9BFA24F-B70B-4FA2-AF6E-8BB796A0AE3E}
// *********************************************************************//
  ICommandCallback = interface(IDispatch)
    ['{B9BFA24F-B70B-4FA2-AF6E-8BB796A0AE3E}']
    function CreateCommand: IDispatch; safecall;
    procedure SetCanceled(aCanceled: WordBool); safecall;
    function GetIdentifier: WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  ICommandCallbackDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B9BFA24F-B70B-4FA2-AF6E-8BB796A0AE3E}
// *********************************************************************//
  ICommandCallbackDisp = dispinterface
    ['{B9BFA24F-B70B-4FA2-AF6E-8BB796A0AE3E}']
    function CreateCommand: IDispatch; dispid 201;
    procedure SetCanceled(aCanceled: WordBool); dispid 202;
    function GetIdentifier: WideString; dispid 203;
  end;

// *********************************************************************//
// Interface: IVars
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {08E22200-8DFB-4F72-A339-DB2DDB258FE8}
// *********************************************************************//
  IVars = interface(IDispatch)
    ['{08E22200-8DFB-4F72-A339-DB2DDB258FE8}']
    function Get_Count: Integer; safecall;
    function Get_Values(const Varname: WideString): OleVariant; safecall;
    procedure Set_Values(const Varname: WideString; Value: OleVariant); safecall;
    function Get_ValuesByIdx(Index: Integer): OleVariant; safecall;
    procedure Set_ValuesByIdx(Index: Integer; Value: OleVariant); safecall;
    function Get_Names(Index: Integer): WideString; safecall;
    procedure AddVar(const Varname: WideString); safecall;
    procedure DeleteVar(const Varname: WideString); safecall;
    function IdxOfVar(const Varname: WideString): Integer; safecall;
    function VarExists(const Varname: WideString): WordBool; safecall;
    function BaseDataType(const Varname: WideString): varBaseType; safecall;
    function ReplaceVarsInString(const Value: WideString): WideString; safecall;
    property Count: Integer read Get_Count;
    property Values[const Varname: WideString]: OleVariant read Get_Values write Set_Values;
    property ValuesByIdx[Index: Integer]: OleVariant read Get_ValuesByIdx write Set_ValuesByIdx;
    property Names[Index: Integer]: WideString read Get_Names;
  end;

// *********************************************************************//
// DispIntf:  IVarsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {08E22200-8DFB-4F72-A339-DB2DDB258FE8}
// *********************************************************************//
  IVarsDisp = dispinterface
    ['{08E22200-8DFB-4F72-A339-DB2DDB258FE8}']
    property Count: Integer readonly dispid 201;
    property Values[const Varname: WideString]: OleVariant dispid 202;
    property ValuesByIdx[Index: Integer]: OleVariant dispid 203;
    property Names[Index: Integer]: WideString readonly dispid 204;
    procedure AddVar(const Varname: WideString); dispid 205;
    procedure DeleteVar(const Varname: WideString); dispid 206;
    function IdxOfVar(const Varname: WideString): Integer; dispid 207;
    function VarExists(const Varname: WideString): WordBool; dispid 208;
    function BaseDataType(const Varname: WideString): varBaseType; dispid 209;
    function ReplaceVarsInString(const Value: WideString): WideString; dispid 210;
  end;

// *********************************************************************//
// Interface: IExecCallback
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9722F0A3-D7EC-4DD6-880C-3C2DDE25259D}
// *********************************************************************//
  IExecCallback = interface(IDispatch)
    ['{9722F0A3-D7EC-4DD6-880C-3C2DDE25259D}']
    procedure CaptureOutput(const Line: WideString; var Aborted: WordBool); safecall;
  end;

// *********************************************************************//
// DispIntf:  IExecCallbackDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9722F0A3-D7EC-4DD6-880C-3C2DDE25259D}
// *********************************************************************//
  IExecCallbackDisp = dispinterface
    ['{9722F0A3-D7EC-4DD6-880C-3C2DDE25259D}']
    procedure CaptureOutput(const Line: WideString; var Aborted: WordBool); dispid 201;
  end;

// *********************************************************************//
// Interface: IPlugin
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {584C09E1-6443-4181-87E3-2ED1248A7217}
// *********************************************************************//
  IPlugin = interface(IDispatch)
    ['{584C09E1-6443-4181-87E3-2ED1248A7217}']
    function RegisterPlugin(const AMakeStudioApp: IJApplication): Integer; safecall;
    function Get_Name: WideString; safecall;
    function Get_Author: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_RequiredPlugins: WideString; safecall;
    function UnregisterPlugin: Integer; safecall;
    function Get_MinorVersion: Integer; safecall;
    function Get_MajorVersion: Integer; safecall;
    function Get_OptionsPageGUID: TGUID; safecall;
    procedure AfterAllPluginsLoaded; safecall;
    property Name: WideString read Get_Name;
    property Author: WideString read Get_Author;
    property Description: WideString read Get_Description;
    property RequiredPlugins: WideString read Get_RequiredPlugins;
    property MinorVersion: Integer read Get_MinorVersion;
    property MajorVersion: Integer read Get_MajorVersion;
    property OptionsPageGUID: TGUID read Get_OptionsPageGUID;
  end;

// *********************************************************************//
// DispIntf:  IPluginDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {584C09E1-6443-4181-87E3-2ED1248A7217}
// *********************************************************************//
  IPluginDisp = dispinterface
    ['{584C09E1-6443-4181-87E3-2ED1248A7217}']
    function RegisterPlugin(const AMakeStudioApp: IJApplication): Integer; dispid 5;
    property Name: WideString readonly dispid 1;
    property Author: WideString readonly dispid 2;
    property Description: WideString readonly dispid 3;
    property RequiredPlugins: WideString readonly dispid 4;
    function UnregisterPlugin: Integer; dispid 6;
    property MinorVersion: Integer readonly dispid 7;
    property MajorVersion: Integer readonly dispid 8;
    property OptionsPageGUID: {??TGUID}OleVariant readonly dispid 9;
    procedure AfterAllPluginsLoaded; dispid 10;
  end;

// *********************************************************************//
// Interface: IJApplication2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0073C47F-0B26-4D9B-9035-53ECF1C2E70F}
// *********************************************************************//
  IJApplication2 = interface(IJApplication)
    ['{0073C47F-0B26-4D9B-9035-53ECF1C2E70F}']
    function Get_MakeKind: EMakeKind; safecall;
    function Get_ApplicationHandle: Integer; safecall;
    function Get_ApplicationLanguage: WideString; safecall;
    property MakeKind: EMakeKind read Get_MakeKind;
    property ApplicationHandle: Integer read Get_ApplicationHandle;
    property ApplicationLanguage: WideString read Get_ApplicationLanguage;
  end;

// *********************************************************************//
// DispIntf:  IJApplication2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0073C47F-0B26-4D9B-9035-53ECF1C2E70F}
// *********************************************************************//
  IJApplication2Disp = dispinterface
    ['{0073C47F-0B26-4D9B-9035-53ECF1C2E70F}']
    property MakeKind: EMakeKind readonly dispid 301;
    property ApplicationHandle: Integer readonly dispid 302;
    property ApplicationLanguage: WideString readonly dispid 303;
    function LoadFromFile(const Filename: WideString): WordBool; dispid 201;
    procedure Run; dispid 202;
    procedure AddCommandType(const CommandName: WideString; const CommandHint: WideString;
                             const CommandCategory: WideString; const Bitmap: IPictureDisp;
                             const DragDropFileExtensions: WideString; CompatibilityIndex: Integer;
                             const Callback: IDispatch); dispid 212;
    procedure AddMenuAction(const ActionName: WideString; const Caption: WideString;
                            const Hint: WideString; const Bitmap: IPictureDisp;
                            const Callback: IDispatch); dispid 207;
    procedure LogMessage(const Value: WideString); dispid 204;
    procedure AddAdditionalInfo(const Value: WideString); dispid 205;
    procedure AddCreditInfo(const Value: WideString); dispid 206;
    property ApplicationRegKey: WideString readonly dispid 208;
    property ApplicationDataFolder: WideString readonly dispid 209;
    procedure AddCommandCategory(const aCaption: WideString; const aPicture: IPictureDisp); dispid 210;
    property Variables: IVars readonly dispid 211;
    function ExecCmdLine(const App: WideString; const Args: WideString; const Dir: WideString;
                         const Callback: IExecCallback): Integer; dispid 203;
    procedure ShowHelp(const Topic: WideString); dispid 213;
    procedure SetStatus(const Text: WideString); dispid 214;
    procedure AddCommandByFile(const Filename: WideString); dispid 215;
    procedure AddCommand(const CommandID: WideString); dispid 216;
  end;

// *********************************************************************//
// Interface: ICommand2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BA02E57D-A446-44AE-827B-35E275C80271}
// *********************************************************************//
  ICommand2 = interface(ICommand)
    ['{BA02E57D-A446-44AE-827B-35E275C80271}']
    function Get_OwnerDraw: WordBool; safecall;
    function Get_PreviewText: WideString; safecall;
    function Notify(const Notification: WideString; Parameter: OleVariant): OleVariant; safecall;
    function Get_Properties: IDispatch; safecall;
    property OwnerDraw: WordBool read Get_OwnerDraw;
    property PreviewText: WideString read Get_PreviewText;
    property Properties: IDispatch read Get_Properties;
  end;

// *********************************************************************//
// DispIntf:  ICommand2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BA02E57D-A446-44AE-827B-35E275C80271}
// *********************************************************************//
  ICommand2Disp = dispinterface
    ['{BA02E57D-A446-44AE-827B-35E275C80271}']
    property OwnerDraw: WordBool readonly dispid 301;
    property PreviewText: WideString readonly dispid 302;
    function Notify(const Notification: WideString; Parameter: OleVariant): OleVariant; dispid 303;
    property Properties: IDispatch readonly dispid 304;
    function MeasureItem(Handle: Integer; BriefView: WordBool): Integer; dispid 201;
    function EditItem: WordBool; dispid 202;
    function ExecuteItem: WordBool; dispid 205;
    function DrawItem(Handle: Integer; Left: Integer; Top: Integer; Right: Integer;
                      Bottom: Integer; Selected: WordBool; BriefView: WordBool; BkColor: OLE_COLOR): WordBool; dispid 206;
    procedure SetFilename(const Filename: WideString); dispid 207;
    property Caption: WideString dispid 208;
    property ParamValues[const ParamName: WideString]: WideString dispid 209;
    property ParamNames[Index: Integer]: WideString readonly dispid 210;
    property ParamCount: Integer readonly dispid 211;
  end;

// *********************************************************************//
// Die Klasse CoJApplication stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IJApplication, dargestellt
// von CoClass JApplication, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoJApplication = class
    class function Create: IJApplication;
    class function CreateRemote(const MachineName: string): IJApplication;
  end;

implementation

uses ComObj;

class function CoJApplication.Create: IJApplication;
begin
  Result := CreateComObject(CLASS_JApplication) as IJApplication;
end;

class function CoJApplication.CreateRemote(const MachineName: string): IJApplication;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JApplication) as IJApplication;
end;

end.

