(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcs.dpr

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/05  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in
2006/04/29  BSchranz  - jvcs Labels added

-----------------------------------------------------------------------------*)

library jvcs_Plugin;

{$I jedi.inc}

uses
  SysUtils,
  Classes,
  msTLB in '..\..\framework\msTLB.pas',
  ComServ,
  ActiveX,
  jvcs_Vars in 'jvcs_Vars.pas',
  jvcs_Actions in 'jvcs_Actions.pas' {Form3},
  jvcs_Actiontest in 'jvcs_Actiontest.pas' {Form2},
  jvcs_CheckInOut in 'jvcs_CheckInOut.pas',
  jvcs_DlgSelectJVCSProject in 'jvcs_DlgSelectJVCSProject.pas' {FormSelectJVCSProject},
  jvcs_EditJVCSInOutModule in 'jvcs_EditJVCSInOutModule.pas' {FormJVCSInOutEdit},
  jvcs_Utils in 'jvcs_Utils.pas',
  jvcs_Module in 'jvcs_Module.pas',
  jvcs_EditJVCSSyncModule in 'jvcs_EditJVCSSyncModule.pas' {FormJVCSSyncEdit},
  jvcs_DlgSelectJVCSModule in 'jvcs_DlgSelectJVCSModule.pas' {FormSelectJVCSModule};

{$E jpl}
{$R *.res}

procedure AfterAllPluginsLoaded;
begin
end;

//:Indentifies this DLL-Version
procedure JVCSMAKPlugin; stdcall;
begin
end;

//:Get name of Plugin
procedure GetName(AName: PChar); stdcall;
begin
  StrCopy(AName, PChar(struPluginName));
end;

//:Get author of Plugin
procedure GetAuthor(AName: PChar); stdcall;
begin
  StrCopy(AName, PChar(struPluginAuthor));
end;

//:Get description of Plugin
procedure GetDescription(AName: PChar); stdcall;
begin
  StrCopy(AName, PChar(struPluginHint));
end;

//:List of Required plugins separeted by ";"
procedure GetRequiredPlugins(AName: PChar); stdcall;
begin
  StrCopy(AName, '');
end;

//:Register an initialize Plugin
function RegisterPlugin(aJVCSMAKApp: IJApplication): Integer; stdcall;
var
  P: Picture;
begin
  Result := 0;
  jvcsmak := aJVCSMAKApp;
  with jvcsmak do begin
    try
      //Create form with actions and ModuleCallback
      Form3 := TForm3.Create(nil);


      //add actions
      {GetPictureFromImageList(Form3.ImageList1, Form3.acTestaction1.ImageIndex, P);
      jvcsmak.AddMenuAction(Form3.acTestaction1.Name,
                             Form3.acTestaction1.Caption,
                             Form3.acTestaction1.Hint,
                             P,
                             IActionCallback(Form3));}

      //add modules
      {compatibility
TDMakModuleType = (dmmtNone=0, dmmtNormalFile(Delphi32)=1, dmmtJVCS=2,
dmmtJVCSSync=3, dmmtPassolo=4, dmmtWise=5, dmmtMkdir=6);
      }
      GetPictureFromImageList(Form3.ImageList1, 0, P);
      //Name=testmodule;GUID=Implementation of Module, Image comes from Form3,
      //Extension=txt (could be more than one extension - spepareted by ;)
      //compatibility - 3
      //Callback for the Moduletype
      JVCSSyncCommandCallback := TJVCSSyncCommandCallback.Create(nil);
      jvcsmak.AddCommandType(stdJVCSProjectOpCaption, '', stdCategory, P, '', 3,
        ICommandCallback(JVCSSyncCommandCallback));

      GetPictureFromImageList(Form3.ImageList1, 2, P);
      JVCSInOutCommandCallback := TJVCSInOutCommandCallback.Create(nil);
      jvcsmak.AddCommandType(stdJVCSInOutCaption, '', stdCategory, P, '', 3,
        ICommandCallback(JVCSInOutCommandCallback));

      //Credits
      jvcsmak.AddCreditInfo(struPluginName + ' by ' + struPluginAuthor);
      jvcsmak.AddCreditInfo('Credits to:');
      jvcsmak.AddCreditInfo('');
      jvcsmak.AddCreditInfo('FreeVCS / JVCS');
      jvcsmak.AddCreditInfo('http://www.freevcs.de/');
      jvcsmak.AddCreditInfo('http://jedivcs.sourceforge.net/');
      jvcsmak.AddCreditInfo('');

      //Additional Info
      jvcsmak.AddAdditionalInfo(JVCSDLLVersionStr);

      //initialize identity list
      ReadIdentities;

    except
    end;
  end;
end;

//:UnRegister an finalize Plugin
function UnregisterPlugin: Integer; stdcall;
begin
  Result := 0;
  try
    Form3.Free;
    JVCSSyncCommandCallback.Free;
  except
  end;
end;

//:Version of plugin
function GetMinorVersion: Integer; stdcall;
begin
  Result := 1;
end;

//:Version of plugin
function GetMajorVersion: Integer; stdcall;
begin
  Result := 1;
end;

//:Return the GUID of the Plugins Options-DLG
function GetOptionsPageGUID: TGUID; stdcall;
begin
  Result := GUID_NULL;
end;

exports
  GetName,
  GetAuthor,
  GetDescription,
  GetRequiredPlugins,
  RegisterPlugin,
  UnregisterPlugin,
  GetMinorVersion,
  GetMajorVersion,
  AfterAllPluginsLoaded,
  GetOptionsPageGUID,
  JVCSMAKPlugin;

begin
end.
