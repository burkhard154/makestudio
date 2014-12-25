(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: DMak.dpr

The Initial Devoloper of the original DMAK-Code is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
Code move to JEDI VCS:
  Burkhard Schranz (burkhard.schranz@optimeas.de)
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
ToDo:
- create forms on demand (USc: Done)
- translate to english (Bus: Done)
- contains still a lot Exit's which should be changed into begin end
- merge/remove FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, ... implementations
-----------------------------------------------------------------------------

Unit history:

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to MakeStudio
2003/11/28  USchuster - rebuild dpr from DMAK
2003/12/05  USchuster - re-formatted
                      - fixed #bf012 (create forms on demand)
                      - changed Application Title
2005/01/02  BSchranz  - Migration to MakeStudio with external plugins
2005/02/02  USchuster - minor style cleaning
2006/02/05  BSchranz  - include support added

-----------------------------------------------------------------------------*)

program makestudio;

{$R 'msver.res' 'msver.rc'}

uses
  Forms,
  msMain in 'msMain.pas' {FormMain},
  msRunListEdit in 'msRunListEdit.pas' {FormMemo},
  msEditDirectories in 'msEditDirectories.pas' {FormEditDirectories},
  msInfo in 'msInfo.pas' {AboutBox},
  msEditKeywordsForm in 'msEditKeywordsForm.pas' {FormKeywords},
  msUtils in 'msUtils.pas',
  msTLB in 'msTLB.pas',
  msPluginHandler in 'msPluginHandler.pas',
  msGlobals in 'msGlobals.pas',
  msActionHandler in 'msActionHandler.pas',
  msProgram in 'msProgram.pas',
  msApplication_impl in 'msApplication_impl.pas',
  msResources in 'msResources.pas',
  msVarHandler in 'msVarHandler.pas',
  msVarListInspect in 'msVarListInspect.pas' {FormVarlistInspect},
  msInternalCommands in 'msInternalCommands.pas',
  msEditWhile in 'msEditWhile.pas' {FormEditWhile},
  msEditSetVariable in 'msEditSetVariable.pas' {FormEditSetVariable},
  msEditFor in 'msEditFor.pas' {FormEditFor},
  msEditIf in 'msEditIf.pas' {FormEditIf},
  msEditTasks in 'msEditTasks.pas' {FormEditTasks},
  msFrmLogbook in 'msFrmLogbook.pas' {FormLogbook},
  msFrmCommands in 'msFrmCommands.pas' {FormCommands},
  msFrmRunListListbox in 'msFrmRunListListbox.pas' {FormRunlistLisbox},
  msEditSaveLog in 'msEditSaveLog.pas' {FormEditSaveLog},
  msFrmSelectCommandTypeByExt in 'msFrmSelectCommandTypeByExt.pas' {FormSelectCommandtypeByExt},
  mshelp in 'mshelp.pas',
  mshelpmergerWorkshopError in 'mshelpmergerWorkshopError.pas' {FormHelpWorkshopError},
  mshelpmerger in 'mshelpmerger.pas',
  mshelpmergerRun in 'mshelpmergerRun.pas' {FormHelpmergerRun},
  msDotNetUtils in 'msDotNetUtils.pas',
  msEditComment in 'msEditComment.pas' {FormEditComment},
  msEditInclude in 'msEditInclude.pas' {FormEditInclude};

{$R *.res}
{$R *.tlb}

begin
  Forms.Application.Initialize;

  InitGlobals;

  Forms.Application.Title := 'optiMEAS MakeStudio'; //D7 Fix --- stApplicationTitle;
  Forms.Application.CreateForm(TFormMain, FormMain);
  Forms.Application.Run;

  FinalizeApplication;
  FreeGlobals;
end.
