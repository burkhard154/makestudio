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

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2003/11/22  BSchranz  - 1st Migrationstep from DMAK code to JVCSMAK
2005/01/02  BSchranz  - Migration to JVCSMak with external plugins
2005/02/04  USchuster - preparations for check in
2005/02/09  BSchranz  - Added Copy, Past, Docking, Debugging
2005/04/09  BSchranz  - Translated to englisch

-----------------------------------------------------------------------------*)

unit msResources;

{$I jedi.inc}

interface

resourcestring
  tecs_MissingDLLFunction = 'Missing DLL-Function %s in Module %s';
  sttwr_StrNoFunctionReturn = 'No return value for function';
  sttwr_LoadingPlugin = 'Loading Plugin %s...';
  tecs_CannotLoadRequiredPlugin = 'Necessary Plugin %s could not be loaded!';
  tecs_ErrorLoadingPlugin = 'Plungin %s could not be loaded';
  tecs_ErrorLoadingPluginClass = 'Classname = %s';
  tecs_NoAppInterface = 'Application Interface could not be created';
  tecs_PluginsAlreadyRegistered = 'Plugins already registered!';

  {$IFDEF ADDITIVE}
  rsOpenSaveFileFilter   = 'D-Make Files (*.dmk)|*.dmk|JVCS-Make Files (*.jmk)|*.jmk';
  {$ELSE}
  rsOpenSaveFileFilter   = 'Make Studio Files (*.jmk)|*.jmk';
  {$ENDIF ADDITIVE}

  stCloseQuery = 'Do you want to save your changes?';
  stSaveFile = 'To proceed this operation the actual program has to be saved!';
  stErrorCreatingSchedulerTask = 'This task could not be performed - no scheduled task created!';
  stErrorFillingSchedulerTask = 'This task could not be performed - Task created but not initialized!';
  stSchedulerTaskCreated = 'Task created an initialized!'#10#13+
                           'You will find this file typically in %systemroot%\tasks';
  stErrAddCommand = 'Error adding the command "%s"';


  stNoEdit    = 'This Item has not functionality';
  stdSetVariableCaption = 'Set variable';
  stdSaveLogCaption = 'Save logbook';
  stdIncludeCaption = 'Include';
  stdSetVariableLog = 'Variable "%s" set to "%s"...';
  stdErrSetVariableLog = 'ERROR: Variable "%s" could not be set to "%s"!';
  stdDoSaveLog = 'Save logbook in "%s"...';

  stSystemCategory = 'System';

  stCaption = 'Make Studio';
  stApplicationTitle = 'Make Studio';
  stdCmdLineWelcome = 'Make Studio';

  stdCmdLoadedPlugins = 'Loaded Plugins:'#10#13+
                        '-----------------' ;
  stdCmdLineHelp = 'Syntax: jmak <Dateiname>';
  stdCmdLineCopyright =
    'The contents of this file are subject to the Mozilla Public License'#10#13+
    'Version 1.1 (the "License"); you may not use this file except in compliance'#10#13+
    'with the License. You may obtain a copy of the License at'#10#13+
    'http://www.mozilla.org/MPL/MPL-1.1.html';

  strhHelpWelcomePage = 'Welcome to optiMEAS Make Studio Help';

const
  stcJvcsMak = 'MakeStudio';

  stcDefaultDesktopFile = 'ddefault.ini';
  stcRunDesktopFile = 'drun.ini';
  stcDebugDesktopFile = 'ddebug.ini';
  tvar_HelpPath = 'HelpPath';
  stHelpPath = 'Help';
  sHHPFilename = 'jvcsmak.hhp';
  sCHMFilename = 'jvcsmak.chm';

implementation

end.
