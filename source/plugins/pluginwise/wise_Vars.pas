(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: wise_Vars.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/21  JDuenow   - launched Wise Module
2005/01/04  BSchranz  - Plugin created
2005/02/05  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit wise_Vars;

{$I jedi.inc}

interface

uses
  msTLB, Windows;

var
  MakeStudio: IJApplication;
  Canceled: Boolean = False;

resourcestring
  struPluginName = 'Wise Installer Plugin';
  struPluginAuthor = 'Jeremy Dünow (jeremy.duenow@optimeas.de)';
  struPluginHint = 'Plugin for Wise Installer';

resourcestring
  stdBreak = '****************************************************************';

resourcestring
  stdWiseCaption            = 'Wise Installationsprojekt';
  stdOutputDir              = 'Ausgabeverzeichnis:';
  stdProjects               = 'Wise Installer Projektdatei(n)';
  stdStartingBatch          = 'Wise starten...';
  stdMovingFiles            = 'Dateien kopieren...'; 
  stdDefaultPath            = 'C:\Programme\Wise InstallBuilder 8.1\WISE32.EXE';

  stdErrNoWiseExec          = 'Wise Installer Programmdatei nicht gefunden';
  stdErrNoProjects          = 'Kein(e) Wise Installationsprojekt(e) eingetragen';
  stdCategory = 'Installation';

const
  stdcExecPath              = 'ExecName';
  stdcOutputDir             = 'OutputDir';
  stdcProjectCount          = 'ProjectsCount';
  stdcProjects              = 'Project%d';

  //First & Latest Supported Wise Installer Version
  stdcFVersion              = 'Unknown';
  stdcLVersion              = '8.1.0.502';

  iDefaultIndent            = 2;

implementation

end.
