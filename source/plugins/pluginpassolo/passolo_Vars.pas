(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: passolo_Vars.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/19  JDuenow   - launched Passolo Module
2005/01/04  BSchranz  - Plugin created
2005/02/04  USchuster - preparations for check in

-----------------------------------------------------------------------------*)

unit passolo_Vars;

{$I jedi.inc}

interface

uses
  msTLB, Windows;

var
  MakeStudio: IJApplication;
  Canceled: Boolean = False;


resourcestring
  struPluginName = 'Passolo Plugin';
  struPluginAuthor = 'Jeremy Dünow (jeremy.duenow@optimeas.de)';
  struPluginHint = 'Plugin for Passolo Localizer';

resourcestring
  stdBreak = '****************************************************************';

const
  iDefaultIndent = 2;

resourcestring
  stdPassoloCaption         = 'Passolo localization project';
  stdProjectPath            = 'Passolo Project file:';
  stdStartingBatch          = 'Starting Passolo...';

  stdErrNoPassoloExec       = 'Passolo Program file not found';
  stdeFileNotFound          = 'Passolo project file not found';
  stdCategory = 'Localization';

const
  stdcProjectPath           = 'Filename';
  stPassolo5_PSLU_TLB : string = '{6EAFC284-E3E9-4D1D-90C7-62B962F3033C}';
  //Passolo6 etc?

  //First & Latest Supported PASSOLO Version
  stdcFVersion              = 'Unknown';
  stdcLVersion              = '5.0.004';

implementation

end.
