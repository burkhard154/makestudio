(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: helpandmanual_Vars.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/01  JDuenow   - launched Help & Manual Module
2005/09/07  BSchranz  - translated to english

-----------------------------------------------------------------------------*)

unit helpandmanual_Vars;

{$I jedi.inc}

interface

uses
  makestudio_TLB, Windows;

var
  MakeStudio: IJApplication;
  Canceled: Boolean = False;


resourcestring
  struPluginName   = 'Help & Manual Plugin';
  struPluginAuthor = 'Jeremy Dünow (jeremy.duenow@optimeas.de)';
  struPluginHint   = 'Plugin for Help & Manual';

resourcestring
  stdBreak = '****************************************************************';

const
  iDefaultIndent = 2;

resourcestring
  stdHelpandmanualCaption      = 'Help & Manual Project';
  stdHelpandmanualMenuCaption  = 'Help & Manual Project';
  stdProjectPath               = 'Help & Manual Project files:';
  stdOutputfile                = 'Output filename:';
  stdStartingBatch             = 'Help & Manual started...';

  stdErrNoHelpandmanualExec = 'Help & Manual program file not found!';
  stdCategory               = 'Documentation';
  stdeFileNotFound          = 'ERROR: Help & Manual File "%s" not found!';

const
  stdcProjectPath           = 'Filename';
  stdcOutputFile            = 'Outputfile';
  stdcOutputOpt             = 'OutputOpt';

  //First & Latest Supported Help & Manual Version
  stdcFVersion              = 'Unknown';
  stdcLVersion              = '3.6.0.1062';

implementation

end.
