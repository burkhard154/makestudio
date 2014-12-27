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
unit pscript_Vars;

{$I jedi.inc}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  makestudio_TLB, PSUtils;

var
  MakeStudio: IJApplication;
  FCanceled: Boolean = False;
  PSHandler: TPSHandler = nil;

resourcestring
  struPluginName = 'Pascal Script Plugin';
  struPluginAuthor = 'Burkhard Schranz (burkhard.schranz@optimeas.de)';
  struPluginHint = 'Pascal Script Extensions';
  stCategory = 'System';
  stCommandCaption = 'Pascal Script';
  stCommandHint = 'Rem Objects Pascal Script';
  StrSuccesfullyCompiled = 'Succesfully compiled';
  StrSuccesfullyExecute = 'Succesfully Execute';


implementation

end.
