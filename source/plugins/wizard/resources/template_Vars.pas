{$IFDEF BLOCKHEADER}
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcsplugintemplate_Vars.pas

The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/01/08  BSchranz  - Plugin template created
2005/02/15  USchuster - preparations for check in and modified for Wizard

-----------------------------------------------------------------------------*)
{$ENDIF BLOCKHEADER}
unit %MODULEIDENT%;

interface

uses
  msTLB;

var
  MakeStudio: IJApplication;
  FCanceled: Boolean = False;

resourcestring
  struPluginName = '%PLUGINNAME%';
  struPluginAuthor = '%PLUGINAUTHOR%';
  struPluginHint = '%PLUGINHINT%';
  stCategory = '%PLUGINCATEGORY%';

implementation

end.
