(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dialogs_Vars.pas

The Initial Developer of the original code (JEDI VCS) is:
  Jeremy Dünow (jeremy.duenow@optimeas.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2005/06/04  JDuenow   - launched Dialogs Module

-----------------------------------------------------------------------------*)

unit dialogs_Vars;

{$I jedi.inc}

interface

uses
  makestudio_TLB, Windows, Dialogs;

var
  MakeStudio: IJApplication;
  Canceled: Boolean = False;

resourcestring
  struPluginName         = 'Dialogs Plugin';
  struPluginAuthor       = 'Jeremy Dünow (jeremy.duenow@optimeas.de)';
  struPluginHint         = 'Plugin for working with Dialogs';

  stdBreak               = '****************************************************************';

  stdMsgBoxCaption       = 'Messagebox';
  stdInputBoxCaption     = 'Inputbox';
  stdMsgBoxReturnValue   = 'Save return value in:';
  stdInputBoxReturnValue = 'Save input value in:';
  stdStartingMsgBox      = 'Showing Messagebox';
  stdStartingInputBox    = 'Showing input box';
  stdCaption             = 'Caption:';
  stdText                = 'Text:';
  stdDefault             = 'Default Value:';
  stdButtons             = 'Buttons:';
  stdSettingVar          = 'Variable "%s" set to "%s"...';
  stdType                = 'Type:';
  stdCategory            = 'Dialogs';

  stdOk                  = 'Ok';
  stdCancel              = 'Cancel';
  stdYes                 = 'Yes';
  stdNo                  = 'No';
  stdAbort               = 'Abort';
  stdRetry               = 'Retry';
  stdIgnore              = 'Ignore';
  stdUnknown             = 'Unknown';

  stdNormal              = 'Normal';
  stdInformation         = 'Information';
  stdQuestion            = 'Question';
  stdExclamation         = 'Warning';
  stdError               = 'Error';

  stdReturnValueInput    = 'Input';
  stdReturnValueString   = 'Return value';
  stdReturnValuesString  = 'Return values';

const
  iDefaultIndent         = 2;

  stdcCaption            = 'Caption';
  stdcText               = 'Text';
  stdcDefault            = 'Default';
  stdcButtons            = 'Buttons';
  stdcStyle              = 'Style';
  stdcReturnValue        = 'ReturnValue';

  ButtonReturnValues: array[0..6] of String = (stdOk, stdCancel, stdAbort, stdRetry, stdIgnore, stdYes, stdNo);
  DialogNames: array[0..4] of String = (stdNormal, stdInformation, stdQuestion, stdExclamation, stdError);  
  DialogTypes: array[0..4] of TMsgDlgType = (mtCustom, mtInformation, mtConfirmation, mtWarning, mtError);

implementation

end.
