(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvcs_Vars.pas

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

unit jvcs_Vars;

{$I jedi.inc}

interface

uses
  msTLB, Windows;

var
  jvcsmak: IJApplication;
  Canceled: Boolean = False;


resourcestring
  struPluginName = 'JVCS Plugin';
  struPluginAuthor = 'Burkhard Schranz (burkhard.schranz@optimeas.de)';
  struPluginHint = 'Plugin for JVCS Integration';

resourcestring
  stdJVCSProjectOpCaption= 'JVCS Project Operation';
  stdJVCSSyncCaption     = 'JVCS Synchronize Projects';
  stdJVCSSyncLabelCaption= 'JVCS Synchronize Labeled Projects - Label "%s"';
  stdJVCSLabelCaption    = 'JVCS Label Projects - Label "%s"';
  stdJVCSInOutCaption    = 'JVCS Check in or out';
  stdJVCSCaption         = 'JVCS Module';
  stdSyncTxt             = 'Server %s on Port %d, username %s';
  stdProjectsTxt         = 'Projects';
  stdModulesTxt          = 'Modules';
  stdBreak = '****************************************************************';
  stdJVCSVersion = 'JVCS Version: ';
  stdJVCSRetrievingProjects = 'Query projects...';
  stdJVCSErrorJVCSEXE = 'Error executing Jvcs.exe:'#10#13'%s';
  stdNotLoggedIn = 'Login not successful!';
  stdDefaultIdentity = '<User defined>';

  stdCategory = 'Version Control';
  stdCheckout = 'OUT';
  stdCheckin = 'IN';

const
  stdcUsername           = 'Username';
  stdcPassword           = 'Password';
  stdcPasswordEncoded    = 'PasswordEncoded';
  stdcServer             = 'Server';
  stdcPort               = 'Port';
  stdcProjectCount       = 'ProjectsCount';
  stdcOperation          = 'Operation';
  stdcLabel              = 'Label';
  stdcProjects           = 'Project%d';
  stdcModules            = 'Modules%d';
  stdcRegKey             = 'Software\JEDI\JVCSMAK';

  stdcJVCSRegKey         = 'Software\JEDI\JEDIVCS';
  stdcJVCSIdentity       = '\Identities';
  stdcRegJvcsExe         = 'Jvcs.exe';
  stdcPathJVCS           = 'JEDI\JVCS\';
  stdcGetProjectListArgs = '%s:%d -user "%s" -password %s -listprojects';
  stdcSyncProjectArgs    = '%s:%d -user "%s" -password %s -syncproject %s noprompt';
  stdcSyncLabelProjectArgs = '%s:%d -user "%s" -password %s -syncproject %s Label="%s" noprompt';
  stdcLabelProjectArgs    = '%s:%d -user "%s" -password %s -labelproject %s "%s"';
  stdcGetModuleListArgs  = '%s:%d -user "%s" -password %s -listproject %s';
  stdcProceedInputFileArgs  = '%s:%d -user "%s" -password %s -input "%s"';
  stdcCheckoutIDArgs     = 'checkout %d';
  stdcCheckinIDArgs      = 'checkin %d';
  stdcUndoCheckoutIDArgs = 'undocheckout %d';
  stdcOpenProjectArgs    = 'openproject %s';

  stdcUserAccepted = 'User accepted';

  cmdJVCSEXE = 'jvcs.exe';

  JVCSExpectedDLLVersion =  090;
  JVCSLogoutTime = 600000;

  iDefaultIndent = 2;

var
  //JVCS Globals
  JVCSInitialized        : Boolean = False; //JVCSCore.dll initialized
  JVCSServerConnected    : Boolean = False; //logged in
  JVCSLoggedInUsername   : string = '';     //logged in user
  JVCSLoggedInPassword   : string = '';     //logged in password
  JVCSLoggedInServer     : string = '';     //logged in server
  JVCSLoggedInPort       : string = '';     //logged in port
  JVCSLogoutTimer        : Cardinal = 0;

  JVCSDLLVersion: DWord = 1;
  JVCSDLLVersionStr: string;

implementation

end.
