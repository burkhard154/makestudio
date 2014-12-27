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

2006/04/21  BSchranz  - Plugin ZIP created

-----------------------------------------------------------------------------*)
unit zip_Vars;

interface

uses
  makestudio_TLB, ZipMstr;

var
  MakeStudio: IJApplication;
  FCanceled: Boolean = False;
  ZipMaster : TZipMaster;

type
  TZIPAction = ( tzaUnzipToFolder, tzaZipFolder, tzaZipWildcardRecurse, tzaZipWildcard);

resourcestring
  struPluginName = 'Zip Plugin';
  struPluginAuthor = 'Burkhard Schranz (burkhard.schranz@optimeas.de)';
  struPluginHint = 'Integration of DelphiZIP Library';
  strZIPCredits = 'Credits to the team of DelphiZip.Net'#10+
                  'www.delphizip.net'#10;

  stCategory = 'File operations';
  stCommandname = 'Zip and Unzip';

  sttzaUnzipToFolder  = 'Unzip to folder';
  sttzaZipFolder      = 'Zip complete folder including Subdirectories';
  sttzaZipWildcard    = 'Zip by Wildcard including Subdirectories';
  sttzaZipFileList    = 'Zip by Wildcard without Subdirectories';
  strZIPMasterVersion = 'ZIPDll Version = %s';
  strZIPMasterPath    = 'ZIPDll loaded from %s';
  strError = 'ERROR: ZIP';

var
  ZipActionStrings : Array[tzaUnzipToFolder..tzaZipWildcard] of String =
    ( sttzaUnzipToFolder, sttzaZipFolder, sttzaZipWildcard, sttzaZipFileList);

const
  stdAction = 'Action';
  stdFilename = 'Filename';
  stdCount = 'Count';
  stdTrace = 'Trace';
  stdVerbose = 'Verbose';
  stdParam = 'Param%d';
  stdUpdate = 'Update';

implementation

end.
