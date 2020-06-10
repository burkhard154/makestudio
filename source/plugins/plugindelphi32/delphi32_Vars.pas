(* -----------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: delphi32_Vars.pas

  The Initial Developer of the original code (JEDI VCS) is:
  Burkhard Schranz (burkhard.schranz@optimeas.de)

  Componentes and used code which is used in this code are explictly stated to
  be copyright of the respective author(s).

  Last Modified: see History

  Known Issues:
  -----------------------------------------------------------------------------

  Unit history:

  2005/01/04  BSchranz  - Plugin created
  2005/02/04  USchuster - preparations for check in
  2005/02/25  Ressourcestring added stdRegisteredPackageDeleted
  2006/04/11  BSchranz - Register IDE DLL Expert support added
  2006/04/12  BSchranz - support for borlands .dof file in compilation added

  ----------------------------------------------------------------------------- *)

unit delphi32_Vars;

{$I jedi.inc}

interface

uses
  makestudio_TLB;

var
  MakeStudio: IJApplication;
  Canceled: Boolean = False;

resourcestring
  struPluginName = 'Delphi Plugin';
  struPluginAuthor = 'Burkhard Schranz (burkhard.schranz@optimeas.de)';
  struPluginHint = 'Delphi Plugin for D5,D7,D9,D10,XE..XE8,D10 Seattle';

resourcestring
  stdmaNone = 'No Action';
  stdmaRegDPK = 'Compile and register Delphi Package';
  stdmaUnRegDPK = 'Release Package in Delphi';
  stdmaRegCOMDll = 'Register COM Dll';
  stdmaUnRegCOMDll = 'Release COM Dll';
  stdmaRegCOMExe = 'Register COM Exe';
  stdmaUnRegCOMExe = 'Release COM Exe';
  stdmaSetSearchPath = 'Set Delphi search path';
  stdmaCompile = 'Compile Delphi File';
  stdmaRunBat = 'Run Batch file';
  stdmaRegisterDllExpert = 'Compile and register IDE DLL Expert';
  stdmaDeleteBPL = 'Unregister and delete Package';
  stdmaDeleteDCP = 'Delete DCP Package File';
  stdmaRemoveSearchPath = 'Remove from Search Path';
  stdmaRemoveSearchPathIncludingSubdir = 'Remove from Search Path including Subdirectories';
  stdmaRemoveAllUserPackages = 'Remove and delete all user packages';
  stdmaResetSearchPath = 'Reset search path to Delphi defaults';
  stdmaMenuPath = 'Extra\Delphi\';

  stotBPLDir = 'BPL Directory';
  stotDCPDir = 'DCP Directory';
  stotRootPath = 'Root Directory';
  stotSearchPath = 'Searchpath';
  stotBDSPROJECTSDIR = 'BDS Project Directory';

  stdSetDVersionCaption = 'Set Delphi Version';
  stdStandardModuleCaption = 'Delphi Module';
  stdFilename = 'File name: ';
  stdNoModule = 'No Command %d';
  stdDMakAction = 'Action: ';
  stdCompilerSwitch = 'Compiler Switch: ';
  stdCommandline = 'Command line: ';
  stdOutputDir = 'Output directory: ';
  stdSearchDirs = 'Searchpath%d: ';
  stccfgFileNotFound = 'WARNING: Configuration file %s not found';
  stdUNRegisteringServer = 'UNREGISTER von %s';
  stdRegisteringServer = 'REGISTER von %s';
  stdServerRegistered = 'Server %s registered';
  stdServerUNRegistered = 'Server %s released';
  stdPackageRegistered = 'Package %s registered';
  stdPackageAlreadyRegistered = 'Package %s already registered';
  stdPackageUnRegistered = 'Package %s unregistered from Delphi';
  stdStartingCompiler = 'Compiler started...';
  stdAddingSearchPath = 'Search path added: %s';
  stdSearchPathAlreadyAdded = '%s already in search path';
  stdStartingBatch = 'Bat started';
  stdMovingFiles = 'Files are moved';
  stdForceDirectories = 'Directories are created';
  stdver5 = 'Delphi 5.0';
  stdver6 = 'Delphi 6.0';
  stdver7 = 'Delphi 7.0';
  stdver2005 = 'Delphi 2005';
  stdver2006 = 'Delphi 2006';
  stdver2007 = 'Delphi 2007';
  stdver2009 = 'Delphi 2009';
  stdver2010 = 'Delphi 2010';
  stdverXE = 'Delphi XE';
  stdverXE2 = 'Delphi XE2';
  stdverXE3 = 'Delphi XE3';
  stdverXE4 = 'Delphi XE4';
  stdverXE5 = 'Delphi XE5';
  stdverXE6 = 'Delphi XE6';
  stdverXE7 = 'Delphi XE7';
  stdverXE8 = 'Delphi XE8';
  stdverD10S = 'Delphi 10 Seattle';
  stdverD101B = 'Delphi 10.1 Berlin';
  stdverD102T = 'Delphi 10.2 Tokyo';
  stdverD103R = 'Delphi 10.3 Rio';
  stdverD104S = 'Delphi 10.4 Sydney';
  stdverUnknown = 'Delphi Version not supported';
  stdverSet = 'Delphi Version selected: ';
  stdverSetErr = 'Delphi Version %s not selected';
  stderrNoDelphi = 'ERROR: Selected Delphi Version seems not to be installed?';
  stdBPLDirNotInPath = 'Selected BPL directory not in Search path.'#10#13 + 'Shall it be added?';
  stdBPLDirNotExist = 'BPL directory "%s" of %s does not exist.'#10#13 + 'Create it?';
  stdverBPLDirNotInPath = 'BPL of %s directory is not in Search path.'#10#13 + 'Edit Environment Variables for %s?';
  stderrPathEmpty = 'Path name cannot be empty!';
  stderrCompile = 'Error compiling!';
  stderrRegister = 'Error registering Server!';
  stderrRunningBatch = 'Error executing batch file!';
  stdExpertRegistered = 'Expert %s registered';
  stdExpertAlreadyRegistered = 'Expert %s already registered';
  stdExpertUnRegistered = 'Expert %s released';
  stdRegisteredExpertDeleted = 'Expert entry for "%s" deleted';

  stdSearchDirsTxt = 'Search directories';

  stdCategoryCompiler = 'Compiler\Embarcadero\Delphi';
  stdCategoryResource = 'Compiler\Resources';

  stdRegisteredPackageDeleted = 'Package entry for "%s" deleted';
  stdRegisteredPackageFileDeleted = 'Package file "%s" deleted';
  stdRegisteredPackageFileNotDeleted = 'ERROR: Package file "%s" could NOT be deleted!';
  stdRemoveSearchPath = '%s removed from search path list';
  stdResetSearchPath = 'Reset search path to "%s"';

  stdBreak = '**************************************************************';

  stdCheckingVersion = 'Checking Delphi Version: %s';
  stdVersionInstalled = '%s installed =%s';
  stdCheckVersionPreview = 'Check if Delphi Version <%s> exists';
  stdCheckVersionCaption = 'Check Delphi Version';

  stdSetSpecialSettingsVarCaption = 'Set variable <%s> to "%s"';
  stdSetSpecialSettingsVarError = '%s not performed - Name Variable is empty';

  stdPackageNotUnRegistered = 'Fatal: Package %s could not be unregistered';
  stdPackageBPLDeleted = 'Package BPL %s deleted';
  stdPackageBPLNotDeleted = 'Fatal: Package BPL %s not deleted';
  stdPackageDCPDeleted = 'Package DCP %s deleted';
  stdPackageDCPNotDeleted = 'Fatal: Package DCP %s not deleted';
  stdDependingDCPNotDeleted = 'Warning: DCP file for %s not found';

  stdActualDelphiVersion = 'Actual Delphi Version: %s';
  stdRootDirectory = 'Root directory: %s';
  stdBPLDirectory = 'BPL directory: %s';
  stdDCPDirectory = 'DCP directory: %s';
  stdRegistryKey = 'Registry Key: %s';
  stdLibDir = 'LibDir: %s';
  stdBinDir = 'BinDir: %s';
  stdPlatform = 'Platform: %s';
  stdLangDir = 'LangDir: %s';

  stdSearchPath = 'Search path:';

  stdBPLNotInSearchPath = 'BPL Path for Delphi Version "%s" is not in Windows search path'#10#13 +
    'Do you want to edit the settings?';
  stdBPLSearchpathIgnore = 'This will ignore all other failtures with path settings!';
  stdBPLPathNotExists = 'BPL Path for Delphi Version "%s" does not exist.'#10#13 + 'Do you want to create it?';
  stdPlatformWin32 = 'Windows 32 bit';
  stdPlatformWin64 = 'Windows 64 bit';
  stdPlatformOSX32 = 'MacOS X 32 bit';
  stdPlatformOSX64 = 'MacOS X 64 bit';
  stdPlatformIOSDevice = 'iOSDevice (depricated, only for Delphi Versions < 10.1 Berlin)';
  stdPlatformIOSDevice32 = 'iOSDevice 32 bit';
  stdPlatformIOSDevice64 = 'iOSDevice 64 bit';
  stdPlatformIOSSimulator = 'iOSSimulator';
  stdPlatformAndroid32 = 'Android 32 bit';
  stdPlatformAndroid64 = 'Android 64 bit';
  StrRegistryKeyNotExist = 'Registry Key %s does not exist';


const
  stdcFilename = 'Filename';
  stdcDMakAction = 'Action';
  stdcCompilerSwitch = 'CompilerSwitch';
  stdcOutputDir = 'OutputDir';
  stdcSearchDirs = 'SearchPath%d';
  stdcSearchCount = 'SearchPathCount';
  stdcAllPlatforms = 'AllPlattforms';
  stdcDelphiversion = 'Delphiversion';
  stdcBdsProjectsDir = 'BDSPROJECTSDIR';
  stdcBdsCommonDir = 'BDSCOMMONDIR';
  stdcBdsUserDir = 'BDSUSERDIR';
  stdcPlatformOSX32 = 'OSX32';
  stdcPlatformOSX64 = 'OSX64';
  stdcPlatformWIN32 = 'Win32';
  stdcPlatformWIN64 = 'Win64';
  stdcPlatformIOSDevice = 'iOSDevice';
  stdcPlatformIOSDevice32 = 'iOSDevice32';
  stdcPlatformIOSDevice64 = 'iOSDevice64';
  stdcPlatformIOSSimulator = 'iOSSimulator';
  stdcPlatformAndroid32 = 'Android32';
  stdcPlatformAndroid64 = 'Android64';
  stdcRCFile = 'RCFile';
  stdcOptions = 'Options';
  stdcSearchPath = 'Search Path';
  stdcLanguageLibraryPath = 'Language Library Path';

  // todo - better as var ?
  stDelphiRootKeyDXX: string = 'Software\Borland\Delphi\';
  stDelphiRootKeyLMDXX: string = 'SOFTWARE\Borland\Delphi\';
  stDelphiRootKeyBDS: string = 'Software\Borland\BDS\';
  stDelphiRootKeyLMBDS: string = 'SOFTWARE\Borland\BDS\';
  stCodeGearDelphiRootKeyBDS: string = 'Software\CodeGear\BDS\';
  stCodeGearDelphiRootKeyLMBDS: string = 'SOFTWARE\CodeGear\BDS\';
  stEmbarcaderoDelphiRootKeyBDS: string = 'Software\Embarcadero\BDS\';
  stEmbarcaderoDelphiRootKeyLMBDS: string = 'SOFTWARE\Embarcadero\BDS\';
  stDelphi5Key: string = '5.0\';
  stDelphi6Key: string = '6.0\';
  stDelphi7Key: string = '7.0\';
  stDelphi9Key: string = '3.0\';
  stDelphi10Key: string = '4.0\';
  stDelphi11Key: string = '5.0\';
  stDelphi12Key: string = '6.0\';
  stDelphi14Key: string = '7.0\'; // 2010
  stDelphi15Key: string = '8.0\'; // XE
  stDelphi16Key: string = '9.0\'; // XE2
  stDelphi17Key: string = '10.0\'; // XE3
  stDelphi18Key: string = '11.0\'; // XE4
  stDelphi19Key: string = '12.0\'; // XE5
  stDelphi20Key: string = '14.0\'; // XE6
  stDelphi21Key: string = '15.0\'; // XE7
  stDelphi22Key: string = '16.0\'; // XE8
  stDelphi23Key: string = '17.0\'; // D10 Seattle
  stDelphi24Key: string = '18.0\'; //D10.1 Berlin
  stDelphi25Key: string = '19.0\'; //D10.2 Tokyo
  stDelphi26Key: string = '20.0\'; //D10.3 Rio
  stDelphi27Key: string = '21.0\'; //D10.4 Sydney
  stDelphiInstallKey: string = 'RootDir';
  stdPackagesKey: string = 'Known Packages';
  stdExpertsKey: string = 'Experts';
  stdLibraryKey: string = 'Library';
  stdEnvironmentVariablesKey: string = 'Environment Variables';
  stdDPLOutValue: string = 'Package DPL Output';
  stdDCPOutValue: string = 'Package DCP Output';
  stExtPackage: string = '.dpk';
  stExtProject: string = '.dpr';
  stExtExe: string = '.exe';
  stExtBpl: string = '.bpl';
  stExtDcp: string = '.dcp';
  stExtBat: string = '.bat';

  stDelphiCompiler: string = 'dcc32';
  stBRCC32Compiler: string = 'brcc32';

  // dof File
  stdofDirectories = 'Directories';
  stdofUnitOutputDir = 'UnitOutputDir';
  stdofEXEOutputDir = 'OutputDir';
  stdofSearchPath = 'SearchPath';
  stdofCompiler = 'Compiler';
  stdofLinker = 'Linker';
  stdofPackages = 'Packages';
  stdofPackagesNoLink = 'PackageNoLink';

  // global Jedi Make variables
  stvarDelphiVersion = 'DelphiVersion';
  stvarDelphiRootDir = '$(DELPHI)';
  stvarBDSProjectDir = '$(BDSPROJECTSDIR)';
  stvarBDSDir = '$(BDS)';
  stvarRegDelphiRootKey = 'RegDelphiRootKey';
  stvarDelphiSearchPath = 'DelphiSearchPath';
  stvarBPLDir = 'DelphiBPLDir';
  stvarDCPDir = 'DelphiDCPDir';
  stvarNamespaces = 'DelphiNamespaces';
  stNamespaces = 'System;System.Win;WinApi;Vcl;Vcl.Imaging;Data;';

const
  iDefaultIndent = 2;

type
  TDMakAction = (dmaNone, dmaRegDPK, dmaUnRegDPK, dmaRegCOMDll, dmaUnRegCOMDll, dmaRegCOMExe, dmaUnRegCOMExe,
    dmaSetSearchPath, dmaCompile, dmaRunBat, dmaRegisterDllExpert, dmaDeleteBPL, dmaDeleteDCP, dmaRemoveSearchPath,
    dmaRemoveSearchPathIncludingSubdir, dmaRemoveAllUserPackages, dmaResetSearchPath);

  TGetVarOperationType = (otBPLDir, otDCPDir, otRootPath, otSearchPath, otBDSPROJECTSDIR);

  TDelphiVersion = (dver5, dver6, dver7, dver2005, dver2006, dver2007, dver2009, dver2010, dverXE, dverXE2, dverXE3,
    dverXE4, dverXE5, dverXE6, dverXE7, dverXE8, dverD10S, dverD101B, dverD102T, dverD103R, dverD104S);

  // starting with XE2
  TCompilerPlatform = (dpOSX32, dpWin32, dpWin64, dpiOSDevice, dpiOSSimulator, dpAndroid32, dpOSX64, dpiOSDevice32, dpiOSDevice64, dpAndroid64);

var
  stActions: array [dmaNone .. dmaResetSearchPath] of string = (
    stdmaNone,
    stdmaRegDPK,
    stdmaUnRegDPK,
    stdmaRegCOMDll,
    stdmaUnRegCOMDll,
    stdmaRegCOMExe,
    stdmaUnRegCOMExe,
    stdmaSetSearchPath,
    stdmaCompile,
    stdmaRunBat,
    stdmaRegisterDllExpert,
    stdmaDeleteBPL,
    stdmaDeleteDCP,
    stdmaRemoveSearchPath,
    stdmaRemoveSearchPathIncludingSubdir,
    stdmaRemoveAllUserPackages,
    stdmaResetSearchPath
  );

  stGetVarActions: array [otBPLDir .. otBDSPROJECTSDIR] of String = (
    stotBPLDir,
    stotDCPDir,
    stotRootPath,
    stotSearchPath,
    stotBDSPROJECTSDIR
  );

  // global - to publish the $(BDSPROJECTSDIR) variable
  Var_BDSProjectsDir: string;
  // global - to publish the $(DELPHI) variable
  Var_Delphi: string;
  // global - to publish the $(BDS) variable
  Var_BDS: string;

implementation

end.
