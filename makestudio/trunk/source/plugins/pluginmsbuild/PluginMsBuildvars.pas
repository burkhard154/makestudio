unit PluginMsBuildvars;

interface

uses
  makestudio_TLB, system.classes, System.Win.Registry, system.SysUtils;

var
  MakeStudio: IJApplication;
  FCanceled: Boolean = False;

  gMSBuildExe : string = '';

resourcestring
  struPluginName   = 'MSBuild';
  struPluginAuthor = 'Steffen Hornig (steffen.hornig@optimeas.de)';
  struPluginHint   = 'Compile and build .NET solutions';
  stCategory       = 'Compiler\.NET';
  strErrorCompilerNotFound = 'MSBuild.exe not found!';

procedure LoadFromRegistry;
procedure SaveToRegistry;

const
  ctOutputPath         = 'Outputpath';
  ctSolutionPath       = 'SolutionPath';
  ctBuildConfigName    = 'BuildConfiguration';
  ctCleanUpBeforeBuild = 'CleanUp';

implementation

function GetRegistryKey:string;
begin
  Result := MakeStudio.ApplicationRegKey + '\plugins\msbuild\';
end;

procedure LoadFromRegistry;
var
  r : TRegistry;
begin
  r := TRegistry.Create;
  try
    if r.OpenKeyReadOnly( GetRegistryKey) then
    begin
      try
        gMSBuildExe := r.ReadString('exepath');
      except
        SaveToRegistry; // Falls ein Eintrag noch nicht in Registry vorhanden ist
      end;
    end;
  finally
    r.Free;
  end;
end;

procedure SaveToRegistry;
var
  r : TRegistry;
begin
  r := TRegistry.Create;
  try
    r.OpenKey(GetRegistryKey, true);
    r.WriteString( 'exepath', gMSBuildExe);
  finally
    r.Free;
  end;
end;

end.
