(* -----------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: delphi32_Actions.pas

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

  ----------------------------------------------------------------------------- *)

unit wizard_Actions;

{$I jedi.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, msTLB, wizard_vars, ActiveX, AxCtrls,
  JvBaseDlg, JvBrowseFolder, JclShell, System.Actions;

type
  TForm3 = class(TForm, IActionCallback)
    ActionList1: TActionList;
    ImageList1: TImageList;
    acNewDelphiPlugin: TAction;
    acNewDelphiCommand: TAction;
    acNewCSPlugin: TAction;
    acNewCSCommand: TAction;
    acNewQTPlugin: TAction;
    acNewQTCommand: TAction;
    acNewScriptPlugin: TAction;
    acNewScriptCommand: TAction;
    BrowseFolder: TJvBrowseForFolderDialog;
    SaveDelphiModule: TSaveDialog;
    procedure acNewDelphiPluginExecute(Sender: TObject);
    procedure acNewDelphiCommandExecute(Sender: TObject);
    procedure acNewCSPluginExecute(Sender: TObject);
  private
    function TemplateFolder: String;
  public
    procedure Execute(const Action: WideString); safecall;
    procedure RegisterActions;
  end;

var
  Form3: TForm3;

procedure GetPictureFromImageList(AImages: TImageList; AIndex: Integer; out APic: Picture);

implementation

{$R *.dfm}

uses JVCSMakPluginWizardOptions, JVCSMakPluginWizardNewCommandOptions, wizard_parser,
  JVCSMakPluginWizardCommon;

resourcestring
  StrProjectFileAlready = 'Project file already exists. Overwrite?';
  StrSourceCodeCreated = 'Source code in "%s" created. Do you want to open the folder';

procedure GetPictureFromImageList(AImages: TImageList; AIndex: Integer; out APic: Picture);
var
  pic: TPicture;
begin
  pic := TPicture.Create;
  try
    AImages.GetBitmap(AIndex, pic.Bitmap);
    GetOlePicture(pic, APic);
  finally
    pic.Free;
  end;
end;

procedure TForm3.acNewCSPluginExecute(Sender: TObject);
var
  ParameterList: TStringList;
  BmpCommand, BmpAction: TBitmap;
  FilesPrefix, TargetFolder: String;

  procedure CreateTarget(aFilename: String);
  begin
    ParameterList.Values['MODULEIDENT'] := ChangeFileExt(FilesPrefix + aFilename, '');
    GetTemplateSource(TemplateFolder + sTemplatePrefixCs + aFilename, TargetFolder + FilesPrefix +
      aFilename, ParameterList);
  end;

  function GetUsesStr(aFilename: String; FormName: String): String;
  begin
    if FormName <> '' then
      Result := Format('%s in ''%s'' {%s}', [ChangeFileExt(FilesPrefix + aFilename, ''),
        FilesPrefix + aFilename, FormName])
    else
      Result := Format('%s in ''%s''', [ChangeFileExt(FilesPrefix + aFilename, ''),
        FilesPrefix + aFilename]);
  end;

begin
  try
    ParameterList := TStringList.Create;
    BmpCommand := TBitmap.Create;
    BmpAction := TBitmap.Create;
    try
      if GetPluginOptions(wkCSharp, ParameterList, BmpCommand, BmpAction) then
        if BrowseFolder.Execute then
        begin
          ParameterList.Values['BLOCKEXTERNALWIZARD'] := '1';
          TargetFolder := IncludeTrailingPathDelimiter(BrowseFolder.Directory);
          FilesPrefix := ParameterList.Values[FilesPrefixParameterName];

          if FileExists(TargetFolder + FilesPrefix + sPluginDpr) then
            if MessageDlg(StrProjectFileAlready, mtConfirmation, mbYesNo, 0) <> mrYes then
              exit;

          ParameterList.Values['EDITUNIT'] := ParameterList.Values['PLUGINIDENTIFIER']+'Edit';
          if ParameterList.Values['BLOCKMENUACTION'] <> '0' then
          begin
            CreateTarget(sActionsCs);
            CreateTarget(sActionsResx);
            CreateTarget(sActionTestCs);
            CreateTarget(sActionTestResx);
            ParameterList.Values['USEACTIONS'] := GetUsesStr(sActionsPas, 'FormActions');
            ParameterList.Values['USEACTIONTEST'] := GetUsesStr(sActionTestPas, 'FormActionTest');
            ParameterList.Values['BLOCKEXTERNALMENUACTION'] := '1';
          end;
          CreateTarget(sEditCs);
          CreateTarget(sEditResx);
          CreateTarget(sModuleCs);
//          CreateTarget(sVarsPas);

          ParameterList.Values['USEVARS'] := GetUsesStr(sVarsPas, '');
          ParameterList.Values['USEEDIT'] := GetUsesStr(sEditPas, 'FormEditParams');
          ParameterList.Values['USEMODULE'] := GetUsesStr(sModulePas, '');

          CreateTarget(sPluginCsproj);
          CreateTarget(sPluginCs);

          BmpAction.SaveToFile(TargetFolder + StringReplace(ParameterList.Values['MENUACTIONPATH'],
            '\', '', [rfReplaceAll]) + '.bmp');
          BmpCommand.SaveToFile(TargetFolder + StringReplace(ParameterList.Values['COMMANDNAME'],
            '\', '', [rfReplaceAll]) + '.bmp');

          if MessageDlg(Format(StrSourceCodeCreated, [TargetFolder]), mtConfirmation, mbYesNo, 0)
            = mrYes then
            OpenFolder(TargetFolder);
        end;
    finally
      ParameterList.Free;
      BmpCommand.Free;
      BmpAction.Free;
    end;
  except
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TForm3.acNewDelphiCommandExecute(Sender: TObject);
var
  ParameterList: TStringList;
  BmpCommand, BmpAction: TBitmap;
  TargetModuleFilename, TargetEditFilename: String;

  procedure CreateTarget(aSource, aTarget: String);
  begin
    ParameterList.Values['MODULEIDENT'] := ChangeFileExt(ExtractFileName(aTarget), '');
    GetTemplateSource(TemplateFolder + sTemplatePrefix + aSource, aTarget, ParameterList);
  end;

  function GetUsesStr(aFilename: String; FormName: String): String;
  begin
    if FormName <> '' then
      Result := Format('%s in ''%s'' {%s}', [ChangeFileExt(ExtractFileName(aFilename), ''),
        ExtractFileName(aFilename), FormName])
    else
      Result := Format('%s in ''%s''', [ChangeFileExt(ExtractFileName(aFilename), ''),
        ExtractFileName(aFilename)]);
  end;

begin
  try
    ParameterList := TStringList.Create;
    BmpCommand := TBitmap.Create;
    try
      if GetNewCommandOptions(wkDelphiWin32VCL, ParameterList, BmpCommand) then
      begin

        // Get Modulefilename
        SaveDelphiModule.Filename := ParameterList.Values['COMMANDIDENTIFIER'];
        SaveDelphiModule.Title := 'Save Command File';
        if SaveDelphiModule.Execute then
          TargetModuleFilename := SaveDelphiModule.Filename
        else
          exit;

        // Edit Filename
        SaveDelphiModule.Filename := 'Edit' + ParameterList.Values['COMMANDIDENTIFIER'];
        SaveDelphiModule.Title := 'Save Edit Command File';
        if SaveDelphiModule.Execute then
          TargetEditFilename := SaveDelphiModule.Filename
        else
          exit;

        ParameterList.Values['EDITUNIT'] := ChangeFileExt( ExtractFileName(TargetEditFilename), '');
        CreateTarget(sEditPas, TargetEditFilename);
        CreateTarget(sEditDfm, ChangeFileExt(TargetEditFilename, '.dfm'));
        CreateTarget(sModulePas, TargetModuleFilename);

        BmpCommand.SaveToFile(IncludeTrailingPathDelimiter(ExtractFilePath(TargetModuleFilename)) +
          StringReplace(ParameterList.Values['COMMANDNAME'], '\', '', [rfReplaceAll]) + '.bmp');

        if MessageDlg(Format(StrSourceCodeCreated, [ExtractFilePath(TargetModuleFilename)]),
          mtConfirmation, mbYesNo, 0) = mrYes then
          OpenFolder(ExtractFilePath(TargetModuleFilename));
      end;
    finally
      ParameterList.Free;
      BmpCommand.Free;
    end;
  except
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TForm3.acNewDelphiPluginExecute(Sender: TObject);
var
  ParameterList: TStringList;
  BmpCommand, BmpAction: TBitmap;
  FilesPrefix, TargetFolder: String;

  procedure CreateTarget(aFilename: String);
  begin
    ParameterList.Values['MODULEIDENT'] := ChangeFileExt(FilesPrefix + aFilename, '');
    GetTemplateSource(TemplateFolder + sTemplatePrefix + aFilename, TargetFolder + FilesPrefix +
      aFilename, ParameterList);
  end;

  function GetUsesStr(aFilename: String; FormName: String): String;
  begin
    if FormName <> '' then
      Result := Format('%s in ''%s'' {%s}', [ChangeFileExt(FilesPrefix + aFilename, ''),
        FilesPrefix + aFilename, FormName])
    else
      Result := Format('%s in ''%s''', [ChangeFileExt(FilesPrefix + aFilename, ''),
        FilesPrefix + aFilename]);
  end;

begin
  try
    ParameterList := TStringList.Create;
    BmpCommand := TBitmap.Create;
    BmpAction := TBitmap.Create;
    try
      if GetPluginOptions(wkDelphiWin32VCL, ParameterList, BmpCommand, BmpAction) then
        if BrowseFolder.Execute then
        begin
          ParameterList.Values['BLOCKEXTERNALWIZARD'] := '1';
          TargetFolder := IncludeTrailingPathDelimiter(BrowseFolder.Directory);
          FilesPrefix := ParameterList.Values[FilesPrefixParameterName];

          if FileExists(TargetFolder + FilesPrefix + sPluginDpr) then
            if MessageDlg(StrProjectFileAlready, mtConfirmation, mbYesNo, 0) <> mrYes then
              exit;

          ParameterList.Values['EDITUNIT'] := ParameterList.Values['PLUGINIDENTIFIER']+'Edit';
          if ParameterList.Values['BLOCKMENUACTION'] <> '0' then
          begin
            CreateTarget(sActionsPas);
            CreateTarget(sActionsDfm);
            CreateTarget(sActionTestPas);
            CreateTarget(sActionTestDfm);
            ParameterList.Values['USEACTIONS'] := GetUsesStr(sActionsPas, 'FormActions');
            ParameterList.Values['USEACTIONTEST'] := GetUsesStr(sActionTestPas, 'FormActionTest');
            ParameterList.Values['BLOCKEXTERNALMENUACTION'] := '1';
          end;
          CreateTarget(sEditPas);
          CreateTarget(sEditDfm);
          CreateTarget(sModulePas);
          CreateTarget(sVarsPas);

          ParameterList.Values['USEVARS'] := GetUsesStr(sVarsPas, '');
          ParameterList.Values['USEEDIT'] := GetUsesStr(sEditPas, 'FormEditParams');
          ParameterList.Values['USEMODULE'] := GetUsesStr(sModulePas, '');

          CreateTarget(sPluginDpr);

          BmpAction.SaveToFile(TargetFolder + StringReplace(ParameterList.Values['MENUACTIONPATH'],
            '\', '', [rfReplaceAll]) + '.bmp');
          BmpCommand.SaveToFile(TargetFolder + StringReplace(ParameterList.Values['COMMANDNAME'],
            '\', '', [rfReplaceAll]) + '.bmp');

          if MessageDlg(Format(StrSourceCodeCreated, [TargetFolder]), mtConfirmation, mbYesNo, 0)
            = mrYes then
            OpenFolder(TargetFolder);
        end;
    finally
      ParameterList.Free;
      BmpCommand.Free;
      BmpAction.Free;
    end;
  except
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOk], 0);
  end;
end;

procedure TForm3.Execute(const Action: WideString);
var
  I: Integer;
begin
  for I := 0 to ActionList1.ActionCount - 1 do
    if CompareText(Action, ActionList1.Actions[I].Name) = 0 then
    begin
      ActionList1.Actions[I].Execute;
    end;
end;

procedure TForm3.RegisterActions;
var
  I: Integer;
  P: Picture;
begin
  for I := 0 to ActionList1.ActionCount - 1 do
  begin
    P := nil;
    if TAction(ActionList1.Actions[I]).Enabled then
    begin

      if TAction(ActionList1.Actions[I]).ImageIndex >= 0 then
        GetPictureFromImageList(ImageList1, TAction(ActionList1.Actions[I]).ImageIndex, P);

      jvcsmak.AddMenuAction(ActionList1.Actions[I].Name,
        strMenuPath + '\' + TAction(ActionList1.Actions[I]).Category + '\' +
        TAction(ActionList1.Actions[I]).Caption, TAction(ActionList1.Actions[I]).Hint, P,
        IActionCallback(self));
    end;
  end;
//  jvcsmak.AddMenuAction('wizard_actions_break', '-', '', nil, IActionCallback(self));
end;

function TForm3.TemplateFolder: String;
begin
  Result := IncludeTrailingPathDelimiter
    (IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'CodeTemplate');
end;

end.
