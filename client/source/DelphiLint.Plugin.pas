{
DelphiLint Client for RAD Studio
Copyright (C) 2023 Integrated Application Development

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.
}
unit DelphiLint.Plugin;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls, System.Actions, Vcl.ActnList, Vcl.Menus,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, DelphiLint.IDE;

const
  C_ImgDefault = 0;
  C_ImgSuccess = 1;
  C_ImgIssues = 2;
  C_ImgError = 3;
  C_ImgWorking = 4;
  C_ImgSuccessWarn = 5;
  C_ImgIssuesWarn = 6;

type
  TLintPlugin = class(TDataModule)
    LintImages: TImageList;
    LintActions: TActionList;
    ActionAnalyzeActiveFile: TAction;
    ActionShowToolWindow: TAction;
    ActionAnalyzeShort: TAction;
    ActionOpenProjectOptions: TAction;
    ActionOpenSettings: TAction;
    ActionAnalyzeOpenFiles: TAction;
    ActionRestartServer: TAction;
    procedure ActionShowToolWindowExecute(Sender: TObject);
    procedure ActionAnalyzeActiveFileExecute(Sender: TObject);
    procedure ActionRestartServerExecute(Sender: TObject);
    procedure ActionAnalyzeOpenFilesExecute(Sender: TObject);
  private
    FEditor: TLintEditor;
    FEditorNotifier: Integer;
    FMainMenu: TMenuItem;
    FAnalysisActionsEnabled: Boolean;

    procedure CreateMainMenu;
    procedure DestroyMainMenu;

    procedure Init;

    procedure OnAnalysisStarted(const Paths: TArray<string>);
    procedure OnAnalysisEnded(const Paths: TArray<string>);

    procedure SetAnalysisActionsEnabled(Value: Boolean);

    procedure RefreshAnalysisActions;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    property AnalysisActionsEnabled: Boolean read FAnalysisActionsEnabled write SetAnalysisActionsEnabled;
  end;

procedure Register;

function Plugin: TLintPlugin;
var
  GPlugin: TLintPlugin;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
    ToolsAPI
  , DelphiLint.Context
  , DelphiLint.ToolWindow
  , DelphiLint.NotifierBase
  ;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionAnalyzeActiveFileExecute(Sender: TObject);
begin
  LintContext.AnalyzeActiveFile;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionAnalyzeOpenFilesExecute(Sender: TObject);
begin
  LintContext.AnalyzeOpenFiles;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionRestartServerExecute(Sender: TObject);
begin
  LintContext.RestartServer;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionShowToolWindowExecute(Sender: TObject);
begin
  TLintToolWindow.ShowInstance;
end;

//______________________________________________________________________________________________________________________

function Plugin: TLintPlugin;
begin
  Result := GPlugin;
end;

//______________________________________________________________________________________________________________________

procedure Register;
begin
  GPlugin := TLintPlugin.Create(nil);
  GPlugin.Init;
end;

//______________________________________________________________________________________________________________________

constructor TLintPlugin.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FEditor := TLintEditor.Create;
  FEditor.OnOwnerFreed.AddListener(
    procedure(const Notf: TNotifierBase) begin
      if Assigned(Self) then begin
        FEditor := nil;
      end;
    end);
  FEditorNotifier := (BorlandIDEServices as IOTAEditorServices).AddNotifier(FEditor);

  CreateMainMenu;

  LintContext.OnAnalysisStarted.AddListener(OnAnalysisStarted);
  LintContext.OnAnalysisComplete.AddListener(OnAnalysisEnded);
  LintContext.OnAnalysisFailed.AddListener(OnAnalysisEnded);

  AnalysisActionsEnabled := True;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.Init;
begin
  TLintToolWindow.CreateInstance;
  FEditor.OnActiveFileChanged.AddListener(TLintToolWindow.Instance.ChangeActiveFile);
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.OnAnalysisStarted(const Paths: TArray<string>);
begin
  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.OnAnalysisEnded(const Paths: TArray<string>);
begin
  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

destructor TLintPlugin.Destroy;
begin
  if LintContextValid then begin
    LintContext.OnAnalysisStarted.RemoveListener(OnAnalysisStarted);
    LintContext.OnAnalysisComplete.RemoveListener(OnAnalysisEnded);
    LintContext.OnAnalysisFailed.RemoveListener(OnAnalysisEnded);
  end;

  DestroyMainMenu;
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(FEditorNotifier);
  TLintToolWindow.RemoveInstance;
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.CreateMainMenu;

  procedure AddItem(Action: TAction);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(FMainMenu);
    MenuItem.Action := Action;
    FMainMenu.Add(MenuItem);
  end;

  procedure AddSeparator;
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(FMainMenu);
    MenuItem.Caption := '-';
    FMainMenu.Add(MenuItem);
  end;

var
  NTAServices: INTAServices;
begin
  NTAServices := (BorlandIDEServices as INTAServices);
  FMainMenu := TMenuItem.Create(NTAServices.MainMenu);
  FMainMenu.Caption := 'DelphiLint';

  AddItem(ActionShowToolWindow);
  AddSeparator;
  AddItem(ActionAnalyzeActiveFile);
  AddItem(ActionAnalyzeOpenFiles);
  AddSeparator;
  AddItem(ActionOpenProjectOptions);
  AddItem(ActionOpenSettings);
  AddItem(ActionRestartServer);

  NTAServices.AddActionMenu('ToolsMenu', nil, FMainMenu);
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.DestroyMainMenu;
begin
  FreeAndNil(FMainMenu);
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.SetAnalysisActionsEnabled(Value: Boolean);
begin
  FAnalysisActionsEnabled := Value;
  RefreshAnalysisActions;
end;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.RefreshAnalysisActions;
begin
  if FAnalysisActionsEnabled and (not LintContext.InAnalysis) then begin
    ActionAnalyzeActiveFile.Enabled := True;
    ActionAnalyzeShort.Enabled := True;
    ActionAnalyzeOpenFiles.Enabled := True;
  end
  else begin
    ActionAnalyzeActiveFile.Enabled := False;
    ActionAnalyzeShort.Enabled := False;
    ActionAnalyzeOpenFiles.Enabled := False;
  end;
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  FreeAndNil(GPlugin);

end.
