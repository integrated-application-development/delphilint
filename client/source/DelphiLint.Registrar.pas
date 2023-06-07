unit DelphiLint.Registrar;

interface

procedure Register;

implementation

uses
    ToolsAPI
  , DelphiLint.IDE
  , DelphiLint.ToolbarManager
  , DelphiLint.Plugin
  , System.SysUtils
  , Vcl.Forms
  ;

var
  GEditorNotifier: Integer;
  GToolbar: TLintToolbarManager;

procedure Register;
begin
  RegisterPackageWizard(TLintMenuItem.Create(
    'analyzeproject',
    'Analyze Project with DelphiLint',
    procedure begin
      Plugin.AnalyzeActiveFile;
    end
  ));

  GEditorNotifier := (BorlandIDEServices as IOTAEditorServices).AddNotifier(TLintEditor.Create);
  GToolbar := TLintToolbarManager.Create(nil);
end;

initialization

finalization
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(GEditorNotifier);
  FreeAndNil(GToolbar);
end.
