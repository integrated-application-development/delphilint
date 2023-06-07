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
  ;

var
  GEditorNotifier: Integer;
  GToolbar: TLintToolbarManager;

procedure Register;
var
  Editor: TLintEditor;
begin
  RegisterPackageWizard(TLintMenuItem.Create(
    'analyzeproject',
    'Analyze Project with DelphiLint',
    procedure begin
      Plugin.AnalyzeActiveFile;
    end
  ));

  Editor := TLintEditor.Create;
  GEditorNotifier := (BorlandIDEServices as IOTAEditorServices).AddNotifier(Editor);

  GToolbar := TLintToolbarManager.Create(nil);
  Editor.OnActiveFileChanged.AddListener(GToolbar.ActiveFileChanged);
end;

initialization

finalization
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(GEditorNotifier);
  FreeAndNil(GToolbar);
end.
