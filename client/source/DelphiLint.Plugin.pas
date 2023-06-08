unit DelphiLint.Plugin;

interface

uses
    DelphiLint.IDE
  , DelphiLint.ToolbarManager
  ;

type
  TLintPlugin = class(TObject)
  private
    FEditor: TLintEditor;
    FEditorNotifier: Integer;
    FToolBar: TLintToolbarManager;
  public
    constructor Create;
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses
    ToolsAPI
  , DelphiLint.IDEUtils
  , System.SysUtils
  , DelphiLint.Context
  ;

var
  GPlugin: TLintPlugin;

//______________________________________________________________________________________________________________________

function Plugin: TLintPlugin;
begin
  Result := GPlugin;
end;

//______________________________________________________________________________________________________________________

procedure Register;
begin
  GPlugin := TLintPlugin.Create;
end;

//______________________________________________________________________________________________________________________

constructor TLintPlugin.Create;
begin
  RegisterPackageWizard(TLintMenuItem.Create(
    'analyzeproject',
    'Analyze Project with DelphiLint',
    procedure begin
      LintContext.AnalyzeActiveFile;
    end
  ));

  FEditor := TLintEditor.Create;
  FEditor.OnOwnerFreed.AddListener(
    procedure(const Notf: TNotifierBase) begin
      if Assigned(Self) then begin
        FEditor := nil;
      end;
    end);
  FEditorNotifier := (BorlandIDEServices as IOTAEditorServices).AddNotifier(FEditor);

  FToolBar := TLintToolbarManager.Create(nil);
  FEditor.OnActiveFileChanged.AddListener(FToolBar.ActiveFileChanged);
end;

//______________________________________________________________________________________________________________________

destructor TLintPlugin.Destroy;
begin
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(FEditorNotifier);
  FreeAndNil(FToolBar);
  inherited;
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  FreeAndNil(GPlugin);

end.
