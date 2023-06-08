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
    LintPopupMenu: TPopupMenu;
    ActionAnalyzeActiveFile: TAction;
    ActionShowToolWindow: TAction;
    procedure ActionShowToolWindowExecute(Sender: TObject);
    procedure ActionAnalyzeActiveFileExecute(Sender: TObject);
  private
    FEditor: TLintEditor;
    FEditorNotifier: Integer;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
    ToolsAPI
  , DelphiLint.Context
  , DelphiLint.ToolWindow
  , DelphiLint.IDEUtils
  ;

var
  GPlugin: TLintPlugin;

//______________________________________________________________________________________________________________________

procedure TLintPlugin.ActionAnalyzeActiveFileExecute(Sender: TObject);
begin
  LintContext.AnalyzeActiveFile;
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

  TLintToolWindow.CreateInstance;
  TLintToolWindow.ShowInstance;
  FEditor.OnActiveFileChanged.AddListener(TLintToolWindow.Instance.ChangeActiveFile);
end;

//______________________________________________________________________________________________________________________

destructor TLintPlugin.Destroy;
begin
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(FEditorNotifier);
  TLintToolWindow.RemoveInstance;
  inherited;
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  FreeAndNil(GPlugin);

end.
