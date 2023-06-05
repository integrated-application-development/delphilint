unit DelphiLint.IDE;

interface

uses
    System.SysUtils
  , ToolsAPI
  , DelphiLint.Server
  , Vcl.Dialogs
  , Vcl.Graphics
  , WinAPI.Windows
  , System.Classes
  , System.Generics.Collections
  , DelphiLint.Data
  , DelphiLint.Logger
  , DockForm
  , DelphiLint.Events
  , DelphiLint.ProjectOptions
  , DelphiLint.EditorSync
  ;

type

//______________________________________________________________________________________________________________________

  TIDERefreshEvent = procedure(Issues: TArray<TLintIssue>);

//______________________________________________________________________________________________________________________

  TLintMenuItem = class(TNotifierObject, IOTAWizard, IOTAMenuWizard)
  public type
    TMenuItemAction = reference to procedure;
  private
    FName: string;
    FCaption: string;
    FAction: TMenuItemAction;
  public
    constructor Create(Name: string; Caption: string; Action: TMenuItemAction);

    function GetIDstring: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    function GetMenuText: string;
  end;

  TLintEditorNotifier = class(TNotifierObject, IOTANotifier, IOTAEditorNotifier, INTAEditServicesNotifier)
  private
    FNotifiers: TDictionary<IOTAEditView, Integer>;
    FTrackers: TObjectDictionary<IOTAEditView, TEditorLineTracker>;

    procedure OnTrackedLineChanged(const ChangedLine: TChangedLine);

    procedure InitView(const View: IOTAEditView);
    procedure DeinitView(const View: IOTAEditView);
    function IsViewInited(const View: IOTAEditView): Boolean;
    procedure OnAnalysisComplete(const Issues: TArray<TLintIssue>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ViewActivated(const View: IOTAEditView);
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);

    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  end;

  TLintViewNotifier = class(TNotifierObject, IOTANotifier, INTAEditViewNotifier)
  private
    FRepaint: Boolean;
    procedure OnAnalysisComplete(const Issues: TArray<TLintIssue>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure EditorIdle(const View: IOTAEditView);
    procedure BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
    procedure PaintLine(const View: IOTAEditView; LineNumber: Integer;
      const LineText: PAnsiChar; const TextWidth: Word; const LineAttributes: TOTAAttributeArray;
      const Canvas: TCanvas; const TextRect: TRect; const LineRect: TRect; const CellSize: TSize);
    procedure EndPaint(const View: IOTAEditView);
  end;

//______________________________________________________________________________________________________________________

procedure Register;

implementation

uses
    System.StrUtils
  , DelphiLint.IDEUtils
  , System.Generics.Defaults
  , System.Math
  , DelphiLint.Settings
  , System.IOUtils
  , DelphiLint.Plugin
  ;

var
  G_EditorNotifier: Integer;


//______________________________________________________________________________________________________________________

procedure Register;
begin
  RegisterPackageWizard(TLintMenuItem.Create(
    'analyzeproject',
    'Analyze Project with DelphiLint',
    procedure begin
      Plugin.AnalyzeActiveFile;
    end
  ));

  G_EditorNotifier := (BorlandIDEServices as IOTAEditorServices).AddNotifier(TLintEditorNotifier.Create);
end;

//______________________________________________________________________________________________________________________

constructor TLintMenuItem.Create(Name: string; Caption: string; Action: TMenuItemAction);
begin
  FName := Name;
  FCaption := Caption;
  FAction := Action;
end;

//______________________________________________________________________________________________________________________

procedure TLintMenuItem.Execute;
begin
  FAction;
end;

//______________________________________________________________________________________________________________________

function TLintMenuItem.GetIDstring: string;
begin
  Result := 'DelphiLint|' + FName;
end;

//______________________________________________________________________________________________________________________

function TLintMenuItem.GetMenuText: string;
begin
  Result := FCaption;
end;

//______________________________________________________________________________________________________________________

function TLintMenuItem.GetName: string;
begin
  Result := FName;
end;

//______________________________________________________________________________________________________________________

function TLintMenuItem.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//______________________________________________________________________________________________________________________
//
// TLintViewNotifier
//
//______________________________________________________________________________________________________________________


constructor TLintEditorNotifier.Create;
begin
  FNotifiers := TDictionary<IOTAEditView, Integer>.Create;
  FTrackers := TObjectDictionary<IOTAEditView, TEditorLineTracker>.Create;

  Plugin.OnAnalysisComplete.AddListener(OnAnalysisComplete);

  Log.Info('Editor notifier created');
end;

destructor TLintEditorNotifier.Destroy;
var
  View: IOTAEditView;
begin
  Plugin.OnAnalysisComplete.RemoveListener(OnAnalysisComplete);

  for View in FNotifiers.Keys do begin
    if Assigned(View) then begin
      View.RemoveNotifier(FNotifiers[View]);
    end;
  end;

  FreeAndNil(FNotifiers);
  FreeAndNil(FTrackers);
  inherited;
end;

procedure TLintEditorNotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  Log.Info('View activated...');
  if not IsViewInited(EditView) then begin
    InitView(EditView);
  end;
end;

procedure TLintEditorNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  if Operation = opInsert then begin
    Log.Info('View created...');
    InitView(View);
  end
  else if IsViewInited(View) then begin
    Log.Info('View removed...');
    DeinitView(View);
  end;
end;

procedure TLintEditorNotifier.InitView(const View: IOTAEditView);
var
  Tracker: TEditorLineTracker;
begin
  FNotifiers.Add(View, View.AddNotifier(TLintViewNotifier.Create));

  Tracker := TEditorLineTracker.Create(View.Buffer.GetEditLineTracker);
  Tracker.OnLineChanged.AddListener(OnTrackedLineChanged);
  FTrackers.Add(View, Tracker);

  Log.Info('Initialised view for ' + View.Buffer.FileName);
end;

procedure TLintEditorNotifier.DeinitView(const View: IOTAEditView);
begin
  View.RemoveNotifier(FNotifiers[View]);
  FNotifiers.Remove(View);
  FTrackers.Remove(View);
  Log.Info('Deinitialised view for ' + View.Buffer.FileName);
end;

function TLintEditorNotifier.IsViewInited(const View: IOTAEditView): Boolean;
begin
  Result := FNotifiers.ContainsKey(View);
end;

procedure TLintEditorNotifier.OnAnalysisComplete(const Issues: TArray<TLintIssue>);
var
  Tracker: TEditorLineTracker;
  FileIssues: TArray<TLintIssue>;
  Issue: TLintIssue;
begin
  Log.Info('Resetting tracking for ' + IntToStr(FTrackers.Count) + ' trackers.');

  for Tracker in FTrackers.Values do begin
    Log.Info('Setting tracking for tracker for ' + Tracker.FilePath);
    Tracker.ClearTracking;

    FileIssues := Plugin.GetIssues(Tracker.FilePath);
    for Issue in FileIssues do begin
      Log.Info('Tracking line ' + IntToStr(Issue.Range.StartLine) + ' in ' + Issue.FilePath);
      Tracker.TrackLine(Issue.Range.StartLine);
    end;
  end;
end;

procedure TLintEditorNotifier.OnTrackedLineChanged(const ChangedLine: TChangedLine);
begin
  Log.Info(Format('Change: %d->%d (%s)', [ChangedLine.FromLine, ChangedLine.ToLine, ChangedLine.Tracker.FilePath]));
  Plugin.UpdateIssueLine(ChangedLine.Tracker.FilePath, ChangedLine.FromLine, ChangedLine.ToLine);
end;

procedure TLintEditorNotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.ViewActivated(const View: IOTAEditView);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  // Exposed for interface
end;

//______________________________________________________________________________________________________________________
//
// TLintViewNotifier
//
//______________________________________________________________________________________________________________________

constructor TLintViewNotifier.Create;
begin
  FRepaint := False;
  Plugin.OnAnalysisComplete.AddListener(OnAnalysisComplete);
end;

destructor TLintViewNotifier.Destroy;
begin
  Plugin.OnAnalysisComplete.RemoveListener(OnAnalysisComplete);
  inherited;
end;

procedure TLintViewNotifier.OnAnalysisComplete(const Issues: TArray<TLintIssue>);
begin
  FRepaint := True;
end;

procedure TLintViewNotifier.EditorIdle(const View: IOTAEditView);
begin
  if FRepaint then begin
    View.GetEditWindow.Form.Repaint;
  end;
end;

procedure TLintViewNotifier.BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
begin
  if FRepaint then begin
    FullRepaint := True;
    FRepaint := False;
  end;
end;

procedure TLintViewNotifier.EndPaint(const View: IOTAEditView);
begin
  // Exposed for interface
end;

procedure TLintViewNotifier.PaintLine(const View: IOTAEditView; LineNumber: Integer; const LineText: PAnsiChar;
  const TextWidth: Word; const LineAttributes: TOTAAttributeArray; const Canvas: TCanvas; const TextRect,
  LineRect: TRect; const CellSize: TSize);

  function ColumnToPx(const Col: Integer): Integer;
  begin
    Result := TextRect.Left + (Col + 1 - View.LeftColumn) * CellSize.Width;
  end;

  procedure DrawLine(const StartChar: Integer; const EndChar: Integer);
  var
    StartX: Integer;
    EndX: Integer;
  begin
    Canvas.Pen.Color := clWebGold;
    Canvas.Pen.Width := 1;

    StartX := Max(ColumnToPx(StartChar), TextRect.Left);
    EndX := Max(ColumnToPx(EndChar), TextRect.Left);

    Canvas.MoveTo(StartX, TextRect.Bottom - 1);
    Canvas.LineTo(EndX, TextRect.Bottom - 1);
  end;

  procedure DrawMessage(const Msg: string);
  begin
    Canvas.Font.Color := clWebGold;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(LineRect.Left + (2 * CellSize.Width), LineRect.Top, '!');
    Canvas.TextOut(TextRect.Right, TextRect.Top, Msg);
  end;

var
  CurrentModule: IOTAModule;
  Issues: TArray<TLintIssue>;
  Issue: TLintIssue;
  Msg: string;
begin
  CurrentModule := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  Issues := Plugin.GetIssues(CurrentModule.FileName, LineNumber);

  if Length(Issues) > 0 then begin
    for Issue in Issues do begin
      Msg := Msg + ' - ' + Issue.Message;
      DrawLine(Issue.Range.StartLineOffset, Issue.Range.EndLineOffset);
    end;

    DrawMessage(Msg);
  end;
end;

procedure TLintEditorNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer;
  var Handled: Boolean);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
begin
  // Exposed for interface
end;

procedure TLintEditorNotifier.WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
begin
  // Exposed for interface
end;

initialization

finalization
  (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(G_EditorNotifier);

end.
