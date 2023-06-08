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
  , DelphiLint.IDEUtils
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

  TLintEditor = class(TEditorNotifierBase)
  private
    FNotifiers: TList<TNotifierBase>;
    FTrackers: TObjectList<TEditorLineTracker>;
    FInitedViews: TList<IOTAEditView>;

    FOnActiveFileChanged: TEventNotifier<string>;

    procedure OnTrackedLineChanged(const ChangedLine: TChangedLine);

    procedure InitView(const View: IOTAEditView);
    function IsViewInited(const View: IOTAEditView): Boolean;
    procedure OnAnalysisComplete(const Paths: TArray<string>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation); override;
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView); override;

    property OnActiveFileChanged: TEventNotifier<string> read FOnActiveFileChanged;
  end;

  TLintView = class(TViewNotifierBase)
  private
    FRepaint: Boolean;
    procedure OnAnalysisComplete(const Paths: TArray<string>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure EditorIdle(const View: IOTAEditView); override;
    procedure BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean); override;
    procedure PaintLine(const View: IOTAEditView; LineNumber: Integer;
      const LineText: PAnsiChar; const TextWidth: Word; const LineAttributes: TOTAAttributeArray;
      const Canvas: TCanvas; const TextRect: TRect; const LineRect: TRect; const CellSize: TSize); override;
  end;

//______________________________________________________________________________________________________________________

implementation

uses
    System.StrUtils
  , System.Generics.Defaults
  , System.Math
  , DelphiLint.Settings
  , System.IOUtils
  , DelphiLint.Context
  ;

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

constructor TLintEditor.Create;
begin
  inherited;

  // Once registered with the IDE, notifiers are reference counted
  FNotifiers := TList<TNotifierBase>.Create;
  FTrackers := TObjectList<TEditorLineTracker>.Create;
  FInitedViews := TList<IOTAEditView>.Create;
  FOnActiveFileChanged := TEventNotifier<string>.Create;

  LintContext.OnAnalysisComplete.AddListener(OnAnalysisComplete);

  Log.Info('Editor notifier created');
end;

//______________________________________________________________________________________________________________________

destructor TLintEditor.Destroy;
var
  Notifier: TNotifierBase;
begin
  for Notifier in FNotifiers do begin
    Notifier.Release;
  end;

  if LintContextValid then begin
    LintContext.OnAnalysisComplete.RemoveListener(OnAnalysisComplete);
  end;

  FreeAndNil(FTrackers);
  FreeAndNil(FNotifiers);
  FreeAndNil(FInitedViews);
  FreeAndNil(FOnActiveFileChanged);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintEditor.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  Log.Info('View activated for ' + EditView.Buffer.FileName);

  FOnActiveFileChanged.Notify(EditView.Buffer.FileName);

  if not IsViewInited(EditView) then begin
    InitView(EditView);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintEditor.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  if Operation = opInsert then begin
    Log.Info('View created...');
    InitView(View);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintEditor.InitView(const View: IOTAEditView);
var
  Tracker: TEditorLineTracker;
  Notifier: TLintView;
  NotifierIndex: Integer;
begin
  Tracker := TEditorLineTracker.Create(View.Buffer.GetEditLineTracker);
  FTrackers.Add(Tracker);
  Tracker.OnLineChanged.AddListener(OnTrackedLineChanged);
  Tracker.OnEditorClosed.AddListener(
    procedure (const Trckr: TEditorLineTracker) begin
      FTrackers.Remove(Trckr);
    end);

  Notifier := TLintView.Create;
  FNotifiers.Add(Notifier);
  NotifierIndex := View.AddNotifier(Notifier);
  Notifier.OnReleased.AddListener(
    procedure(const Notf: TNotifierBase) begin
      View.RemoveNotifier(NotifierIndex);
    end);
  Notifier.OnOwnerFreed.AddListener(
    procedure(const Notf: TNotifierBase) begin
      // Only one notifier per view so this is OK
      FNotifiers.Remove(Notf);
      FInitedViews.Remove(View);
    end);

  FInitedViews.Add(View);

  Log.Info('Initialised view for ' + View.Buffer.FileName);
end;

//______________________________________________________________________________________________________________________

function TLintEditor.IsViewInited(const View: IOTAEditView): Boolean;
begin
  Result := FInitedViews.Contains(View);
end;

//______________________________________________________________________________________________________________________

procedure TLintEditor.OnAnalysisComplete(const Paths: TArray<string>);
var
  Tracker: TEditorLineTracker;
  FileIssues: TArray<TLiveIssue>;
  Issue: TLiveIssue;
begin
  Log.Info('Resetting tracking for ' + IntToStr(FTrackers.Count) + ' trackers.');

  for Tracker in FTrackers do begin
    Log.Info('Setting tracking for ' + Tracker.FilePath);
    Tracker.ClearTracking;

    FileIssues := LintContext.GetIssues(Tracker.FilePath);
    for Issue in FileIssues do begin
      Log.Info(Format('Tracking line %d (now %d) in %s', [Issue.StartLine, Issue.OriginalStartLine, Issue.FilePath]));
      Tracker.TrackLine(Issue.StartLine);
      Issue.NewLineMoveSession;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintEditor.OnTrackedLineChanged(const ChangedLine: TChangedLine);
begin
  Log.Info(Format('Change: %d->%d (%s)', [ChangedLine.FromLine, ChangedLine.ToLine, ChangedLine.Tracker.FilePath]));
  LintContext.UpdateIssueLine(ChangedLine.Tracker.FilePath, ChangedLine.FromLine, ChangedLine.ToLine);
end;

//______________________________________________________________________________________________________________________
//
// TLintView
//
//______________________________________________________________________________________________________________________

constructor TLintView.Create;
begin
  inherited;

  FRepaint := False;
  LintContext.OnAnalysisComplete.AddListener(OnAnalysisComplete);
end;

//______________________________________________________________________________________________________________________

destructor TLintView.Destroy;
begin
  LintContext.OnAnalysisComplete.RemoveListener(OnAnalysisComplete);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLintView.OnAnalysisComplete(const Paths: TArray<string>);
begin
  FRepaint := True;
end;

//______________________________________________________________________________________________________________________

procedure TLintView.EditorIdle(const View: IOTAEditView);
begin
  if FRepaint then begin
    View.GetEditWindow.Form.Repaint;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintView.BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
begin
  if FRepaint then begin
    FullRepaint := True;
    FRepaint := False;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintView.PaintLine(const View: IOTAEditView; LineNumber: Integer; const LineText: PAnsiChar;
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
  Issues: TArray<TLiveIssue>;
  Issue: TLiveIssue;
  Msg: string;
begin
  CurrentModule := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  Issues := LintContext.GetIssues(CurrentModule.FileName, LineNumber);

  if Length(Issues) > 0 then begin
    for Issue in Issues do begin
      Msg := Msg + ' - ' + Issue.Message;
      DrawLine(Issue.StartLineOffset, Issue.EndLineOffset);
    end;

    DrawMessage(Msg);
  end;
end;

end.
