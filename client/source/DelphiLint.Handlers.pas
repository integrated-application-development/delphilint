{
DelphiLint Client
Copyright (C) 2024 Integrated Application Development

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
unit DelphiLint.Handlers;

interface

uses
    System.Generics.Collections
  , Vcl.Graphics
  , Vcl.Menus
  , DelphiLint.Events
  , DelphiLint.Context
  , DelphiLint.LiveData
  , DelphiLint.PopupHook
  ;

type

  THandler = class(TInterfacedObject, IIDEHandler)
  private
    FOnOwnerFreed: TEventNotifier<IIDEHandler>;
    FOnReleased: TEventNotifier<IIDEHandler>;
  protected
    function GetOnOwnerFreed: TEventNotifier<IIDEHandler>;
    function GetOnReleased: TEventNotifier<IIDEHandler>;
  public
    constructor Create;
    procedure Release;

    property OnOwnerFreed: TEventNotifier<IIDEHandler> read GetOnOwnerFreed;
    property OnReleased: TEventNotifier<IIDEHandler> read GetOnReleased;
  end;

//______________________________________________________________________________________________________________________

  TOnEditorLineChanged = reference to procedure(OldLine: Integer; NewLine: Integer; Data: Integer);

//______________________________________________________________________________________________________________________

  TEditLineHandler = class(THandler, IIDEEditLineHandler)
  private
    FOnLineChanged: TOnEditorLineChanged;
  public
    constructor Create(OnLineChanged: TOnEditorLineChanged);
    procedure OnLineChanged(OldLine: Integer; NewLine: Integer; Data: Integer);
  end;

//______________________________________________________________________________________________________________________

  TLineTracker = class;

  TChangedLine = record
    FromLine: Integer;
    ToLine: Integer;
    Tracker: TLineTracker;
  end;

//______________________________________________________________________________________________________________________

  TLineTracker = class(TObject)
  private
    FTracker: IIDEEditLineTracker;
    FNotifier: TEditLineHandler;
    FPath: string;
    FOnEditorClosed: TEventNotifier<TLineTracker>;
    FOnLineChanged: TEventNotifier<TChangedLine>;

    procedure OnNotifierTriggered(OldLine: Integer; NewLine: Integer; Data: Integer);
  public
    constructor Create(Tracker: IIDEEditLineTracker);
    destructor Destroy; override;

    procedure TrackLine(Line: Integer);
    procedure ClearTracking;

    property OnLineChanged: TEventNotifier<TChangedLine> read FOnLineChanged;
    property OnEditorClosed: TEventNotifier<TLineTracker> read FOnEditorClosed;
    property FilePath: string read FPath;
  end;

//______________________________________________________________________________________________________________________

  TEditorHandler = class(THandler, IIDEEditorHandler)
  private
    FNotifiers: TList<IIDEHandler>;
    FTrackers: TObjectList<TLineTracker>;
    FPopupHooks: TDictionary<TPopupMenu, TEditorPopupMenuHook>;
    FInitedViews: TList<IInterface>;

    FOnActiveFileChanged: TEventNotifier<string>;

    procedure OnTrackedLineChanged(const ChangedLine: TChangedLine);

    procedure InitView(const View: IIDEEditView);
    function IsViewInited(const View: IIDEEditView): Boolean;
    procedure OnAnalysisStateChanged(const StateChange: TAnalysisStateChangeContext);

    procedure OnHookFreed(const Hook: TEditorPopupMenuHook);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnViewAdded(const View: IIDEEditView);
    procedure OnViewActivated(const View: IIDEEditView);

    property OnActiveFileChanged: TEventNotifier<string> read FOnActiveFileChanged;
  end;

//______________________________________________________________________________________________________________________

  TLintLinePainter = class(TObject)
  private
    FTagTextColor: TColor;
    FTagBgColor: TColor;
    FUnderlineColor: TColor;

    procedure DrawUnderline(
      const Issue: ILiveIssue;
      const Context: TLinePaintContext
    );
    procedure DrawTag(
      const Msg: string;
      const Context: TLinePaintContext
    );
    function ColumnToPx(
      const Col: Integer;
      const Context: TLinePaintContext
    ): Integer;
  public
    procedure PaintLine(
      const Issues: TArray<ILiveIssue>;
      const Context: TLinePaintContext
    );

    constructor Create(TagTextColor: TColor; TagBgColor: TColor; UnderlineColor: TColor);
  end;

//______________________________________________________________________________________________________________________

  TViewHandler = class(THandler, IIDEViewHandler)
  private
    FRepaint: Boolean;
    FLinePainter: TLintLinePainter;
    procedure OnAnalysisStateChanged(const StateChange: TAnalysisStateChangeContext);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnBeginPaint(const View: IIDEEditView; var FullRepaint: Boolean);
    procedure OnPaintLine(const Context: TLinePaintContext);
  end;

//______________________________________________________________________________________________________________________

implementation

uses
    System.Math
  , System.Classes
  , System.StrUtils
  , System.SysUtils
  , System.Types
  , Winapi.Windows
  , DelphiLint.Utils
  ;

//______________________________________________________________________________________________________________________

constructor THandler.Create;
begin
  inherited;

  FOnOwnerFreed := TEventNotifier<IIDEHandler>.Create;
  FOnReleased := TEventNotifier<IIDEHandler>.Create;
end;

//______________________________________________________________________________________________________________________

function THandler.GetOnOwnerFreed: TEventNotifier<IIDEHandler>;
begin
  Result := FOnOwnerFreed;
end;

//______________________________________________________________________________________________________________________

function THandler.GetOnReleased: TEventNotifier<IIDEHandler>;
begin
  Result := FOnReleased;
end;

//______________________________________________________________________________________________________________________

procedure THandler.Release;
begin
  FOnReleased.Notify(Self);
end;

//______________________________________________________________________________________________________________________

procedure TLineTracker.ClearTracking;
begin
  FTracker.Clear;
end;

//______________________________________________________________________________________________________________________

constructor TLineTracker.Create(Tracker: IIDEEditLineTracker);
var
  NotifierIndex: Integer;
begin
  inherited Create;

  FOnEditorClosed := TEventNotifier<TLineTracker>.Create;
  FOnLineChanged := TEventNotifier<TChangedLine>.Create;
  FPath := Tracker.FileName;
  FTracker := Tracker;

  FNotifier := TEditLineHandler.Create(OnNotifierTriggered);
  NotifierIndex := Tracker.AddNotifier(FNotifier);
  FNotifier.OnOwnerFreed.AddListener(
    procedure (const Notf: IIDEHandler) begin
      FNotifier := nil;
      OnEditorClosed.Notify(Self);
    end);
  FNotifier.OnReleased.AddListener(
    procedure (const Notf: IIDEHandler) begin
      FTracker.RemoveNotifier(NotifierIndex);
    end);
end;

//______________________________________________________________________________________________________________________

destructor TLineTracker.Destroy;
begin
  if Assigned(FNotifier) then begin
    FNotifier.Release;
  end;

  FreeAndNil(FOnLineChanged);
  FreeAndNil(FOnEditorClosed);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLineTracker.OnNotifierTriggered(OldLine: Integer; NewLine: Integer; Data: Integer);
var
  ChangedLine: TChangedLine;
begin
  ChangedLine.FromLine := Data;
  ChangedLine.ToLine := NewLine;
  ChangedLine.Tracker := Self;
  FOnLineChanged.Notify(ChangedLine);
end;

//______________________________________________________________________________________________________________________

procedure TLineTracker.TrackLine(Line: Integer);
begin
  FTracker.AddLine(Line, Line);
end;

//______________________________________________________________________________________________________________________

constructor TEditLineHandler.Create(OnLineChanged: TOnEditorLineChanged);
begin
  inherited Create;
  FOnLineChanged := OnLineChanged;
end;

//______________________________________________________________________________________________________________________

procedure TEditLineHandler.OnLineChanged(OldLine: Integer; NewLine: Integer; Data: Integer);
begin
  FOnLineChanged(OldLine, NewLine, Data);
end;

//______________________________________________________________________________________________________________________

constructor TEditorHandler.Create;
begin
  inherited;

  // Once registered with the IDE, notifiers are reference counted
  FNotifiers := TList<IIDEHandler>.Create;
  FTrackers := TObjectList<TLineTracker>.Create;
  // These are components, nominally owned by the context menu
  FPopupHooks := TDictionary<TPopupMenu, TEditorPopupMenuHook>.Create;
  FInitedViews := TList<IInterface>.Create;
  FOnActiveFileChanged := TEventNotifier<string>.Create;

  Analyzer.OnAnalysisStateChanged.AddListener(OnAnalysisStateChanged);
end;

//______________________________________________________________________________________________________________________

destructor TEditorHandler.Destroy;
var
  Notifier: IIDEHandler;
  Hook: TEditorPopupMenuHook;
begin
  for Notifier in FNotifiers do begin
    Notifier.Release;
  end;

  // Although these hooks are owned by the context menu, we need to free them when the plugin is disabled
  for Hook in FPopupHooks.Values do begin
    Hook.OnFreed.RemoveListener(OnHookFreed);
    FreeAndNil(Hook);
  end;
  FreeAndNil(FPopupHooks);

  if ContextValid then begin
    Analyzer.OnAnalysisStateChanged.RemoveListener(OnAnalysisStateChanged);
  end;

  FreeAndNil(FTrackers);
  FreeAndNil(FNotifiers);
  FreeAndNil(FInitedViews);
  FreeAndNil(FOnActiveFileChanged);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TEditorHandler.OnViewActivated(const View: IIDEEditView);
begin
  FOnActiveFileChanged.Notify(View.FileName);

  if not IsViewInited(View) then begin
    InitView(View);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TEditorHandler.OnViewAdded(const View: IIDEEditView);
begin
  InitView(View);
end;

//______________________________________________________________________________________________________________________

procedure TEditorHandler.InitView(const View: IIDEEditView);

  procedure InitTracker;
  var
    Tracker: TLineTracker;
    FileIssues: TArray<ILiveIssue>;
    Issue: ILiveIssue;
    BufferHandler: IIDEHandler;
    BufferHandlerIndex: Integer;
  begin
    Tracker := TLineTracker.Create(View.GetLineTracker);
    FTrackers.Add(Tracker);
    Tracker.OnLineChanged.AddListener(OnTrackedLineChanged);
    Tracker.OnEditorClosed.AddListener(
      procedure (const Trckr: TLineTracker) begin
        FTrackers.Remove(Trckr);
      end);

    BufferHandler := THandler.Create;
    FNotifiers.Add(BufferHandler);
    BufferHandlerIndex := View.AddBufferNotifier(BufferHandler);
    BufferHandler.OnOwnerFreed.AddListener(
      procedure (const Notf: IIDEHandler) begin
        FTrackers.Remove(Tracker);
        FNotifiers.Remove(Notf);
      end);
    BufferHandler.OnReleased.AddListener(
      procedure (const Notf: IIDEHandler) begin
        View.RemoveBufferNotifier(BufferHandlerIndex);
      end);

    FileIssues := Analyzer.GetIssues(Tracker.FilePath);
    for Issue in FileIssues do begin
      Tracker.TrackLine(Issue.StartLine);
      Issue.NewLineMoveSession;
    end;
  end;

  procedure InitPopupHook;
  var
    PopupHook: TEditorPopupMenuHook;
    ContextMenu: TPopupMenu;
  begin
    ContextMenu := View.GetContextMenu;
    if not FPopupHooks.ContainsKey(ContextMenu) then begin
      PopupHook := TEditorPopupMenuHook.Create(View.GetContextMenu);
      PopupHook.OnFreed.AddListener(OnHookFreed);
      FPopupHooks.Add(ContextMenu, PopupHook);
    end;
  end;

  procedure InitNotifier;
  var
    Notifier: TViewHandler;
    NotifierIndex: Integer;
  begin
    Notifier := TViewHandler.Create;
    FNotifiers.Add(Notifier);
    NotifierIndex := View.AddNotifier(Notifier);
    Notifier.OnReleased.AddListener(
      procedure(const Notf: IIDEHandler) begin
        View.RemoveNotifier(NotifierIndex);
      end);
    Notifier.OnOwnerFreed.AddListener(
      procedure(const Notf: IIDEHandler) begin
        // Only one notifier per view so this is OK
        FNotifiers.Remove(Notf);
        FInitedViews.Remove(View);
      end);
  end;

begin
  InitTracker;
  InitNotifier;
  InitPopupHook;
  FInitedViews.Add(View.Raw);
end;

//______________________________________________________________________________________________________________________

function TEditorHandler.IsViewInited(const View: IIDEEditView): Boolean;
begin
  Result := FInitedViews.Contains(View.Raw);
end;

//______________________________________________________________________________________________________________________

procedure TEditorHandler.OnAnalysisStateChanged(const StateChange: TAnalysisStateChangeContext);
var
  Tracker: TLineTracker;
  FileIssues: TArray<ILiveIssue>;
  Issue: ILiveIssue;
  SourceEditor: IIDESourceEditor;
begin
  if StateChange.Change = ascStarted then begin
    // Editor should not change at all when an analysis is started
    Exit;
  end;

  for Tracker in FTrackers do begin
    Tracker.ClearTracking;

    FileIssues := Analyzer.GetIssues(Tracker.FilePath);
    for Issue in FileIssues do begin
      Tracker.TrackLine(Issue.StartLine);
      Issue.NewLineMoveSession;
    end;
  end;

  if TryGetCurrentSourceEditor(SourceEditor) and (SourceEditor.EditViewCount <> 0) then begin
    TThread.ForceQueue(
      TThread.Current,
      procedure begin
        SourceEditor.EditViews[0].Paint;
      end);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TEditorHandler.OnHookFreed(const Hook: TEditorPopupMenuHook);
var
  Menu: TPopupMenu;
begin
  for Menu in FPopupHooks.Keys do begin
    if FPopupHooks[Menu] = Hook then begin
      FPopupHooks.Remove(Menu);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TEditorHandler.OnTrackedLineChanged(const ChangedLine: TChangedLine);
begin
  Analyzer.UpdateIssueLine(ChangedLine.Tracker.FilePath, ChangedLine.FromLine, ChangedLine.ToLine);
end;

//______________________________________________________________________________________________________________________
//
// TViewHandler
//
//______________________________________________________________________________________________________________________

constructor TViewHandler.Create;
const
  CTextColor: TColor = $00322f2d;
  CBgColor: TColor = $0036c5ff;
begin
  inherited;

  FLinePainter := TLintLinePainter.Create(CTextColor, CBgColor, CBgColor);
  FRepaint := False;
  Analyzer.OnAnalysisStateChanged.AddListener(OnAnalysisStateChanged);
end;

//______________________________________________________________________________________________________________________

destructor TViewHandler.Destroy;
begin
  if ContextValid then begin
    Analyzer.OnAnalysisStateChanged.RemoveListener(OnAnalysisStateChanged);
  end;
  FreeAndNil(FLinePainter);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TViewHandler.OnAnalysisStateChanged(const StateChange: TAnalysisStateChangeContext);
begin
  if StateChange.Change <> ascStarted then begin
    FRepaint := True;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TViewHandler.OnBeginPaint(const View: IIDEEditView; var FullRepaint: Boolean);
begin
  if FRepaint then begin
    FullRepaint := True;
    FRepaint := False;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TViewHandler.OnPaintLine(const Context: TLinePaintContext);
var
  Issues: TArray<ILiveIssue>;
begin
  Issues := Analyzer.GetIssues(Context.View.FileName, Context.LineNumber);

  if Length(Issues) > 0 then begin
    FLinePainter.PaintLine(Issues, Context);
  end;
end;

//______________________________________________________________________________________________________________________

function TLintLinePainter.ColumnToPx(
  const Col: Integer;
  const Context: TLinePaintContext
): Integer;
begin
  Result := Context.TextRect.Left + (Col + 1 - Context.View.LeftColumn) * Context.CellSize.Width;
end;

//______________________________________________________________________________________________________________________

constructor TLintLinePainter.Create(TagTextColor: TColor; TagBgColor: TColor; UnderlineColor: TColor);
begin
  inherited Create;
  FTagTextColor := TagTextColor;
  FTagBgColor := TagBgColor;
  FUnderlineColor := UnderlineColor;
end;

//______________________________________________________________________________________________________________________

procedure TLintLinePainter.DrawTag(const Msg: string; const Context: TLinePaintContext);
var
  LeftBound: Integer;
  RightBound: Integer;
  TextLeft: Integer;
  HalfHeight: Integer;
begin
  LeftBound := Context.TextRect.Right + (Context.CellSize.Width div 2);
  TextLeft := LeftBound + Context.CellSize.Width;
  HalfHeight := Context.TextRect.Height div 2;
  RightBound := TextLeft + (Length(Msg) + 2) * Context.CellSize.Width;

  if RightBound > Context.LineRect.Right then begin
    RightBound := Context.LineRect.Right;
  end;

  Context.Canvas.Brush.Color := FTagBgColor;
  Context.Canvas.Brush.Style := bsSolid;
  Context.Canvas.Polygon([
    TPoint.Create(LeftBound, Context.LineRect.Top + HalfHeight),
    TPoint.Create(TextLeft, Context.LineRect.Top),
    TPoint.Create(RightBound, Context.LineRect.Top),
    TPoint.Create(RightBound, Context.LineRect.Bottom),
    TPoint.Create(TextLeft, Context.LineRect.Bottom),
    TPoint.Create(TextLeft, Context.LineRect.Top + 2 * HalfHeight)
  ]);

  Context.Canvas.Font.Color := FTagTextColor;
  Context.Canvas.Font.Style := [fsBold];
  Context.Canvas.Brush.Style := bsClear;
  Context.Canvas.TextOut(TextLeft + Context.CellSize.Width, Context.TextRect.Top - 1, Msg);
  Context.Canvas.Font.Style := [];
end;

//______________________________________________________________________________________________________________________

procedure TLintLinePainter.DrawUnderline(
  const Issue: ILiveIssue;
  const Context: TLinePaintContext
);
var
  StartX: Integer;
  EndX: Integer;
  StartLineOffset: Integer;
  EndLineOffset: Integer;
begin
  StartLineOffset := Issue.StartLineOffset;
  if Issue.StartLine <> Context.LineNumber then begin
    StartLineOffset := 0;
  end;

  EndLineOffset := Issue.EndLineOffset;
  if Issue.EndLine <> Context.LineNumber then begin
    EndLineOffset := -1;
  end;

  StartX := Max(ColumnToPx(StartLineOffset, Context), Context.TextRect.Left);
  if EndLineOffset = -1 then begin
    EndX := Context.TextRect.Right;
  end
  else begin
    EndX := Max(ColumnToPx(EndLineOffset, Context), Context.TextRect.Left);
  end;

  Context.Canvas.Pen.Color := FUnderlineColor;
  Context.Canvas.Rectangle(
    StartX,
    Context.TextRect.Bottom - 2,
    EndX,
    Context.TextRect.Bottom
  );
end;

//______________________________________________________________________________________________________________________

procedure TLintLinePainter.PaintLine(
  const Issues: TArray<ILiveIssue>;
  const Context: TLinePaintContext
);
var
  Issue: ILiveIssue;
  Msg: string;
  NumIssuesOnLine: Integer;
begin
  NumIssuesOnLine := 0;

  for Issue in Issues do begin
    Issue.UpdateTether(Context.LineNumber, Context.LineText);
    if not Issue.IsTethered then begin
      Continue;
    end;

    DrawUnderline(Issue, Context);

    if Issue.StartLine = Context.LineNumber then begin
      NumIssuesOnLine := NumIssuesOnLine + 1;

      if Msg = '' then begin
        Msg := Issue.Message;
      end;
    end;
  end;

  if NumIssuesOnLine = 1 then begin
    DrawTag(Msg, Context);
  end
  else if NumIssuesOnLine > 1 then begin
    if (Length(Msg) > 0) and (Msg[Length(Msg)] = '.') then begin
      Msg := LeftStr(Msg, Length(Msg) - 1);
    end;

    Msg := Format('%d issues (%s + %d more)', [NumIssuesOnLine, Msg, NumIssuesOnLine - 1]);
    DrawTag(Msg, Context);
  end;
end;

//______________________________________________________________________________________________________________________

end.
