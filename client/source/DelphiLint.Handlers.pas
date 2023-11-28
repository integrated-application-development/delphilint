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
unit DelphiLint.Handlers;

interface

uses
    System.Generics.Collections
  , System.Types
  , Winapi.Windows
  , Vcl.Graphics
  , DelphiLint.Events
  , DelphiLint.Context
  ;

type

  THandler = class abstract(TInterfacedObject, IIDEHandler)
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
    FInitedViews: TList<IInterface>;

    FOnActiveFileChanged: TEventNotifier<string>;

    procedure OnTrackedLineChanged(const ChangedLine: TChangedLine);

    procedure InitView(const View: IIDEEditView);
    function IsViewInited(const View: IIDEEditView): Boolean;
    procedure OnAnalysisComplete(const Paths: TArray<string>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnViewAdded(const View: IIDEEditView);
    procedure OnViewActivated(const View: IIDEEditView);

    property OnActiveFileChanged: TEventNotifier<string> read FOnActiveFileChanged;
  end;

//______________________________________________________________________________________________________________________

  TViewHandler = class(THandler, IIDEViewHandler)
  private
    FRepaint: Boolean;
    procedure OnAnalysisComplete(const Paths: TArray<string>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OnBeginPaint(const View: IIDEEditView; var FullRepaint: Boolean);
    procedure OnPaintLine(
      const View: IIDEEditView;
      LineNumber: Integer;
      LineText: string;
      const TextWidth: Integer;
      const Canvas: TCanvas;
      const TextRect: TRect;
      const LineRect: TRect;
      const CellSize: TSize
    );
end;

//______________________________________________________________________________________________________________________

implementation

uses
    System.Math
  , System.Classes
  , System.StrUtils
  , DelphiLint.Utils
  , Vcl.Themes
  , System.SysUtils
  , DelphiLint.Data
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

procedure TLineTracker.OnNotifierTriggered(OldLine, NewLine, Data: Integer);
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
  FInitedViews := TList<IInterface>.Create;
  FOnActiveFileChanged := TEventNotifier<string>.Create;

  Analyzer.OnAnalysisComplete.AddListener(OnAnalysisComplete);
end;

//______________________________________________________________________________________________________________________

destructor TEditorHandler.Destroy;
var
  Notifier: IIDEHandler;
begin
  for Notifier in FNotifiers do begin
    Notifier.Release;
  end;

  if ContextValid then begin
    Analyzer.OnAnalysisComplete.RemoveListener(OnAnalysisComplete);
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
    FileIssues: TArray<TLiveIssue>;
    Issue: TLiveIssue;
  begin
    Tracker := TLineTracker.Create(View.GetLineTracker);
    FTrackers.Add(Tracker);
    Tracker.OnLineChanged.AddListener(OnTrackedLineChanged);
    Tracker.OnEditorClosed.AddListener(
      procedure (const Trckr: TLineTracker) begin
        FTrackers.Remove(Trckr);
      end);

    FileIssues := Analyzer.GetIssues(Tracker.FilePath);
    for Issue in FileIssues do begin
      Tracker.TrackLine(Issue.StartLine);
      Issue.NewLineMoveSession;
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
  FInitedViews.Add(View.Raw);
end;

//______________________________________________________________________________________________________________________

function TEditorHandler.IsViewInited(const View: IIDEEditView): Boolean;
begin
  Result := FInitedViews.Contains(View.Raw);
end;

//______________________________________________________________________________________________________________________

procedure TEditorHandler.OnAnalysisComplete(const Paths: TArray<string>);
var
  Tracker: TLineTracker;
  FileIssues: TArray<TLiveIssue>;
  Issue: TLiveIssue;
  SourceEditor: IIDESourceEditor;
begin
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
begin
  inherited;

  FRepaint := False;
  Analyzer.OnAnalysisComplete.AddListener(OnAnalysisComplete);
end;

//______________________________________________________________________________________________________________________

destructor TViewHandler.Destroy;
begin
  if ContextValid then begin
    Analyzer.OnAnalysisComplete.RemoveListener(OnAnalysisComplete);
  end;
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TViewHandler.OnAnalysisComplete(const Paths: TArray<string>);
begin
  FRepaint := True;
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

procedure TViewHandler.OnPaintLine(const View: IIDEEditView; LineNumber: Integer; LineText: string;
  const TextWidth: Integer; const Canvas: TCanvas; const TextRect,
  LineRect: TRect; const CellSize: TSize);

  function ColumnToPx(const Col: Integer): Integer;
  begin
    Result := TextRect.Left + (Col + 1 - View.LeftColumn) * CellSize.Width;
  end;

  procedure DrawLine(const StartChar: Integer; const EndChar: Integer; const Color: TColor);
  var
    StartX: Integer;
    EndX: Integer;
  begin
    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := 1;

    StartX := Max(ColumnToPx(StartChar), TextRect.Left);
    EndX := Max(ColumnToPx(EndChar), TextRect.Left);
    if EndChar = -1 then begin
      EndX := TextRect.Right;
    end;

    Canvas.Rectangle(
      StartX,
      TextRect.Bottom - 2,
      EndX,
      TextRect.Bottom
    );
  end;

  procedure DrawMessage(const Msg: string; const Color: TColor; const BgColor: TColor);
  var
    LeftBound: Integer;
    RightBound: Integer;
    TextLeft: Integer;
    HalfHeight: Integer;
  begin
    LeftBound := TextRect.Right + (CellSize.Width div 2);
    TextLeft := LeftBound + CellSize.Width;
    HalfHeight := TextRect.Height div 2;
    RightBound := TextLeft + (Length(Msg) + 2) * CellSize.Width;

    if RightBound > LineRect.Right then begin
      RightBound := LineRect.Right;
    end;

    Canvas.Brush.Color := BgColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.Polygon([
      TPoint.Create(LeftBound, LineRect.Top + HalfHeight),
      TPoint.Create(TextLeft, LineRect.Top),
      TPoint.Create(RightBound, LineRect.Top),
      TPoint.Create(RightBound, LineRect.Bottom),
      TPoint.Create(TextLeft, LineRect.Bottom),
      TPoint.Create(TextLeft, LineRect.Top + 2 * HalfHeight)
    ]);

    Canvas.Font.Color := Color;
    Canvas.Font.Style := [fsBold];
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(TextLeft + CellSize.Width, TextRect.Top - 1, Msg);
    Canvas.Font.Style := [];
  end;

var
  Issues: TArray<TLiveIssue>;
  Issue: TLiveIssue;
  Msg: string;
  StartLineOffset: Integer;
  EndLineOffset: Integer;
  NumIssuesOnLine: Integer;
  TextColor: TColor;
  BgColor: TColor;
begin
  Issues := Analyzer.GetIssues(View.FileName, LineNumber);

  TextColor := $00322f2d;
  BgColor := $0036c5ff;

  NumIssuesOnLine := 0;

  if Length(Issues) > 0 then begin

    for Issue in Issues do begin
      Issue.UpdateTether(LineNumber, LineText);

      if not Issue.Tethered then begin
        Continue;
      end;

      StartLineOffset := Issue.StartLineOffset;
      EndLineOffset := Issue.EndLineOffset;

      if Issue.StartLine <> LineNumber then begin
        StartLineOffset := 0;
      end;

      if Issue.EndLine <> LineNumber then begin
        EndLineOffset := -1;
      end;

      DrawLine(StartLineOffset, EndLineOffset, BgColor);

      if Issue.StartLine = LineNumber then begin
        NumIssuesOnLine := NumIssuesOnLine + 1;

        if Msg = '' then begin
          Msg := Issue.Message;
        end;
      end;
    end;

    if NumIssuesOnLine > 0 then begin
      if NumIssuesOnLine > 1 then begin
        if (Length(Msg) > 0) and (Msg[Length(Msg)] = '.') then begin
          Msg := LeftStr(Msg, Length(Msg) - 1);
        end;

        Msg := Format('%d issues (%s + %d more)', [NumIssuesOnLine, Msg, NumIssuesOnLine - 1]);
      end;

      DrawMessage(Msg, TextColor, BgColor);
    end;
  end;
end;

end.
