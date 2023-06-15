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
unit DelphiLint.IDE;

interface

uses
    System.SysUtils
  , ToolsAPI
  , Vcl.Dialogs
  , Vcl.Graphics
  , Winapi.Windows
  , System.Classes
  , System.Generics.Collections
  , DelphiLint.Data
  , DockForm
  , DelphiLint.Events
  , DelphiLint.EditorSync
  , DelphiLint.IDEUtils
  ;

type

//______________________________________________________________________________________________________________________

  TIDERefreshEvent = procedure(Issues: TArray<TLintIssue>);

//______________________________________________________________________________________________________________________

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
    procedure BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean); override;
    procedure PaintLine(const View: IOTAEditView; LineNumber: Integer;
      const LineText: PAnsiChar; const TextWidth: Word; const LineAttributes: TOTAAttributeArray;
      const Canvas: TCanvas; const TextRect: TRect; const LineRect: TRect; const CellSize: TSize); override;
  end;

//______________________________________________________________________________________________________________________

implementation

uses
    System.Math
  , DelphiLint.Context
  , DelphiLint.Logger
  ;

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
  FOnActiveFileChanged.Notify(EditView.Buffer.FileName);

  if not IsViewInited(EditView) then begin
    InitView(EditView);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintEditor.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  if Operation = opInsert then begin
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
  SourceEditor: IOTASourceEditor;
begin
  for Tracker in FTrackers do begin
    Tracker.ClearTracking;

    FileIssues := LintContext.GetIssues(Tracker.FilePath);
    for Issue in FileIssues do begin
      Tracker.TrackLine(Issue.StartLine);
      Issue.NewLineMoveSession;
    end;
  end;

  SourceEditor := GetCurrentSourceEditor;
  if Assigned(SourceEditor) and (SourceEditor.EditViewCount <> 0) then begin
    TThread.ForceQueue(
      TThread.Current,
      procedure begin
        SourceEditor.EditViews[0].Paint;
      end);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintEditor.OnTrackedLineChanged(const ChangedLine: TChangedLine);
begin
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
