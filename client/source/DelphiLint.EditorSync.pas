unit DelphiLint.EditorSync;

interface

uses
    ToolsAPI
  , DelphiLint.Events
  , System.Generics.Collections
  , DelphiLint.IDEUtils
  ;

type
  TOnEditorLineChanged = reference to procedure(OldLine: Integer; NewLine: Integer; Data: Integer);

  TEditorLineNotifier = class(TEditLineNotifierBase)
  private
    FOnLineChanged: TOnEditorLineChanged;
  public
    constructor Create(OnLineChanged: TOnEditorLineChanged);
    procedure LineChanged(OldLine: Integer; NewLine: Integer; Data: Integer); override;
  end;

  TEditorLineTracker = class;

  TChangedLine = record
    FromLine: Integer;
    ToLine: Integer;
    Tracker: TEditorLineTracker;
  end;

  TEditorLineTracker = class(TObject)
  private
    FTracker: IOTAEditLineTracker;
    FNotifier: TEditorLineNotifier;
    FPath: string;
    FOnEditorClosed: TEventNotifier<TEditorLineTracker>;
    FOnLineChanged: TEventNotifier<TChangedLine>;

    procedure OnNotifierTriggered(OldLine: Integer; NewLine: Integer; Data: Integer);
  public
    constructor Create(Tracker: IOTAEditLineTracker);
    destructor Destroy; override;

    procedure TrackLine(Line: Integer);
    procedure ClearTracking;

    property OnLineChanged: TEventNotifier<TChangedLine> read FOnLineChanged;
    property OnEditorClosed: TEventNotifier<TEditorLineTracker> read FOnEditorClosed;
    property FilePath: string read FPath;
  end;

implementation

uses
    System.SysUtils
  , DelphiLint.Logger
  ;

//______________________________________________________________________________________________________________________

procedure TEditorLineTracker.ClearTracking;
var
  Index: Integer;
begin
  for Index := FTracker.Count - 1 downto 0 do begin
    FTracker.Delete(Index);
  end;
end;

//______________________________________________________________________________________________________________________

constructor TEditorLineTracker.Create(Tracker: IOTAEditLineTracker);
var
  NotifierIndex: Integer;
begin
  FOnEditorClosed := TEventNotifier<TEditorLineTracker>.Create;
  FOnLineChanged := TEventNotifier<TChangedLine>.Create;
  FPath := Tracker.GetEditBuffer.FileName;
  FTracker := Tracker;

  FNotifier := TEditorLineNotifier.Create(OnNotifierTriggered);
  NotifierIndex := Tracker.AddNotifier(FNotifier);
  FNotifier.OnOwnerFreed.AddListener(
    procedure (const Notf: TNotifierBase) begin
      FNotifier := nil;
      OnEditorClosed.Notify(Self);
    end);
  FNotifier.OnReleased.AddListener(
    procedure (const Notf: TNotifierBase) begin
      FTracker.RemoveNotifier(NotifierIndex);
    end);
end;

//______________________________________________________________________________________________________________________

destructor TEditorLineTracker.Destroy;
begin
  if Assigned(FNotifier) then begin
    FNotifier.Release;
  end;

  FreeAndNil(FOnLineChanged);
  FreeAndNil(FOnEditorClosed);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TEditorLineTracker.OnNotifierTriggered(OldLine, NewLine, Data: Integer);
var
  ChangedLine: TChangedLine;
begin
  ChangedLine.FromLine := Data;
  ChangedLine.ToLine := NewLine;
  ChangedLine.Tracker := Self;
  FOnLineChanged.Notify(ChangedLine);
end;

//______________________________________________________________________________________________________________________

procedure TEditorLineTracker.TrackLine(Line: Integer);
begin
  FTracker.AddLine(Line, Line);
end;

//______________________________________________________________________________________________________________________

constructor TEditorLineNotifier.Create(OnLineChanged: TOnEditorLineChanged);
begin
  inherited Create;
  FOnLineChanged := OnLineChanged;
end;

//______________________________________________________________________________________________________________________

procedure TEditorLineNotifier.LineChanged(OldLine, NewLine, Data: Integer);
begin
  FOnLineChanged(OldLine, NewLine, Data);
end;

//______________________________________________________________________________________________________________________

end.
