unit DelphiLint.EditorSync;

interface

uses
    ToolsAPI
  , DelphiLint.Events
  , System.Generics.Collections
  ;

type
  TOnEditorLineChanged = procedure(OldLine: Integer; NewLine: Integer; Data: Integer) of object;

  TEditorLineNotifier = class(TNotifierObject, IOTAEditLineNotifier)
  private
    FOnLineChanged: TOnEditorLineChanged;
  public
    constructor Create(OnLineChanged: TOnEditorLineChanged);
    procedure LineChanged(OldLine: Integer; NewLine: Integer; Data: Integer);
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
    FNotifierIndex: Integer;
    FValid: Boolean;
    FPath: string;

    FOnLineChanged: TEventNotifier<TChangedLine>;

    procedure OnNotifierTriggered(OldLine: Integer; NewLine: Integer; Data: Integer);
  protected
    procedure Destroyed;
  public
    constructor Create(Tracker: IOTAEditLineTracker);
    destructor Destroy; override;

    procedure TrackLine(Line: Integer);
    procedure ClearTracking;

    property OnLineChanged: TEventNotifier<TChangedLine> read FOnLineChanged;
    property Valid: Boolean read FValid;
    property FilePath: string read FPath;
  end;

implementation

uses
    System.SysUtils
  , DelphiLint.Logger
  ;

{ TEditorLineTracker }

procedure TEditorLineTracker.ClearTracking;
var
  Index: Integer;
begin
  for Index := 0 to FTracker.Count-1 do begin
    FTracker.Delete(Index);
  end;
end;

constructor TEditorLineTracker.Create(Tracker: IOTAEditLineTracker);
begin
  FTracker := Tracker;
  FNotifier := TEditorLineNotifier.Create(OnNotifierTriggered);
  FNotifierIndex := Tracker.AddNotifier(FNotifier);
  FOnLineChanged := TEventNotifier<TChangedLine>.Create;
  FValid := True;
  FPath := Tracker.GetEditBuffer.FileName;
end;

destructor TEditorLineTracker.Destroy;
begin
  if FValid then begin
    FTracker.RemoveNotifier(FNotifierIndex);
  end;
  FreeAndNil(FOnLineChanged);
  inherited;
end;

procedure TEditorLineTracker.Destroyed;
begin
  FValid := False;
end;

procedure TEditorLineTracker.OnNotifierTriggered(OldLine, NewLine, Data: Integer);
var
  ChangedLine: TChangedLine;
begin
  ChangedLine.FromLine := OldLine;
  ChangedLine.ToLine := NewLine;
  ChangedLine.Tracker := Self;
  FOnLineChanged.Notify(ChangedLine);
end;

procedure TEditorLineTracker.TrackLine(Line: Integer);
begin
  if FValid then begin
    FTracker.AddLine(Line, 1);
  end;
end;

{ TEditorLineNotifier }

constructor TEditorLineNotifier.Create(OnLineChanged: TOnEditorLineChanged);
begin
  FOnLineChanged := OnLineChanged;
end;

procedure TEditorLineNotifier.LineChanged(OldLine, NewLine, Data: Integer);
begin
  Log.Info('(line changed)');
  FOnLineChanged(OldLine, NewLine, Data);
end;

end.
