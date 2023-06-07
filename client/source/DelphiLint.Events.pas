unit DelphiLint.Events;

interface

uses
    System.Generics.Collections
  , System.SysUtils
  , System.Classes
  ;

type
  TEventListener<T> = reference to procedure(const Arg: T);
  TEventListener = reference to procedure;

  TEventNotifier<T> = class(TObject)
  private
    FListeners: TList<TEventListener<T>>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddListener(Listener: TEventListener<T>);
    procedure RemoveListener(Listener: TEventListener<T>);
    procedure Notify(const Arg: T);
  end;

  TEventNotifier = class(TObject)
  private
    FListeners: TList<TEventListener>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddListener(Listener: TEventListener);
    procedure RemoveListener(Listener: TEventListener);
    procedure Notify;
  end;

implementation

//______________________________________________________________________________________________________________________

constructor TEventNotifier<T>.Create;
begin
  FListeners := TList<TEventListener<T>>.Create;
end;

destructor TEventNotifier<T>.Destroy;
begin
  FreeAndNil(FListeners);
  inherited;
end;

procedure TEventNotifier<T>.AddListener(Listener: TEventListener<T>);
begin
  FListeners.Add(Listener);
end;

procedure TEventNotifier<T>.RemoveListener(Listener: TEventListener<T>);
begin
  FListeners.Remove(Listener);
end;

procedure TEventNotifier<T>.Notify(const Arg: T);
var
  Listener: TEventListener<T>;
begin
  for Listener in FListeners do begin
    Listener(Arg);
  end;
end;

//______________________________________________________________________________________________________________________

constructor TEventNotifier.Create;
begin
  FListeners := TList<TEventListener>.Create;
end;

destructor TEventNotifier.Destroy;
begin
  FreeAndNil(FListeners);
  inherited;
end;

procedure TEventNotifier.AddListener(Listener: TEventListener);
begin
  FListeners.Add(Listener);
end;

procedure TEventNotifier.RemoveListener(Listener: TEventListener);
begin
  FListeners.Remove(Listener);
end;

procedure TEventNotifier.Notify;
var
  Listener: TEventListener;
begin
  for Listener in FListeners do begin
    Listener;
  end;
end;

//______________________________________________________________________________________________________________________

end.
