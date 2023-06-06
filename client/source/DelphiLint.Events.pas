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
    Listeners: TList<TEventListener<T>>;
  public type
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddListener(Listener: TEventListener<T>);
    procedure RemoveListener(Listener: TEventListener<T>);
    procedure Notify(const Arg: T);
  end;

  TEventNotifier = class(TObject)
  private
    Listeners: TList<TEventListener>;
  public type
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
  Listeners := TList<TEventListener<T>>.Create;
end;

destructor TEventNotifier<T>.Destroy;
begin
  FreeAndNil(Listeners);
  inherited;
end;

procedure TEventNotifier<T>.AddListener(Listener: TEventListener<T>);
begin
  Listeners.Add(Listener);
end;

procedure TEventNotifier<T>.RemoveListener(Listener: TEventListener<T>);
begin
  Listeners.Remove(Listener);
end;

procedure TEventNotifier<T>.Notify(const Arg: T);
var
  Listener: TEventListener<T>;
begin
  for Listener in Listeners do
    Listener(Arg);
end;

//______________________________________________________________________________________________________________________

constructor TEventNotifier.Create;
begin
  Listeners := TList<TEventListener>.Create;
end;

destructor TEventNotifier.Destroy;
begin
  FreeAndNil(Listeners);
  inherited;
end;

procedure TEventNotifier.AddListener(Listener: TEventListener);
begin
  Listeners.Add(Listener);
end;

procedure TEventNotifier.RemoveListener(Listener: TEventListener);
begin
  Listeners.Remove(Listener);
end;

procedure TEventNotifier.Notify;
var
  Listener: TEventListener;
begin
  for Listener in Listeners do
    Listener;
end;

//______________________________________________________________________________________________________________________

end.
