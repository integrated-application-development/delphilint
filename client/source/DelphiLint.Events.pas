unit DelphiLint.Events;

interface

uses
    System.Generics.Collections
  ;

type
  TEventListener<T> = procedure(const Arg: T) of object;

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

implementation

uses System.SysUtils;

{ TEventNotifier<T> }

procedure TEventNotifier<T>.AddListener(Listener: TEventListener<T>);
begin
  Listeners.Add(Listener);
end;

constructor TEventNotifier<T>.Create;
begin
  Listeners := TList<TEventListener<T>>.Create;
end;

destructor TEventNotifier<T>.Destroy;
begin
  FreeAndNil(Listeners);
  inherited;
end;

procedure TEventNotifier<T>.Notify(const Arg: T);
var
  Listener: TEventListener<T>;
begin
  for Listener in Listeners do
    Listener(Arg);
end;

procedure TEventNotifier<T>.RemoveListener(Listener: TEventListener<T>);
begin
  Listeners.Remove(Listener);
end;

end.
