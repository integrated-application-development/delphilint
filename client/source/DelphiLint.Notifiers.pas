unit DelphiLint.Notifiers;

interface

uses
    DelphiLint.Events
  , DelphiLint.Context
  ;

type
  TNotifierBase = class abstract(TInterfacedObject, IIDENotifier)
  private
    FOnOwnerFreed: TEventNotifier<IIDENotifier>;
    FOnReleased: TEventNotifier<IIDENotifier>;
  protected
    function GetOnOwnerFreed: TEventNotifier<IIDENotifier>;
    function GetOnReleased: TEventNotifier<IIDENotifier>;
  public
    constructor Create;
    procedure Release;

    property OnOwnerFreed: TEventNotifier<IIDENotifier> read GetOnOwnerFreed;
    property OnReleased: TEventNotifier<IIDENotifier> read GetOnReleased;
  end;

implementation

constructor TNotifierBase.Create;
begin
  inherited;

  FOnOwnerFreed := TEventNotifier<IIDENotifier>.Create;
  FOnReleased := TEventNotifier<IIDENotifier>.Create;
end;

function TNotifierBase.GetOnOwnerFreed: TEventNotifier<IIDENotifier>;
begin
  Result := FOnOwnerFreed;
end;

function TNotifierBase.GetOnReleased: TEventNotifier<IIDENotifier>;
begin
  Result := FOnReleased;
end;

procedure TNotifierBase.Release;
begin
  FOnReleased.Notify(Self);
end;

end.
