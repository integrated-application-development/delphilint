unit DelphiLintTest.MockUtils;

interface

uses
    System.SysUtils
  , DelphiLint.Events
  ;

type
  EMockError = class(Exception)
  end;

  TMock = class(TObject)
  public
    class function Construct<I: IInterface; T: constructor, I>(out Raw: T): I;
  end;

  THookedEventInfo<T> = record
    Method: T;
    Args: TArray<TVarRec>;
  end;

  THookedObject<TEventType> = class(TInterfacedObject)
  private
    FOnCalled: TEventNotifier<THookedEventInfo<TEventType>>;
  protected
    procedure NotifyEvent(Discriminator: TEventType); overload;
    procedure NotifyEvent(Discriminator: TEventType; Args: array of const); overload;
  public
    constructor Create;
    destructor Destroy; override;

    property OnCalled: TEventNotifier<THookedEventInfo<TEventType>> read FOnCalled;
  end;

implementation

//______________________________________________________________________________________________________________________

class function TMock.Construct<I, T>(out Raw: T): I;
begin
  Raw := T.Create;
  Result := Raw;
end;

//______________________________________________________________________________________________________________________

constructor THookedObject<TEventType>.Create;
begin
  inherited;
  FOnCalled := TEventNotifier<THookedEventInfo<TEventType>>.Create;
end;

//______________________________________________________________________________________________________________________

destructor THookedObject<TEventType>.Destroy;
begin
  FreeAndNil(FOnCalled);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure THookedObject<TEventType>.NotifyEvent(Discriminator: TEventType);
begin
  NotifyEvent(Discriminator, []);
end;

//______________________________________________________________________________________________________________________

procedure THookedObject<TEventType>.NotifyEvent(Discriminator: TEventType; Args: array of const);
var
  CallInfo: THookedEventInfo<TEventType>;
  Index: Integer;
begin
  CallInfo.Method := Discriminator;

  SetLength(CallInfo.Args, Length(Args));
  for Index := 0 to Length(Args) - 1 do begin
    CallInfo.Args[Index] := Args[Index];
  end;

  FOnCalled.Notify(CallInfo);
end;

//______________________________________________________________________________________________________________________

end.
