unit DelphiLintTest.MockUtils;

interface

type
  TMock = class(TObject)
  public
    class function Construct<I: IInterface; T: constructor, I>(out Raw: T): I;
  end;

implementation

//______________________________________________________________________________________________________________________

class function TMock.Construct<I, T>(out Raw: T): I;
begin
  Raw := T.Create;
  Result := Raw;
end;

//______________________________________________________________________________________________________________________

end.
