unit DelphiLint.Version;

interface

function DelphiLintVersion: string;

implementation

uses
    System.SysUtils
  ;

//______________________________________________________________________________________________________________________

{$I dlversion.inc}

//______________________________________________________________________________________________________________________

function DelphiLintVersion: string;
begin
  Result := Format('%d.%d.%d', [C_DlMajorVersion, C_DlMinorVersion, C_DlPatchVersion]);
  if C_DlIsDevVersion then begin
    Result := Format('%s+dev.%s', [Result, C_DlCommit]);
  end;
end;

//______________________________________________________________________________________________________________________

end.
