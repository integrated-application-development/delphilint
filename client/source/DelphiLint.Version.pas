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
  Result := Format('%d.%d.%d', [CDlMajorVersion, CDlMinorVersion, CDlPatchVersion]);
  if CDlIsDevVersion then begin
    Result := Format('%s+dev.%s', [Result, CDlCommit]);
  end;
end;

//______________________________________________________________________________________________________________________

end.
