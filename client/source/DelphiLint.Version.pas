unit DelphiLint.Version;

interface

function DelphiLintVersion: string;
function DelphiLintMajorVersion: Integer;
function DelphiLintMinorVersion: Integer;
function DelphiLintPatchVersion: Integer;
function DelphiLintIsDevVersion: Boolean;
function DelphiLintCommit: string;


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
    Result := Format('%s.%s', [Result, C_DlCommit]);
  end;
end;

//______________________________________________________________________________________________________________________

function DelphiLintMajorVersion: Integer;
begin
  Result := C_DlMajorVersion;
end;

//______________________________________________________________________________________________________________________

function DelphiLintMinorVersion: Integer;
begin
  Result := C_DlMinorVersion;
end;

//______________________________________________________________________________________________________________________

function DelphiLintPatchVersion: Integer;
begin
  Result := C_DlPatchVersion;
end;

//______________________________________________________________________________________________________________________

function DelphiLintIsDevVersion: Boolean;
begin
  Result := C_DlIsDevVersion;
end;

//______________________________________________________________________________________________________________________

function DelphiLintCommit: string;
begin
  Result := C_DlCommit;
end;

//______________________________________________________________________________________________________________________

end.
