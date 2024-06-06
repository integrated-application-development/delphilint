{
DelphiLint Client
Copyright (C) 2024 Integrated Application Development

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}
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
