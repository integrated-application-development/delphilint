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
unit DelphiLint.VersionControl;

interface

// Retrieves the Delphi source files that the Git repository containing BaseDir reports as
// changed (modified, added, or untracked). Returns False if BaseDir is not in a Git repository
// or Git could not be run.
function TryGetChangedDelphiFiles(const BaseDir: string; out ChangedFiles: TArray<string>): Boolean;

// Converts `git status --porcelain` output lines to absolute paths, excluding deleted files.
// Exposed for testing.
function ParseGitStatusOutput(const StatusLines: TArray<string>; const RepoRoot: string): TArray<string>;

implementation

uses
    System.SysUtils
  , System.Classes
  , System.IOUtils
  , System.Generics.Collections
  , Winapi.Windows
  , DelphiLint.Utils
  ;

//______________________________________________________________________________________________________________________

function TryRunGit(const Args: string; const WorkingDir: string; out Output: TArray<string>): Boolean;
const
  CBufferSize = 4096;
  CProcessTimeoutMillis = 10000;
var
  SecurityAttributes: TSecurityAttributes;
  ReadPipe: THandle;
  WritePipe: THandle;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CommandLine: string;
  Buffer: array[0..CBufferSize - 1] of Byte;
  BytesRead: Cardinal;
  OutputStream: TBytesStream;
  ExitCode: Cardinal;
begin
  Result := False;
  Output := [];

  SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
  SecurityAttributes.bInheritHandle := True;
  SecurityAttributes.lpSecurityDescriptor := nil;

  if not CreatePipe(ReadPipe, WritePipe, @SecurityAttributes, 0) then begin
    Exit;
  end;

  OutputStream := TBytesStream.Create;
  try
    // The read end must not be inherited, or the pipe stays open after the process exits
    SetHandleInformation(ReadPipe, HANDLE_FLAG_INHERIT, 0);

    ZeroMemory(@StartupInfo, SizeOf(TStartupInfo));
    StartupInfo.cb := SizeOf(TStartupInfo);
    StartupInfo.dwFlags := STARTF_USESTDHANDLES;
    StartupInfo.hStdOutput := WritePipe;

    // CreateProcess may modify the command line buffer, so it cannot share string memory
    CommandLine := 'git ' + Args;
    UniqueString(CommandLine);

    if not CreateProcess(
      nil,
      PChar(CommandLine),
      nil,
      nil,
      True,
      CREATE_NO_WINDOW,
      nil,
      PChar(WorkingDir),
      StartupInfo,
      ProcessInfo
    ) then begin
      CloseHandle(WritePipe);
      Exit;
    end;

    // Close our copy of the write end so reads terminate when the process exits
    CloseHandle(WritePipe);

    while ReadFile(ReadPipe, Buffer, CBufferSize, BytesRead, nil) and (BytesRead > 0) do begin
      OutputStream.WriteBuffer(Buffer, BytesRead);
    end;

    Result :=
      (WaitForSingleObject(ProcessInfo.hProcess, CProcessTimeoutMillis) = WAIT_OBJECT_0)
      and GetExitCodeProcess(ProcessInfo.hProcess, ExitCode)
      and (ExitCode = 0);

    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);

    if Result then begin
      Output := TEncoding.UTF8
        .GetString(OutputStream.Bytes, 0, OutputStream.Size)
        .Replace(#13, '')
        .Split([#10]);
    end;
  finally
    FreeAndNil(OutputStream);
    CloseHandle(ReadPipe);
  end;
end;

//______________________________________________________________________________________________________________________

function UnquoteGitPath(const Path: string): string;
begin
  Result := Path;
  if (Length(Result) >= 2) and (Result[1] = '"') and (Result[Length(Result)] = '"') then begin
    Result := Copy(Result, 2, Length(Result) - 2);
    Result := StringReplace(Result, '\"', '"', [rfReplaceAll]);
    Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
  end;
end;

//______________________________________________________________________________________________________________________

function ParseGitStatusOutput(const StatusLines: TArray<string>; const RepoRoot: string): TArray<string>;
var
  Files: TList<string>;
  Line: string;
  FilePath: string;
begin
  Files := TList<string>.Create;
  try
    // Porcelain format is XY <path>, where X is the index status and Y is the working tree status
    for Line in StatusLines do begin
      if Length(Line) < 4 then begin
        Continue;
      end;

      // Deleted files no longer exist in the working tree, so they cannot be analyzed
      if (Line[1] = 'D') or (Line[2] = 'D') then begin
        Continue;
      end;

      FilePath := UnquoteGitPath(Copy(Line, 4, Length(Line)));
      FilePath := StringReplace(FilePath, '/', '\', [rfReplaceAll]);
      // Validation is skipped so that unrepresentable paths are passed through instead of raising -
      // nonexistent files are filtered out downstream
      Files.Add(TPath.Combine(RepoRoot, FilePath, False));
    end;

    Result := Files.ToArray;
  finally
    FreeAndNil(Files);
  end;
end;

//______________________________________________________________________________________________________________________

function TryGetChangedDelphiFiles(const BaseDir: string; out ChangedFiles: TArray<string>): Boolean;
var
  CdupOutput: TArray<string>;
  StatusOutput: TArray<string>;
  RepoRelativeRoot: string;
  RepoRoot: string;
  Files: TList<string>;
  FilePath: string;
begin
  Result := False;
  ChangedFiles := [];

  // The relative path to the repository root is used instead of --show-toplevel, as the latter
  // resolves mapped and substituted drives to physical paths that don't match the IDE's paths
  if not TryRunGit('rev-parse --show-cdup', BaseDir, CdupOutput) then begin
    Exit;
  end;

  RepoRelativeRoot := '';
  if Length(CdupOutput) > 0 then begin
    RepoRelativeRoot := StringReplace(Trim(CdupOutput[0]), '/', '\', [rfReplaceAll]);
  end;

  if RepoRelativeRoot = '' then begin
    RepoRoot := BaseDir;
  end
  else begin
    RepoRoot := ToAbsolutePath(RepoRelativeRoot, BaseDir);
  end;

  // quotepath is disabled so that non-ASCII paths are emitted verbatim instead of escaped
  if not TryRunGit('-c core.quotepath=off status --porcelain --no-renames', BaseDir, StatusOutput) then begin
    Exit;
  end;

  Files := TList<string>.Create;
  try
    for FilePath in ParseGitStatusOutput(StatusOutput, RepoRoot) do begin
      if IsDelphiSource(FilePath) and FileExists(FilePath) then begin
        Files.Add(FilePath);
      end;
    end;

    ChangedFiles := Files.ToArray;
  finally
    FreeAndNil(Files);
  end;

  Result := True;
end;

end.
