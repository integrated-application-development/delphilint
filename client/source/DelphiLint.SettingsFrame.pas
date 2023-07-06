{
DelphiLint Client for RAD Studio
Copyright (C) 2023 Integrated Application Development

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.
}
unit DelphiLint.SettingsFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DelphiLint.ToolsApiBase, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls;

type
  TLintSettingsFrame = class(TFrame)
    ServerConfigGroupBox: TGroupBox;
    ServerJarEdit: TLabeledEdit;
    ServerJavaExeEdit: TLabeledEdit;
    ServerShowConsoleCheckBox: TCheckBox;
    ServerAutoLaunchCheckBox: TCheckBox;
    ServerStartDelayEdit: TLabeledEdit;
    AdvancedLabel: TLabel;
    ClientConfigGroupBox: TGroupBox;
    ClientDarkModeCheckBox: TCheckBox;
    SonarDelphiConfigGroupBox: TGroupBox;
    SonarDelphiJarEdit: TLabeledEdit;
    JarOpenDialog: TOpenDialog;
    ExeOpenDialog: TOpenDialog;
    SonarDelphiJarBrowseButton: TButton;
    ServerJarBrowseButton: TButton;
    ServerJavaExeBrowseButton: TButton;
    SettingsGridPanel: TGridPanel;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    procedure SonarDelphiJarBrowseButtonClick(Sender: TObject);
    procedure ServerJarBrowseButtonClick(Sender: TObject);
    procedure ServerJavaExeBrowseButtonClick(Sender: TObject);
  public
    procedure Init;
    procedure Save;
    function Validate: Boolean;
  end;

  TLintAddInOptions = class(TAddInOptionsBase)
  private
    FFrame: TLintSettingsFrame;
  public
    function GetFrameClass: TCustomFrameClass; override;
    function GetCaption: string; override;
    procedure FrameCreated(AFrame: TCustomFrame); override;
    procedure DialogClosed(Accepted: Boolean); override;
    function ValidateContents: Boolean; override;
  end;

implementation

uses
    DelphiLint.Settings
  ;

{$R *.dfm}

//______________________________________________________________________________________________________________________

procedure TLintAddInOptions.FrameCreated(AFrame: TCustomFrame);
begin
  inherited;
  FFrame := TLintSettingsFrame(AFrame);
  FFrame.Init;
end;

//______________________________________________________________________________________________________________________

function TLintAddInOptions.GetCaption: string;
begin
  Result := 'DelphiLint';
end;

//______________________________________________________________________________________________________________________

function TLintAddInOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TLintSettingsFrame;
end;

//______________________________________________________________________________________________________________________

procedure TLintAddInOptions.DialogClosed(Accepted: Boolean);
begin
  inherited;
  if Accepted then begin
    FFrame.Save;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintAddInOptions.ValidateContents: Boolean;
begin
  Result := FFrame.Validate;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettingsFrame.Init;
begin
  LintSettings.Load;
  ServerJarEdit.Text := LintSettings.ServerJar;
  ServerShowConsoleCheckBox.Checked := LintSettings.ServerShowConsole;
  SonarDelphiJarEdit.Text := LintSettings.SonarDelphiJar;
  ServerJavaExeEdit.Text := LintSettings.ServerJavaExe;
  ServerStartDelayEdit.Text := IntToStr(LintSettings.ServerStartDelay);
  ServerAutoLaunchCheckBox.Checked := LintSettings.ServerAutoLaunch;
  ClientDarkModeCheckBox.Checked := LintSettings.ClientDarkMode;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettingsFrame.Save;
begin
  LintSettings.ServerJar := ServerJarEdit.Text;
  LintSettings.ServerShowConsole := ServerShowConsoleCheckBox.Checked;
  LintSettings.SonarDelphiJar := SonarDelphiJarEdit.Text;
  LintSettings.ServerJavaExe := ServerJavaExeEdit.Text;
  LintSettings.ServerStartDelay := StrToInt(ServerStartDelayEdit.Text);
  LintSettings.ServerAutoLaunch := ServerAutoLaunchCheckBox.Checked;
  LintSettings.ClientDarkMode := ClientDarkModeCheckBox.Checked;
  LintSettings.Save;
end;

//______________________________________________________________________________________________________________________

function TLintSettingsFrame.Validate: Boolean;
begin
  Result := FileExists(ServerJarEdit.Text)
    and FileExists(SonarDelphiJarEdit.Text)
    and FileExists(ServerJavaExeEdit.Text);
end;

//______________________________________________________________________________________________________________________

procedure TLintSettingsFrame.ServerJarBrowseButtonClick(Sender: TObject);
begin
  JarOpenDialog.InitialDir := ExtractFilePath(ServerJarEdit.Text);
  JarOpenDialog.FileName := '';
  if JarOpenDialog.Execute then begin
    ServerJarEdit.Text := JarOpenDialog.FileName;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettingsFrame.ServerJavaExeBrowseButtonClick(Sender: TObject);
begin
  ExeOpenDialog.InitialDir := ExtractFilePath(ServerJavaExeEdit.Text);
  ExeOpenDialog.FileName := '';
  if ExeOpenDialog.Execute then begin
    ServerJavaExeEdit.Text := ExeOpenDialog.FileName;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettingsFrame.SonarDelphiJarBrowseButtonClick(Sender: TObject);
begin
  JarOpenDialog.InitialDir := ExtractFilePath(SonarDelphiJarEdit.Text);
  JarOpenDialog.FileName := '';
  if JarOpenDialog.Execute then begin
    SonarDelphiJarEdit.Text := JarOpenDialog.FileName;
  end;
end;

//______________________________________________________________________________________________________________________

end.
