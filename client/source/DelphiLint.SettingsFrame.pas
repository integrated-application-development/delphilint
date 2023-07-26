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
    ServerShowConsoleCheckBox: TCheckBox;
    ServerAutoLaunchCheckBox: TCheckBox;
    ComponentsGroupBox: TGroupBox;
    ComponentsButton: TButton;
    BrokenSetupWarningLabel: TLabel;
    ClientConfigGroupBox: TGroupBox;
    ClientAutoShowToolWindowCheckBox: TCheckBox;
    procedure ComponentsButtonClick(Sender: TObject);
  public
    procedure Init;
    procedure Save;
  end;

  TLintAddInOptions = class(TAddInOptionsBase)
  private
    FFrame: TLintSettingsFrame;
  public
    function GetFrameClass: TCustomFrameClass; override;
    function GetCaption: string; override;
    procedure FrameCreated(AFrame: TCustomFrame); override;
    procedure DialogClosed(Accepted: Boolean); override;
  end;

implementation

uses
    DelphiLint.Settings
  , DelphiLint.SetupForm
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

procedure TLintSettingsFrame.Init;
begin
  LintSettings.Load;
  ServerShowConsoleCheckBox.Checked := LintSettings.ServerShowConsole;
  ServerAutoLaunchCheckBox.Checked := not LintSettings.ServerAutoLaunch;
  BrokenSetupWarningLabel.Visible := not TLintSetupForm.IsSetupValid;
  ClientAutoShowToolWindowCheckBox.Checked := LintSettings.ClientAutoShowToolWindow;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettingsFrame.Save;
begin
  LintSettings.ServerShowConsole := ServerShowConsoleCheckBox.Checked;
  LintSettings.ServerAutoLaunch := not ServerAutoLaunchCheckBox.Checked;
  LintSettings.ClientAutoShowToolWindow := ClientAutoShowToolWindowCheckBox.Checked;
  LintSettings.Save;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettingsFrame.ComponentsButtonClick(Sender: TObject);
var
  SetupForm: TLintSetupForm;
begin
  SetupForm := TLintSetupForm.Create(nil);
  try
    SetupForm.RefreshTheme;
    SetupForm.ShowModal;
  finally
    FreeAndNil(SetupForm);
  end;

  BrokenSetupWarningLabel.Visible := not TLintSetupForm.IsSetupValid;
end;

//______________________________________________________________________________________________________________________

end.
