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
unit DelphiLint.SetupForm;

interface

uses
    System.Classes
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  ;

type
  TLintSetupForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    OkButton: TButton;
    ExeOpenDialog: TOpenDialog;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    SonarDelphiJarLabel: TLabel;
    RefreshButton: TButton;
    JavaExeBrowseButton: TButton;
    JavaExeLabel: TLabel;
    ServerJarLabel: TLabel;
    ServerJarIndicator: TPanel;
    SonarDelphiJarIndicator: TPanel;
    JavaExeIndicator: TPanel;
    JavaExeClearButton: TButton;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure JavaExeBrowseButtonClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure JavaExeClearButtonClick(Sender: TObject);
  private
    FSaved: Boolean;
    FJavaExeOverride: string;

    function GetEffectiveJavaExe: string;
    function GetJavaExeCaption: string;

    procedure UpdateControls;
    procedure UpdateValidState(Indicator: TPanel; Valid: Boolean);
    procedure UpdateOkButton;
    class function IsValidValue(Value: string): Boolean;
    function IsAllValid: Boolean;
  public
    procedure RefreshTheme;

    class function TryFixSetup(SetPluginEnabled: Boolean = True): Boolean;
    class function IsSetupValid: Boolean;
  end;

implementation

{$R *.dfm}

uses
    System.StrUtils
  , System.SysUtils
  , Vcl.Graphics
  , Vcl.Themes
  , DelphiLint.Settings
  , DelphiLint.Context
  ;

//______________________________________________________________________________________________________________________

class function TLintSetupForm.IsSetupValid: Boolean;
begin
  LintContext.Settings.Load;
  Result := IsValidValue(LintContext.Settings.JavaExe)
    and IsValidValue(LintContext.Settings.ServerJar)
    and IsValidValue(LintContext.Settings.SonarDelphiJar);
end;

//______________________________________________________________________________________________________________________

class function TLintSetupForm.TryFixSetup(SetPluginEnabled: Boolean = True): Boolean;
var
  Form: TLintSetupForm;
begin
  Result := IsSetupValid;
  if not Result then begin
    Form := TLintSetupForm.Create(nil);
    try
      Form.RefreshTheme;
      Form.ShowModal;
      Result := IsSetupValid;
    finally
      FreeAndNil(Form);
    end;
  end;

  if SetPluginEnabled then begin
    if Result then begin
      LintContext.Plugin.EnablePlugin;
    end
    else begin
      LintContext.Plugin.DisablePlugin;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.FormCreate(Sender: TObject);
begin
  FSaved := False;
  LintContext.Settings.Load;
  UpdateControls;
end;

//______________________________________________________________________________________________________________________

function TLintSetupForm.GetEffectiveJavaExe: string;
begin
  Result := IfThen(FJavaExeOverride <> '', FJavaExeOverride, LintContext.Settings.DefaultJavaExe);
end;

//______________________________________________________________________________________________________________________

function TLintSetupForm.GetJavaExeCaption: string;
begin
  Result := IfThen(FJavaExeOverride <> '', FJavaExeOverride, 'JAVA_HOME (' + LintContext.Settings.DefaultJavaExe + ')');
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.OkButtonClick(Sender: TObject);
begin
  LintContext.Settings.ServerJavaExeOverride := FJavaExeOverride;
  LintContext.Settings.Save;

  FSaved := True;
  LintContext.Plugin.EnablePlugin;
  Close;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.RefreshButtonClick(Sender: TObject);
begin
  LintContext.Settings.Load;
  UpdateControls;
  UpdateOkButton;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.RefreshTheme;
var
  WindowColor: TColor;
begin
  LintContext.IDEServices.ApplyTheme(Self);
  WindowColor := StyleServices(Self).GetSystemColor(clWindow);
  RightPanel.Color := WindowColor;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FSaved then begin
    CanClose := True;
    Exit;
  end;

  CanClose := (MessageDlg(
    'DelphiLint will not be usable until these settings are configured. Are you sure you want to close the window?',
    TMsgDlgType.mtWarning,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbCancel],
    0) = mrYes);
end;

//______________________________________________________________________________________________________________________

function TLintSetupForm.IsAllValid: Boolean;
begin
  Result := IsValidValue(GetEffectiveJavaExe)
    and IsValidValue(LintContext.Settings.ServerJar)
    and IsValidValue(LintContext.Settings.SonarDelphiJar);
end;

//______________________________________________________________________________________________________________________

class function TLintSetupForm.IsValidValue(Value: string): Boolean;
begin
  Result := FileExists(Value);
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.JavaExeBrowseButtonClick(Sender: TObject);
begin
  ExeOpenDialog.InitialDir := ExtractFilePath(
    IfThen(FJavaExeOverride <> '', FJavaExeOverride, LintContext.Settings.DefaultJavaExe));
  ExeOpenDialog.FileName := '';

  if ExeOpenDialog.Execute then begin
    FJavaExeOverride := ExeOpenDialog.FileName;
    JavaExeIndicator.Caption := GetJavaExeCaption;
    JavaExeClearButton.Enabled := FJavaExeOverride <> '';
  end;
  UpdateValidState(JavaExeIndicator, IsValidValue(GetEffectiveJavaExe));
  UpdateOkButton;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.JavaExeClearButtonClick(Sender: TObject);
begin
  FJavaExeOverride := '';
  JavaExeIndicator.Caption := GetJavaExeCaption;
  JavaExeClearButton.Enabled := False;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.UpdateControls;
begin
  FJavaExeOverride := LintContext.Settings.ServerJavaExeOverride;
  JavaExeClearButton.Enabled := (FJavaExeOverride <> '');

  JavaExeIndicator.Caption := GetJavaExeCaption;
  ServerJarIndicator.Caption := LintContext.Settings.ServerJar;
  SonarDelphiJarIndicator.Caption := LintContext.Settings.SonarDelphiJar;
  UpdateValidState(JavaExeIndicator, IsValidValue(GetEffectiveJavaExe));
  UpdateValidState(ServerJarIndicator, IsValidValue(LintContext.Settings.ServerJar));
  UpdateValidState(SonarDelphiJarIndicator, IsValidValue(LintContext.Settings.SonarDelphiJar));
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.UpdateOkButton;
begin
  OkButton.Enabled := IsAllValid;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.UpdateValidState(Indicator: TPanel; Valid: Boolean);
begin
  if Valid then begin
    Indicator.Color := clGreen;
    Indicator.Font.Color := clWhite;
  end
  else begin
    Indicator.Color := clMaroon;
    Indicator.Font.Color := clWhite;
  end;

  UpdateOkButton;
end;

//______________________________________________________________________________________________________________________

end.
