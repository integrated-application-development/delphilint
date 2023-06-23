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
    ServerPortEdit: TLabeledEdit;
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
  LintSettings.Reload;
  ServerJarEdit.Text := LintSettings.ServerJar;
  ServerPortEdit.Text := IntToStr(LintSettings.ServerPort);
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
  LintSettings.ServerPort := StrToInt(ServerPortEdit.Text);
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
    and FileExists(ServerJavaExeEdit.Text)
    and (StrToInt(ServerPortEdit.Text) > 0);
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
