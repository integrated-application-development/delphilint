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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask;

type
  TLintSetupForm = class(TForm)
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    JavaExeEdit: TLabeledEdit;
    JavaExeIndicator: TShape;
    ServerJarIndicator: TShape;
    ServerJarEdit: TLabeledEdit;
    SonarDelphiJarEdit: TLabeledEdit;
    SonarDelphiJarIndicator: TShape;
    JavaExeBrowseButton: TButton;
    ServerJarBrowseButton: TButton;
    SonarDelphiJarBrowseButton: TButton;
    OkButton: TButton;
    ExeOpenDialog: TOpenDialog;
    JarOpenDialog: TOpenDialog;
    Label4: TLabel;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    Label5: TLabel;
    LinkLabel1: TLinkLabel;
    procedure FormCreate(Sender: TObject);
    procedure JavaExeEditChange(Sender: TObject);
    procedure ServerJarEditChange(Sender: TObject);
    procedure SonarDelphiJarEditChange(Sender: TObject);
    procedure JarBrowseButtonClick(Sender: TObject);
    procedure ExeBrowseButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSaved: Boolean;

    procedure UpdateControls;
    procedure UpdateValidState(Edit: TCustomEdit; Indicator: TShape);
    procedure UpdateOkButton;
    function IsValidValue(Value: string): Boolean;
    function IsAllValid: Boolean;
  public
    procedure RefreshTheme;
  end;

implementation

{$R *.dfm}

uses
    DelphiLint.Settings
  , DelphiLint.Plugin
  , ToolsAPI
  , Vcl.Themes
  ;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.FormCreate(Sender: TObject);
begin
  FSaved := False;
  LintSettings.Load;
  UpdateControls;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.OkButtonClick(Sender: TObject);
begin
  LintSettings.ServerJavaExe := JavaExeEdit.Text;
  LintSettings.ServerJar := ServerJarEdit.Text;
  LintSettings.SonarDelphiJar := SonarDelphiJarEdit.Text;
  LintSettings.Save;

  FSaved := True;
  Plugin.PluginEnabled := True;
  Close;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.RefreshTheme;
var
  WindowColor: TColor;
begin
  (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(Self);
  WindowColor := StyleServices(Self).GetSystemColor(clWindow);
  RightPanel.Color := WindowColor;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
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
  Result := IsValidValue(JavaExeEdit.Text)
    and IsValidValue(ServerJarEdit.Text)
    and IsValidValue(SonarDelphiJarEdit.Text);
end;

//______________________________________________________________________________________________________________________

function TLintSetupForm.IsValidValue(Value: string): Boolean;
begin
  Result := FileExists(Value);
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.ExeBrowseButtonClick(Sender: TObject);
var
  Edit: TCustomEdit;
begin
  Edit := TCustomEdit(Sender);

  ExeOpenDialog.InitialDir := ExtractFilePath(Edit.Text);
  ExeOpenDialog.FileName := '';
  if ExeOpenDialog.Execute then begin
    Edit.Text := ExeOpenDialog.FileName;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.JarBrowseButtonClick(Sender: TObject);
var
  Edit: TCustomEdit;
begin
  Edit := TCustomEdit(Sender);

  JarOpenDialog.InitialDir := ExtractFilePath(Edit.Text);
  JarOpenDialog.FileName := '';
  if JarOpenDialog.Execute then begin
    Edit.Text := JarOpenDialog.FileName;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.JavaExeEditChange(Sender: TObject);
begin
  UpdateValidState(JavaExeEdit, JavaExeIndicator);
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.ServerJarEditChange(Sender: TObject);
begin
  UpdateValidState(ServerJarEdit, ServerJarIndicator);
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.SonarDelphiJarEditChange(Sender: TObject);
begin
  UpdateValidState(SonarDelphiJarEdit, SonarDelphiJarIndicator);
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.UpdateControls;
begin
  JavaExeEdit.Text := LintSettings.ServerJavaExe;
  ServerJarEdit.Text := LintSettings.ServerJar;
  SonarDelphiJarEdit.Text := LintSettings.SonarDelphiJar;
  UpdateValidState(JavaExeEdit, JavaExeIndicator);
  UpdateValidState(ServerJarEdit, ServerJarIndicator);
  UpdateValidState(SonarDelphiJarEdit, SonarDelphiJarIndicator);
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.UpdateOkButton;
begin
  OkButton.Enabled := IsAllValid;
end;

//______________________________________________________________________________________________________________________

procedure TLintSetupForm.UpdateValidState(Edit: TCustomEdit; Indicator: TShape);
begin
  if IsValidValue(Edit.Text) then begin
    Indicator.Brush.Color := clLime;
  end
  else begin
    Indicator.Brush.Color := clRed;
  end;

  UpdateOkButton;
end;

//______________________________________________________________________________________________________________________

end.
