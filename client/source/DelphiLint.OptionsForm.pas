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
unit DelphiLint.OptionsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls, DelphiLint.ProjectOptions;

type
  TLintOptionsForm = class(TForm)
    SonarHostGroup: TGroupBox;
    SonarHostUrlEdit: TLabeledEdit;
    SonarHostTokenEdit: TLabeledEdit;
    AnalysisGroup: TGroupBox;
    ProjectKeyEdit: TLabeledEdit;
    ProjectBaseDirEdit: TLabeledEdit;
    ProjectNameLabel: TLabel;
    CreateTokenButton: TButton;
    HeaderPanel: TPanel;
    ContentPanel: TPanel;
    FooterPanel: TPanel;
    ProjectBaseDirBrowseButton: TButton;
    PropertiesOpenDialog: TOpenDialog;
    SaveButton: TButton;
    CancelButton: TButton;
    ProjectReadPropertiesCheckBox: TCheckBox;
    AnalysisModeGroup: TRadioGroup;
    AnalysisModeGroupBox: TGroupBox;
    procedure ProjectBaseDirEditChange(Sender: TObject);
    procedure SonarHostUrlEditChange(Sender: TObject);
    procedure ProjectKeyEditChange(Sender: TObject);
    procedure SonarHostTokenEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CreateTokenButtonClick(Sender: TObject);
    procedure ProjectBaseDirBrowseButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ProjectReadPropertiesCheckBoxClick(Sender: TObject);
    procedure AnalysisModeGroupClick(Sender: TObject);
  private
    FProjectOptions: TLintProjectOptions;
    FProjectFile: string;

    function GetCreateTokenUrl(BaseUrl: string): string;
    function IsUrl(Val: string): Boolean;

    function IsConnectedMode: Boolean;

    procedure UpdateControls;
    procedure UpdateCreateTokenButton;
  public
    procedure RefreshOptions;
    procedure RefreshTheme;
  end;

implementation

{$R *.dfm}

uses
    DelphiLint.Utils
  , System.IOUtils
  , Winapi.ShellAPI
  , System.StrUtils
  , Vcl.Themes
  , ToolsAPI
  , System.Math
  ;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.FormCreate(Sender: TObject);
begin
  RefreshOptions;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.UpdateControls;
var
  ProjectName: string;
begin
  AnalysisModeGroup.ItemIndex := IfThen(IsConnectedMode, 1, 0);

  if IsConnectedMode then begin
    AnalysisModeGroup.ItemIndex := 1;
    SonarHostUrlEdit.Enabled := True;
    ProjectKeyEdit.Enabled := True;
    SonarHostTokenEdit.Enabled := True;
    SonarHostGroup.Enabled := True;
  end
  else begin
    AnalysisModeGroup.ItemIndex := 0;
    SonarHostUrlEdit.Enabled := False;
    ProjectKeyEdit.Enabled := False;
    SonarHostTokenEdit.Enabled := False;
    SonarHostGroup.Enabled := False;
  end;

  if Assigned(FProjectOptions) then begin
    SonarHostUrlEdit.Text := FProjectOptions.SonarHostUrl;
    SonarHostTokenEdit.Text := FProjectOptions.SonarHostToken;
    ProjectKeyEdit.Text := FProjectOptions.ProjectKey;
    ProjectBaseDirEdit.Text := FProjectOptions.ProjectBaseDir;
    ProjectReadPropertiesCheckBox.Checked := FProjectOptions.ProjectReadProperties;
    SaveButton.Enabled := True;

    ProjectName := TPath.GetFileName(FProjectFile);
    ProjectNameLabel.Caption := 'DelphiLint: ' + ProjectName;
    Caption := 'DelphiLint Project Options - ' + ProjectName;
  end
  else begin
    SonarHostUrlEdit.Text := '';
    SonarHostTokenEdit.Text := '';
    ProjectKeyEdit.Text := '';
    ProjectBaseDirEdit.Text := '';
    ProjectReadPropertiesCheckBox.Checked := False;
    SaveButton.Enabled := False;

    ProjectNameLabel.Caption := 'DelphiLint: (no project)';
    Caption := 'DelphiLint Project Options (no project)';
  end;

  UpdateCreateTokenButton;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.RefreshOptions;
var
  ProjectFile: string;
begin

  if TryGetProjectFile(ProjectFile) then begin
    if not Assigned(FProjectOptions) or (FProjectFile <> ProjectFile) then begin
      FreeAndNil(FProjectOptions);
      FProjectOptions := TLintProjectOptions.Create(ProjectFile);
      FProjectFile := ProjectFile;
    end;
    FProjectOptions.Load;
  end
  else begin
    FreeAndNil(FProjectOptions);
    FProjectFile := '';
  end;

  UpdateControls;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.RefreshTheme;
var
  WindowColor: TColor;
begin
  (BorlandIDEServices as IOTAIDEThemingServices).ApplyTheme(Self);
  WindowColor := StyleServices(Self).GetSystemColor(clWindow);
  ContentPanel.Color := WindowColor;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.FormShow(Sender: TObject);
begin
  RefreshOptions;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.SaveButtonClick(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.Save;
    Close;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.ProjectBaseDirEditChange(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.ProjectBaseDir := ProjectBaseDirEdit.Text;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.SonarHostTokenEditChange(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.SonarHostToken := SonarHostTokenEdit.Text;
    UpdateCreateTokenButton;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.SonarHostUrlEditChange(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.SonarHostUrl := SonarHostUrlEdit.Text;
    UpdateCreateTokenButton;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.AnalysisModeGroupClick(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.AnalysisConnectedMode := (AnalysisModeGroup.ItemIndex = 1);
    UpdateControls;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.ProjectKeyEditChange(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.ProjectKey := ProjectKeyEdit.Text;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.ProjectReadPropertiesCheckBoxClick(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.ProjectReadProperties := ProjectReadPropertiesCheckBox.Checked;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.ProjectBaseDirBrowseButtonClick(Sender: TObject);
begin
  PropertiesOpenDialog.InitialDir := ExtractFilePath(ProjectBaseDirEdit.Text);
  PropertiesOpenDialog.FileName := '';
  if PropertiesOpenDialog.Execute then begin
    ProjectBaseDirEdit.Text := PropertiesOpenDialog.FileName;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.CreateTokenButtonClick(Sender: TObject);
var
  Url: string;
begin
  if Assigned(FProjectOptions) then begin
    Url := FProjectOptions.SonarHostUrl;

    if IsUrl(Url) then begin
      Url := GetCreateTokenUrl(Url);
      ShellExecute(Handle, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL);
    end;
  end;
end;

//______________________________________________________________________________________________________________________

function TLintOptionsForm.GetCreateTokenUrl(BaseUrl: string): string;
begin
  if not EndsStr('/', BaseUrl) then begin
    BaseUrl := BaseUrl + '/';
  end;

  Result := BaseUrl + 'account/security';
end;

//______________________________________________________________________________________________________________________

function TLintOptionsForm.IsConnectedMode: Boolean;
begin
  Result := Assigned(FProjectOptions) and FProjectOptions.AnalysisConnectedMode;
end;

//______________________________________________________________________________________________________________________

function TLintOptionsForm.IsUrl(Val: string): Boolean;
begin
  Result := StartsText('http://', Val) or StartsText('https://', Val);
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.UpdateCreateTokenButton;
begin
  CreateTokenButton.Enabled :=
    IsConnectedMode
    and (FProjectOptions.SonarHostToken = '')
    and IsUrl(FProjectOptions.SonarHostUrl);
end;

//______________________________________________________________________________________________________________________

end.
