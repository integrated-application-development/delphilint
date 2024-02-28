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
    System.Classes
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.Mask
  , Vcl.ExtCtrls
  , DelphiLint.ProjectOptions
  ;

type
  TLintOptionsForm = class(TForm)
    SonarHostGroup: TGroupBox;
    SonarHostUrlEdit: TLabeledEdit;
    SonarHostTokenEdit: TLabeledEdit;
    AnalysisGroup: TGroupBox;
    SonarHostProjectKeyEdit: TLabeledEdit;
    AnalysisBaseDirEdit: TLabeledEdit;
    ProjectNameLabel: TLabel;
    ManageTokensButton: TButton;
    HeaderPanel: TPanel;
    ContentPanel: TPanel;
    FooterPanel: TPanel;
    ProjectBaseDirBrowseButton: TButton;
    SaveButton: TButton;
    CancelButton: TButton;
    AnalysisReadPropertiesCheckBox: TCheckBox;
    AnalysisModeGroup: TRadioGroup;
    AnalysisModeGroupBox: TGroupBox;
    BaseDirDialog: TFileOpenDialog;
    SonarHostDownloadPluginCheckBox: TCheckBox;
    procedure AnalysisBaseDirEditChange(Sender: TObject);
    procedure SonarHostUrlEditChange(Sender: TObject);
    procedure SonarHostProjectKeyEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ManageTokensButtonClick(Sender: TObject);
    procedure ProjectBaseDirBrowseButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure AnalysisReadPropertiesCheckBoxClick(Sender: TObject);
    procedure AnalysisModeGroupClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SonarHostDownloadPluginCheckBoxClick(Sender: TObject);
  private
    FProjectOptions: TLintProjectOptions;
    FProjectFile: string;

    function IsUrl(Val: string): Boolean;

    function IsConnectedMode: Boolean;

    procedure UpdateControls;
    procedure UpdateTokenInfo;
  public
    procedure RefreshOptions;
    procedure RefreshTheme;
  end;

implementation

{$R *.dfm}

uses
    System.SysUtils
  , System.StrUtils
  , System.IOUtils
  , System.Math
  , Vcl.Themes
  , Vcl.Graphics
  , DelphiLint.Utils
  , DelphiLint.Context
  ;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.FormCreate(Sender: TObject);
begin
  RefreshOptions;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProjectOptions);
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
    SonarHostProjectKeyEdit.Enabled := True;
    SonarHostDownloadPluginCheckBox.Enabled := True;
    SonarHostGroup.Enabled := True;
  end
  else begin
    AnalysisModeGroup.ItemIndex := 0;
    SonarHostUrlEdit.Enabled := False;
    SonarHostProjectKeyEdit.Enabled := False;
    SonarHostDownloadPluginCheckBox.Enabled := False;
    SonarHostGroup.Enabled := False;
  end;

  if Assigned(FProjectOptions) then begin
    SonarHostUrlEdit.Text := FProjectOptions.SonarHostUrl;
    SonarHostProjectKeyEdit.Text := FProjectOptions.SonarHostProjectKey;
    SonarHostDownloadPluginCheckBox.Checked := FProjectOptions.SonarHostDownloadPlugin;
    AnalysisBaseDirEdit.Text := FProjectOptions.AnalysisBaseDir;
    AnalysisReadPropertiesCheckBox.Checked := FProjectOptions.AnalysisReadProperties;
    SaveButton.Enabled := True;

    ProjectName := TPath.GetFileName(FProjectFile);
    ProjectNameLabel.Caption := 'DelphiLint: ' + ProjectName;
    Caption := 'DelphiLint Project Options - ' + ProjectName;
  end
  else begin
    SonarHostUrlEdit.Text := '';
    SonarHostProjectKeyEdit.Text := '';
    SonarHostDownloadPluginCheckBox.Checked := False;
    AnalysisBaseDirEdit.Text := '';
    AnalysisReadPropertiesCheckBox.Checked := False;
    SaveButton.Enabled := False;

    ProjectNameLabel.Caption := 'DelphiLint: (no project)';
    Caption := 'DelphiLint Project Options (no project)';
  end;

  UpdateTokenInfo;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.RefreshOptions;
var
  ProjectFile: string;
begin

  if TryGetProjectFile(ProjectFile) then begin
    if not Assigned(FProjectOptions) or (FProjectFile <> ProjectFile) then begin
      FreeAndNil(FProjectOptions);
      FProjectOptions := LintContext.GetProjectOptions(ProjectFile);
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
  LintContext.IDEServices.ApplyTheme(Self);
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

procedure TLintOptionsForm.AnalysisBaseDirEditChange(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.AnalysisBaseDir := AnalysisBaseDirEdit.Text;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.SonarHostDownloadPluginCheckBoxClick(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.SonarHostDownloadPlugin := SonarHostDownloadPluginCheckBox.Checked;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.SonarHostUrlEditChange(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.SonarHostUrl := SonarHostUrlEdit.Text;
    UpdateTokenInfo;
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

procedure TLintOptionsForm.SonarHostProjectKeyEditChange(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.SonarHostProjectKey := SonarHostProjectKeyEdit.Text;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.AnalysisReadPropertiesCheckBoxClick(Sender: TObject);
begin
  if Assigned(FProjectOptions) then begin
    FProjectOptions.AnalysisReadProperties := AnalysisReadPropertiesCheckBox.Checked;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.ProjectBaseDirBrowseButtonClick(Sender: TObject);
begin
  BaseDirDialog.DefaultFolder := ExtractFilePath(AnalysisBaseDirEdit.Text);
  BaseDirDialog.FileName := '';
  if BaseDirDialog.Execute then begin
    AnalysisBaseDirEdit.Text := BaseDirDialog.FileName;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintOptionsForm.ManageTokensButtonClick(Sender: TObject);
begin
  LintContext.IDEServices.EditOptions('', 'DelphiLint');
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

procedure TLintOptionsForm.UpdateTokenInfo;
var
  TokenText: string;
begin
  TokenText := LintContext.Settings.GetSonarHostToken(
    SonarHostUrlEdit.Text,
    SonarHostProjectKeyEdit.Text);

  SonarHostTokenEdit.Text := IfThen(TokenText = '', '(no token)', TokenText);

  ManageTokensButton.Enabled :=
    IsConnectedMode
    and (FProjectOptions.SonarHostProjectKey <> '')
    and IsUrl(FProjectOptions.SonarHostUrl);
end;

//______________________________________________________________________________________________________________________

end.
