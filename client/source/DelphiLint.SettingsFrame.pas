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
    System.Classes
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.StdCtrls
  , DelphiLint.IDEBaseTypes
  , Vcl.ExtCtrls
  , Data.DB
  , Vcl.Grids
  , Vcl.DBGrids
  , Vcl.DBCtrls
  , Datasnap.DBClient
  ;

type
  TLintSettingsFrame = class(TFrame)
    ComponentsGroupBox: TGroupBox;
    ComponentsButton: TButton;
    BrokenSetupWarningLabel: TLabel;
    ClientConfigGroupBox: TGroupBox;
    ClientAutoShowToolWindowCheckBox: TCheckBox;
    ClientSaveBeforeAnalysisCheckBox: TCheckBox;
    TokensGroupBox: TGroupBox;
    TokensGrid: TDBGrid;
    TokensDataSource: TDataSource;
    TokensDataSet: TClientDataSet;
    DBNavigator1: TDBNavigator;
    ServerConfigGroupBox: TGroupBox;
    SonarDelphiVersionComboBox: TComboBox;
    SonarDelphiVersionRadioGroup: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    procedure ComponentsButtonClick(Sender: TObject);
    procedure SonarDelphiVersionRadioGroupClick(Sender: TObject);
  private
    FRetrievedReleases: Boolean;
    procedure RetrieveReleases;
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
    System.SysUtils
  , System.Generics.Collections
  , System.Net.HttpClient
  , System.JSON
  , System.StrUtils
  , DelphiLint.SetupForm
  , DelphiLint.Context
  , DelphiLint.Settings
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
var
  Ident: TSonarProjectIdentifier;
  SonarDelphiVersion: string;
begin
  LintContext.Settings.Load;
  BrokenSetupWarningLabel.Visible := not TLintSetupForm.IsSetupValid;
  ClientAutoShowToolWindowCheckBox.Checked := LintContext.Settings.ClientAutoShowToolWindow;
  ClientSaveBeforeAnalysisCheckBox.Checked := LintContext.Settings.ClientSaveBeforeAnalysis;

  SonarDelphiVersionRadioGroup.Items[0] := Format(
    'Use default SonarDelphi version (%s)',
    [LintContext.Settings.DefaultSonarDelphiVersion]);
  if LintContext.Settings.ServerSonarDelphiVersionOverride <> '' then begin
    SonarDelphiVersionRadioGroup.ItemIndex := 1;
  end
  else begin
    SonarDelphiVersionRadioGroup.ItemIndex := 0;
  end;

  TokensDataSet.CreateDataSet;
  TokensDataSet.Open;

  TokensDataSet.DisableControls;
  try
    TokensDataSet.EmptyDataSet;

    for Ident in LintContext.Settings.SonarHostTokensMap.Keys do begin
      TokensDataSet.Append;
      TokensDataSet.FieldByName('ServerURL').AsString := Ident.Host;
      TokensDataSet.FieldByName('ProjectKey').AsString := Ident.ProjectKey;
      TokensDataSet.FieldByName('Token').AsString := LintContext.Settings.SonarHostTokensMap[Ident];
      TokensDataSet.Post;
    end;

    TokensDataSet.First;
  finally
    TokensDataSet.EnableControls;
  end;

  TokensDataSet.IndexFieldNames := 'ServerURL;ProjectKey';

  RetrieveReleases;
  SonarDelphiVersionComboBox.Enabled := (SonarDelphiVersionRadioGroup.ItemIndex = 1) and FRetrievedReleases;
  if FRetrievedReleases then begin
    SonarDelphiVersion := LintContext.Settings.ServerSonarDelphiVersionOverride;
    if SonarDelphiVersionRadioGroup.ItemIndex = 1 then begin
      SonarDelphiVersionComboBox.ItemIndex := SonarDelphiVersionComboBox.Items.IndexOf(SonarDelphiVersion);
      SonarDelphiVersionComboBox.Text := SonarDelphiVersion;
    end
    else begin
      if SonarDelphiVersionComboBox.Items.Count <> 0 then begin
        SonarDelphiVersionComboBox.ItemIndex := 0;
      end;
    end;
  end
  else begin
    SonarDelphiVersionComboBox.Text := 'Could not retrieve releases';
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettingsFrame.RetrieveReleases;
var
  Http: THTTPClient;
  Response: IHTTPResponse;
  Json: TJSONArray;
  ReleaseJson: TJSONValue;
  ReleaseVersion: string;
begin
  FRetrievedReleases := False;

  Http := THTTPClient.Create;
  try
    Response := Http.Get('https://api.github.com/repos/integrated-application-development/sonar-delphi/releases');
    if Response.StatusCode = 200 then begin
      Json := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONArray;
      try
        for ReleaseJson in Json do begin
          ReleaseVersion := ReleaseJson.GetValue<string>('tag_name');

          if StartsStr('v', ReleaseVersion) and (Length(ReleaseVersion) > 1) then begin
            ReleaseVersion := Copy(ReleaseVersion, 2);
          end;

          SonarDelphiVersionComboBox.Items.Add(ReleaseVersion);
        end;
      finally
        FreeAndNil(Json);
      end;

      FRetrievedReleases := True;
    end;
  finally
    FreeAndNil(Http);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettingsFrame.Save;
var
  Tokens: TDictionary<TSonarProjectIdentifier, string>;
begin
  Tokens := LintContext.Settings.SonarHostTokensMap;

  TokensDataSet.DisableControls;
  try
    Tokens.Clear;

    TokensDataSet.First;
    while not TokensDataSet.Eof do begin
      if (TokensDataSet.FieldByName('ServerURL').AsString <> '') then begin
        Tokens.AddOrSetValue(
          TSonarProjectIdentifier.Create(
            TokensDataSet.FieldByName('ServerURL').AsString,
            TokensDataSet.FieldByName('ProjectKey').AsString),
          TokensDataSet.FieldByName('Token').AsString);
      end;

      TokensDataSet.Next;
    end;
  finally
    TokensDataSet.EnableControls;
  end;

  LintContext.Settings.ClientAutoShowToolWindow := ClientAutoShowToolWindowCheckBox.Checked;
  LintContext.Settings.ClientSaveBeforeAnalysis := ClientSaveBeforeAnalysisCheckBox.Checked;
  LintContext.Settings.ServerSonarDelphiVersionOverride := IfThen(
    (SonarDelphiVersionRadioGroup.ItemIndex = 1) and (SonarDelphiVersionComboBox.ItemIndex <> -1),
    SonarDelphiVersionComboBox.Items[SonarDelphiVersionComboBox.ItemIndex],
    ''
  );
  LintContext.Settings.Save;
end;

//______________________________________________________________________________________________________________________

procedure TLintSettingsFrame.SonarDelphiVersionRadioGroupClick(Sender: TObject);
begin
  SonarDelphiVersionComboBox.Enabled := (SonarDelphiVersionRadioGroup.ItemIndex = 1) and FRetrievedReleases;
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
