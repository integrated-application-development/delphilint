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
    System.SysUtils
  , System.Generics.Collections
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
begin
  LintContext.Settings.Load;
  BrokenSetupWarningLabel.Visible := not TLintSetupForm.IsSetupValid;
  ClientAutoShowToolWindowCheckBox.Checked := LintContext.Settings.ClientAutoShowToolWindow;
  ClientSaveBeforeAnalysisCheckBox.Checked := LintContext.Settings.ClientSaveBeforeAnalysis;

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
  LintContext.Settings.Save;
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
