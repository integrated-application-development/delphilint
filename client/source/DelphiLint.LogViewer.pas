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
unit DelphiLint.LogViewer;

interface

uses
    Winapi.Windows
  , System.Classes
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.ExtDlgs
  , System.ImageList
  , Vcl.ImgList
  , Vcl.Buttons
  , Vcl.Menus
  ;

type
  TLogViewerForm = class(TForm)
    BottomPanel: TPanel;
    SaveButton: TButton;
    SaveDialog: TSaveTextFileDialog;
    LogListBox: TListBox;
    IconImageList: TImageList;
    ReportButton: TBitBtn;
    Label3: TLabel;
    LogPopup: TPopupMenu;
    CopyItem: TMenuItem;
    SaveItem: TMenuItem;
    TopPanel: TPanel;
    TitleLabel: TLabel;
    procedure SaveButtonClick(Sender: TObject);
    procedure LogListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure LogListBoxMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure ReportButtonClick(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
  private
    FLogList: TStringList;
    FAnalysisTime: TDateTime;

    function SelectedLogText: string;
  public
    constructor Create(Owner: TComponent; Time: TDateTime; LogMessages: TArray<string>); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
    System.StrUtils
  , System.SysUtils
  , DelphiLint.ExternalConsts
  , Winapi.ShellAPI
  , Vcl.Graphics
  , System.IOUtils
  , Vcl.Clipbrd
  ;

{$R *.dfm}

//______________________________________________________________________________________________________________________

function TLogViewerForm.SelectedLogText: string;
var
  Index: Integer;
begin
  Result := '';
  for Index := 0 to LogListBox.Count - 1 do begin
    if LogListBox.Selected[Index] then begin
      Result := Result + LogListBox.Items[Index] + #13#10;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLogViewerForm.CopyItemClick(Sender: TObject);
var
  Text: string;
begin
  Text := SelectedLogText;
  if Text = '' then begin
    Text := FLogList.Text;
  end;

  Clipboard.AsText := SelectedLogText;
end;

//______________________________________________________________________________________________________________________

constructor TLogViewerForm.Create(Owner: TComponent; Time: TDateTime; LogMessages: TArray<string>);
var
  Line: string;
  TimeStr: string;
begin
  inherited Create(Owner);

  FLogList := TStringList.Create;
  for Line in LogMessages do begin
    FLogList.Add(StringReplace(TrimRight(Line), #9, '    ', [rfReplaceAll]));
  end;

  FAnalysisTime := Time;
  TimeStr := FormatDateTime('h:nnam/pm', FAnalysisTime);
  TitleLabel.Caption := Format('Analysis at %s', [TimeStr]);
  Self.Caption := Format('Analysis Log - %s', [TimeStr]);

  LogListBox.Items := FLogList;
end;

//______________________________________________________________________________________________________________________

destructor TLogViewerForm.Destroy;
begin
  FreeAndNil(FLogList);
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TLogViewerForm.LogListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Text: string;
begin
  if Index >= FLogList.Count then begin
    Exit;
  end;

  Text := FLogList[Index];

  if StartsStr('[ERROR]', Text) then begin
    if not (odSelected in State) then begin
      LogListBox.Canvas.Font.Color := clRed;
    end;
  end
  else if StartsStr('[WARN]', Text) then begin
    if not (odSelected in State) then begin
      LogListBox.Canvas.Font.Color := clWebOrangeRed;
    end;
  end;

  LogListBox.Canvas.FillRect(Rect);
  LogListBox.Canvas.TextRect(Rect, Text, [tfLeft]);

  if odFocused in State then begin
    LogListBox.Canvas.DrawFocusRect(Rect);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLogViewerForm.LogListBoxMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
var
  Rect: TRect;
  Text: string;
begin
  if Index >= FLogList.Count then begin
    Exit;
  end;

  Text := FLogList[Index];
  LogListBox.Canvas.Font.Name := LogListBox.Font.Name;
  LogListBox.Canvas.TextRect(Rect, Text, [tfCalcRect]);
  Height := Rect.Height;
end;

//______________________________________________________________________________________________________________________

procedure TLogViewerForm.ReportButtonClick(Sender: TObject);
var
  Url: string;
begin
  Url := DelphiLint.ExternalConsts.CReportSonarDelphiIssueUrl;
  ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL);
end;

//______________________________________________________________________________________________________________________

procedure TLogViewerForm.SaveButtonClick(Sender: TObject);
begin
  SaveDialog.FileName := Format(
    'delphilint_analysis_%s.log',
    [FormatDateTime('yyyymmdd"_"hhnnss', FAnalysisTime)]);
  if SaveDialog.Execute then begin
    FLogList.SaveToFile(SaveDialog.FileName, TEncoding.UTF8);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TLogViewerForm.SaveItemClick(Sender: TObject);
var
  Text: string;
begin
  Text := SelectedLogText;
  if Text = '' then begin
    Text := FLogList.Text;
  end;

  SaveDialog.FileName := Format(
    'delphilint_analysis_partial_%s.log',
    [FormatDateTime('yyyymmdd"_"hhnnss', FAnalysisTime)]);
  if SaveDialog.Execute then begin
    TFile.WriteAllText(SaveDialog.FileName, Text, TEncoding.UTF8);
  end;
end;

//______________________________________________________________________________________________________________________

end.
