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
unit DelphiLint.ToolFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, DockForm, Vcl.Menus,
  Vcl.Buttons;

type
  TLintToolFrame = class(TFrame)
    LintPanel: TPanel;
    ProgLabel: TLabel;
    ProgBar: TProgressBar;
    FileNameLabel: TLabel;
    LintButton: TBitBtn;
    ProgImage: TImage;
    IssueListBox: TListBox;
    LintButtonPanel: TPanel;
    RulePanel: TPanel;
    RuleNameLabel: TLabel;
    RuleTypeLabel: TLabel;
    RuleDescLabel: TLabel;
    ContentPanel: TPanel;
    SplitPanel: TPanel;
    RuleHeading: TPanel;
    procedure SplitPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SplitPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FResizing: Boolean;
  public
    constructor Create(Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

constructor TLintToolFrame.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FResizing := False;
end;

procedure TLintToolFrame.SplitPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FResizing := True;
end;

procedure TLintToolFrame.SplitPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NewWidth: Integer;
begin
  if FResizing then begin
    NewWidth := RulePanel.Width - X;

    if (NewWidth < ContentPanel.Width - 10) then begin
      RulePanel.Width := NewWidth;
    end;
  end;
end;

procedure TLintToolFrame.SplitPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FResizing := False;
end;

end.
