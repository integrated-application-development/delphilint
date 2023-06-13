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
unit DelphiLint.IssueFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DelphiLint.Context, Vcl.ExtCtrls;

type
  TLintIssueFrame = class(TFrame)
    IssueMessageLabel: TLabel;
    IssueLineLabel: TLabel;
    IssueKeyLabel: TLabel;
    LintIssuePanel: TPanel;
  public
    constructor Create(Owner: TComponent; Issue: TLiveIssue); reintroduce;
  end;

implementation

{$R *.dfm}

{ TLintIssueFrame }

constructor TLintIssueFrame.Create(Owner: TComponent; Issue: TLiveIssue);
begin
  inherited Create(Owner);
  IssueMessageLabel.Caption := Issue.Message;
  IssueLineLabel.Caption := Format('%d:%d', [Issue.StartLine, Issue.StartLineOffset]);
end;

end.
