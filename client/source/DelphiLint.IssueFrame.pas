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
