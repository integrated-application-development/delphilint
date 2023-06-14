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
  end;

implementation

{$R *.dfm}

end.
