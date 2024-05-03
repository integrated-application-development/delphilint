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
unit DelphiLint.IssueActions;

interface

uses
    Vcl.Menus
  , System.Classes
  , DelphiLint.LiveData
  ;

type
  TIssueMenuItemFactory = class(TObject)
  private
    FIssue: ILiveIssue;

  public
    constructor Create(Issue: ILiveIssue);
    destructor Destroy; override;

    function HideIssue(Owner: TComponent): TMenuItem;
  end;

  TIssueLinkedMenuItem = class abstract(TMenuItem)
  private
    FIssue: ILiveIssue;
  protected
    procedure DoOnClick(Sender: TObject); virtual; abstract;
  public
    constructor CreateLinked(AOwner: TComponent; Issue: ILiveIssue);

    property LinkedIssue: ILiveIssue read FIssue;
  end;

  TUntetherMenuItem = class(TIssueLinkedMenuItem)
  protected
    procedure DoOnClick(Sender: TObject); override;
  end;

implementation

//______________________________________________________________________________________________________________________

constructor TIssueMenuItemFactory.Create(Issue: ILiveIssue);
begin
  inherited Create;
  FIssue := Issue;
end;

//______________________________________________________________________________________________________________________

destructor TIssueMenuItemFactory.Destroy;
begin
  FIssue := nil;
  inherited;
end;

//______________________________________________________________________________________________________________________

function TIssueMenuItemFactory.HideIssue(Owner: TComponent): TMenuItem;
begin
  Result := TUntetherMenuItem.CreateLinked(Owner, FIssue);
  Result.Caption := 'Hide';
  Result.Enabled := FIssue.IsTethered;
end;

//______________________________________________________________________________________________________________________

constructor TIssueLinkedMenuItem.CreateLinked(AOwner: TComponent; Issue: ILiveIssue);
begin
  inherited Create(AOwner);

  FIssue := Issue;
  OnClick := DoOnClick;
end;

//______________________________________________________________________________________________________________________

procedure TUntetherMenuItem.DoOnClick(Sender: TObject);
begin
  LinkedIssue.Untether;
end;

//______________________________________________________________________________________________________________________

end.
