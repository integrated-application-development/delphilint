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
unit DelphiLint.PopupHook;

interface

uses
    System.Classes
  , Vcl.Menus
  , DelphiLint.Events
  ;

type
  TEditorPopupMenuHook = class(TComponent)
  private
    FMenu: TPopupMenu;
    FBaseOnPopup: TNotifyEvent;
    FOnFreed: TEventNotifier<TEditorPopupMenuHook>;

    procedure PopulatePopup(Sender: TObject);
    procedure ClearTaggedItems;
    procedure TagItem(Item: TMenuItem);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    property OnFreed: TEventNotifier<TEditorPopupMenuHook> read FOnFreed;
  end;

implementation

uses
    System.SysUtils
  , DelphiLint.Context
  , DelphiLint.LiveData
  , DelphiLint.IssueActions
  ;

const
  CDelphiLintMenuItemTag = 856248;

//______________________________________________________________________________________________________________________

procedure TEditorPopupMenuHook.ClearTaggedItems;
var
  I: Integer;
begin
  for I := FMenu.Items.Count - 1 downto 0 do begin
    if FMenu.Items[I].Tag = CDelphiLintMenuItemTag then begin
      FMenu.Items[I].Free;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

constructor TEditorPopupMenuHook.Create(Owner: TComponent);
begin
  inherited;

  Assert(Owner is TPopupMenu, 'Popup menu hook owner is not a TPopupMenu');
  FMenu := TPopupMenu(Owner);

  FBaseOnPopup := FMenu.OnPopup;
  FMenu.OnPopup := PopulatePopup;

  FOnFreed := TEventNotifier<TEditorPopupMenuHook>.Create;
end;

//______________________________________________________________________________________________________________________

destructor TEditorPopupMenuHook.Destroy;
begin
  FOnFreed.Notify(Self);
  FreeAndNil(FOnFreed);
  ClearTaggedItems;
  FMenu.OnPopup := FBaseOnPopup;
  inherited;
end;

//______________________________________________________________________________________________________________________

procedure TEditorPopupMenuHook.PopulatePopup(Sender: TObject);

  function Separator(Owner: TComponent): TMenuItem;
  begin
    Result := TMenuItem.Create(Owner);
    Result.Name := 'DelphiLintSeparator';
    Result.Caption := '-';
    TagItem(Result);
  end;

var
  TopView: IIDEEditView;
  Issues: TArray<ILiveIssue>;
  Issue: ILiveIssue;
  MenuItemFactory: TIssueMenuItemFactory;
  MenuItem: TMenuItem;
  SeparatorAdded: Boolean;
begin
  if Assigned(FBaseOnPopup) then begin
    FBaseOnPopup(Sender);
  end;

  ClearTaggedItems;

  if not ContextValid then begin
    Exit;
  end;

  SeparatorAdded := False;
  TopView := LintContext.IDEServices.GetTopView;
  if Assigned(TopView) then begin
    Issues := Analyzer.GetIssues(TopView.FileName, TopView.Row, TopView.Column);

    for Issue in Issues do begin
      if not SeparatorAdded then begin
        FMenu.Items.Add(Separator(FMenu));
        SeparatorAdded := True;
      end;

      MenuItemFactory := TIssueMenuItemFactory.Create(Issue);
      try
        // Apply quick fix
        MenuItem := MenuItemFactory.ApplyQuickFix(FMenu);
        if Assigned(MenuItem) then begin
          TagItem(MenuItem);
          FMenu.Items.Add(MenuItem);
        end;
      finally
        FreeAndNil(MenuItemFactory);
      end;
    end;
  end;
end;

//______________________________________________________________________________________________________________________

procedure TEditorPopupMenuHook.TagItem(Item: TMenuItem);
begin
  Item.Tag := CDelphiLintMenuItemTag;
end;

end.
