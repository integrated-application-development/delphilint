unit DelphiLint.ToolbarManager;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls, System.Actions, Vcl.ActnList, Vcl.Menus,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TLintToolbarManager = class(TDataModule)
    LintImages: TImageList;
    LintActions: TActionList;
    LintPopupMenu: TPopupMenu;
    ActionAnalyze: TAction;

    procedure DataModuleCreate(Sender: TObject);
    procedure ActionAnalyzeExecute(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FToolbar: TToolBar;
    FImageIndexOffset: Integer;
    FProgBar: TProgressBar;
    FProgLabel: TLabel;
    FLintButton: TToolButton;

    procedure ResetToolbar(Toolbar: TToolBar);
    procedure CreateToolbar;
    procedure CreateMenuItem;
    function CreateLintMenu(Owner: TComponent): TPopupMenu;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
    ToolsAPI
  , Vcl.Forms
  ;

function TLintToolbarManager.CreateLintMenu(Owner: TComponent): TPopupMenu;

  procedure CreateItem(Menu: TPopupMenu; Caption: string; Action: TAction);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := Menu.CreateMenuItem;
    MenuItem.Caption := Caption;
    MenuItem.Action := Action;
    Menu.Items.Add(MenuItem);
  end;

begin
  Result := TPopupMenu.Create(Owner);
  CreateItem(Result, 'Project Options', nil);
  CreateItem(Result, 'Settings', nil);
  CreateItem(Result, '-', nil);
  CreateItem(Result, 'Restart Server', nil);
end;

procedure TLintToolbarManager.CreateMenuItem;
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(FToolbar);
  MenuItem.Caption := 'Analyze';
  MenuItem.Action := ActionAnalyze;
  (BorlandIDEServices as INTAServices).AddActionMenu('FileExitItem', ActionAnalyze, MenuItem, False);
end;

procedure TLintToolbarManager.CreateToolbar;
var
  ProgPanel: TPanel;
begin
  FToolbar := (BorlandIDEServices as INTAServices).NewToolbar('DelphiLintToolbar', 'DelphiLint');
  ResetToolbar(FToolbar);
  FToolbar.ShowCaptions := False;
  FToolbar.AutoSize := True;

  ProgPanel := TPanel.Create(FToolbar);
  ProgPanel.Parent := FToolbar;
  ProgPanel.ShowCaption := False;
  ProgPanel.VerticalAlignment := taAlignTop;
  ProgPanel.BorderStyle := bsNone;
  ProgPanel.BevelKind := bkNone;
  ProgPanel.BevelOuter := bvNone;
  ProgPanel.BevelInner := bvNone;
  ProgPanel.Width := 100;
  ProgPanel.Height := 24;
  ProgPanel.ParentBackground := False;

  FLintButton := TToolButton.Create(FToolbar);
  FLintButton.Parent := FToolbar;
  FLintButton.Action := ActionAnalyze;
  FLintButton.ImageIndex := FImageIndexOffset + 0;
  FLintButton.Style := TToolButtonStyle.tbsDropDown;
  FLintButton.DropdownMenu := CreateLintMenu(FToolbar);
  FLintButton.AutoSize := True;

  FProgBar := TProgressBar.Create(ProgPanel);
  FProgBar.Parent := ProgPanel;
  FProgBar.Min := 0;
  FProgBar.Max := 100;
  FProgBar.Step := 1;
  FProgBar.Width := 98;
  FProgBar.Left := 2;
  FProgBar.Top := 16;
  FProgBar.Height := 10;

  FProgLabel := TLabel.Create(ProgPanel);
  FProgLabel.Parent := ProgPanel;
  FProgLabel.Caption := 'Idle';
  FProgLabel.Left := 2;
  FProgLabel.Top := 2;
  FProgLabel.Width := 100;
  FProgLabel.Height := 16;
end;

procedure TLintToolbarManager.DataModuleCreate(Sender: TObject);
var
  NTAServices: INTAServices;
begin
  NTAServices := BorlandIDEServices as INTAServices;
  FImageIndexOffset := NTAServices.AddImages(LintImages);
  CreateToolbar;
  CreateMenuItem;
end;

procedure TLintToolbarManager.DataModuleDestroy(Sender: TObject);
begin
  ResetToolbar(FToolbar);
end;

procedure TLintToolbarManager.ResetToolbar(Toolbar: TToolBar);
var
  Index: Integer;
  Button: TToolButton;
begin
  for Index := Toolbar.ButtonCount - 1 downto 0 do begin
    Button := Toolbar.Buttons[Index];
    Button.Parent := nil;
    FreeAndNil(Button);
  end;
end;

procedure TLintToolbarManager.ActionAnalyzeExecute(Sender: TObject);
begin
  ActionAnalyze.ImageIndex := FImageIndexOffset + 1;
  FToolbar.Repaint;
  FProgBar.StepBy(10);
  FProgLabel.Caption := 'Analyzing';
end;

end.
