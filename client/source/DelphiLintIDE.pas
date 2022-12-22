unit DelphiLintIDE;

interface

uses
    System.SysUtils
  , ToolsAPI
  , DelphiLintServer
  , Vcl.Dialogs
  ;

type

//______________________________________________________________________________________________________________________

  TDelphiLintIDE = class(TObject)
  private
    FServer: TDelphiLintServer;
  public
    constructor Create;
    destructor Destroy; override;

    property Server: TDelphiLintServer read FServer;
  end;

//______________________________________________________________________________________________________________________
  
  TDelphiLintMenuItem = class(TNotifierObject, IOTAWizard, IOTAMenuWizard)  
  public type
    TMenuItemAction = reference to procedure;
  private
    FName: String;
    FCaption: String;
    FAction: TMenuItemAction;
  public
    constructor Create(Name: String; Caption: String; Action: TMenuItemAction);
  
    function GetIDString: String;
    function GetName: String;
    function GetState: TWizardState;
    procedure Execute;
    function GetMenuText: String;
  end;

//______________________________________________________________________________________________________________________

procedure Register;

function DelphiLintIDE: TDelphiLintIDE;

implementation

uses
    System.StrUtils
  ;

var
  G_DelphiLintIDE: TDelphiLintIDE;

//______________________________________________________________________________________________________________________

procedure Register;
begin
  RegisterPackageWizard(TDelphiLintMenuItem.Create(
    'analyze',
    'Analyze Active File with DelphiLint',
    procedure begin
      ShowMessage(IfThen(DelphiLintIDE.Server.Initialize, 'Server initialized', 'Server not initialized'));
    end
  ));
end;

//______________________________________________________________________________________________________________________

constructor TDelphiLintIDE.Create;
begin
  inherited;
  FServer := TDelphiLintServer.Create('{URL REMOVED}');
end;

//______________________________________________________________________________________________________________________

destructor TDelphiLintIDE.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

//______________________________________________________________________________________________________________________

function DelphiLintIDE: TDelphiLintIDE;
begin
  if not Assigned(G_DelphiLintIDE) then begin
    G_DelphiLintIDE := TDelphiLintIDE.Create;
  end;

  Result := G_DelphiLintIDE;
end;

//______________________________________________________________________________________________________________________

constructor TDelphiLintMenuItem.Create(Name: String; Caption: String; Action: TMenuItemAction);
begin
  FName := Name;
  FCaption := Caption;
  FAction := Action;
end;

//______________________________________________________________________________________________________________________

procedure TDelphiLintMenuItem.Execute;
begin
  FAction;
end;

//______________________________________________________________________________________________________________________

function TDelphiLintMenuItem.GetIDString: String;
begin
  Result := 'DelphiLint|' + FName;
end;

//______________________________________________________________________________________________________________________

function TDelphiLintMenuItem.GetMenuText: String;
begin
  Result := FCaption;
end;

//______________________________________________________________________________________________________________________

function TDelphiLintMenuItem.GetName: String;
begin
  Result := FName;
end;

//______________________________________________________________________________________________________________________

function TDelphiLintMenuItem.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//______________________________________________________________________________________________________________________

initialization

finalization
  FreeAndNil(G_DelphiLintIDE);

end.
