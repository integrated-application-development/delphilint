unit DelphiLintTest.Plugin;

interface

uses
    DUnitX.TestFramework
  , DelphiLintTest.MockContext
  ;

type
  [TestFixture]
  TIDEPluginTest = class(TObject)
  private
    procedure MockAllToolBars(IDEServices: TMockIDEServices);
    procedure BuildMockedContext(out IDEServices: TMockIDEServices);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [TestCase]
    procedure TestPluginInfoAddedOnCreate;
    [TestCase]
    procedure TestPluginInfoCleanedUp;
    [TestCase]
    procedure TestEditorHandlerAddedOnInit;
    [TestCase]
    procedure TestEditorHandlerCleanedUp;
    [TestCase]
    procedure TestFormClassesRegisteredOnInit;
    [TestCase]
    procedure TestEnablesOnInitWhenSetupValid;
    [TestCase]
    procedure TestDisablesOnInitWhenSetupInvalid;
    [TestCase('ShowToolWindow', '0,&Show DelphiLint')]
    [TestCase('AnalyzeActiveFile', '2,Analyze &This File')]
    [TestCase('AnalyzeAllOpenFiles', '3,Analyze &All Open Files')]
    [TestCase('ProjectOptions', '5,Project &Options...')]
    [TestCase('Settings', '6,Se&ttings...')]
    [TestCase('RestartServer', '7,&Restart Server')]
    procedure TestMenuItemAdded(ExpectedIndex: Integer; ExpectedCaption: string);
    [TestCase]
    procedure TestActionsAddedToIDEInsight;
    [TestCase]
    procedure TestIDEInsightActionsCleanedUp;
  end;

implementation

uses
    System.SysUtils
  , System.Generics.Collections
  , Vcl.ComCtrls
  , Vcl.Menus
  , DelphiLint.Context
  , DelphiLint.Plugin
  , DelphiLint.Version
  , DelphiLintTest.MockUtils
  , DelphiLint.SetupForm
  , DelphiLint.OptionsForm
  ;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.Setup;
begin
  MockContext.Reset;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TearDown;
begin
  MockContext.Reset;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.BuildMockedContext(out IDEServices: TMockIDEServices);
begin
  MockContext.MockAnalyzer(TMockAnalyzer.Create);
  IDEServices := TMockIDEServices.Create;
  MockContext.MockIDEServices(IDEServices);
  MockAllToolBars(IDEServices);
  MockContext.MockSettings;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.MockAllToolBars(IDEServices: TMockIDEServices);
const
  CToolBars: array of string = [
    'CustomToolBar', 'StandardToolBar', 'DebugToolBar', 'ViewToolBar', 'DesktopToolBar', 'AlignToolbar',
    'BrowserToolbar', 'HTMLDesignToolbar', 'HTMLFormatToolbar', 'HTMLTableToolbar', 'PersonalityToolBar',
    'PositionToolbar', 'SpacingToolbar', 'IDEInsightToolbar', 'PlatformDeviceToolbar'
  ];
var
  ToolBarId: string;
begin
  for ToolBarId in CToolBars do begin
    IDEServices.MockToolBar(ToolBarId, TToolBar.Create(nil));
  end;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TestPluginInfoAddedOnCreate;
var
  Plugin: IPlugin;
  IDEServices: TMockIDEServices;
  PluginInfo: TMockPluginInfo;
begin
  BuildMockedContext(IDEServices);

  Assert.AreEqual(0, IDEServices.IDE.PluginInfos.Count);
  Plugin := TIDEPlugin.Create(LintContext.IDEServices);
  Assert.AreEqual(1, IDEServices.IDE.PluginInfos.Count);

  PluginInfo := IDEServices.IDE.PluginInfos[0];
  Assert.AreEqual(Format('DelphiLint %s', [DelphiLintVersion]), PluginInfo.Title);
  Assert.StartsWith('Free and open source Delphi code linter', PluginInfo.Description);
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TestEnablesOnInitWhenSetupValid;
var
  Plugin: IPlugin;
  IDEServices: TMockIDEServices;
begin
  BuildMockedContext(IDEServices);

  Plugin := TIDEPlugin.Create(LintContext.IDEServices);
  Plugin.Init;
  try
    Assert.IsTrue(Plugin.PluginEnabled);
  finally
    Plugin.Deinit(IDEServices);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TestDisablesOnInitWhenSetupInvalid;
var
  Plugin: IPlugin;
  IDEServices: TMockIDEServices;
begin
  BuildMockedContext(IDEServices);
  MockContext.MockInvalidSetup;

  Plugin := TIDEPlugin.Create(LintContext.IDEServices);
  Plugin.Init;
  try
    Assert.IsFalse(Plugin.PluginEnabled);
  finally
    Plugin.Deinit(IDEServices);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TestEditorHandlerAddedOnInit;
var
  Plugin: IPlugin;
  IDEServices: TMockIDEServices;
begin
  BuildMockedContext(IDEServices);

  Plugin := TIDEPlugin.Create(LintContext.IDEServices);
  Assert.AreEqual(0, IDEServices.IDE.EditorNotifiers.Count);
  Plugin.Init;
  try
    Assert.AreEqual(1, IDEServices.IDE.EditorNotifiers.Count);
  finally
    Plugin.Deinit(IDEServices);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TestFormClassesRegisteredOnInit;
var
  Plugin: IPlugin;
  IDEServices: TMockIDEServices;
  ClassesToRegister: TList<TClass>;
begin
  BuildMockedContext(IDEServices);

  ClassesToRegister := TList<TClass>.Create;
  try
    ClassesToRegister.Add(TLintOptionsForm);
    ClassesToRegister.Add(TLintSetupForm);

    IDEServices.OnCalled.AddListener(
      procedure(const HookedEvent: THookedEventInfo<TIDEServicesCallType>) begin
        if HookedEvent.Method = iscRegisterFormClass then begin
          ClassesToRegister.Remove(HookedEvent.Args[0].VClass);
        end;
      end);

    Plugin := TIDEPlugin.Create(LintContext.IDEServices);
    Assert.AreEqual(2, ClassesToRegister.Count);
    Plugin.Init;
    try
      Assert.AreEqual(0, ClassesToRegister.Count);
    finally
      Plugin.Deinit(IDEServices);
    end;
  finally
    FreeAndNil(ClassesToRegister);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TestEditorHandlerCleanedUp;
var
  Plugin: IPlugin;
  IDEServices: TMockIDEServices;
begin
  BuildMockedContext(IDEServices);

  Plugin := TIDEPlugin.Create(LintContext.IDEServices);
  Plugin.Init;
  try
    Assert.AreEqual(1, IDEServices.IDE.EditorNotifiers.Count);
  finally
    Plugin.Deinit(IDEServices);
  end;
  Assert.AreEqual(0, IDEServices.IDE.EditorNotifiers.Count);
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TestPluginInfoCleanedUp;
var
  Plugin: IPlugin;
  IDEServices: TMockIDEServices;
begin
  BuildMockedContext(IDEServices);

  Plugin := TIDEPlugin.Create(LintContext.IDEServices);
  try
    Plugin.Init;
    Assert.AreEqual(1, IDEServices.IDE.PluginInfos.Count);
  finally
    Plugin.Deinit(IDEServices);
  end;
  Assert.AreEqual(0, IDEServices.IDE.PluginInfos.Count);
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TestMenuItemAdded(ExpectedIndex: Integer; ExpectedCaption: string);
var
  Plugin: IPlugin;
  IDEServices: TMockIDEServices;
  Root: TMenuItem;
begin
  BuildMockedContext(IDEServices);

  Plugin := TIDEPlugin.Create(LintContext.IDEServices);
  try
    Plugin.Init;

    Assert.IsTrue(IDEServices.IDE.Menus.ContainsKey('ToolsMenu'), 'DelphiLint menu should be after ''Tools''');
    Root := IDEServices.IDE.Menus['ToolsMenu'];
    Assert.IsTrue(Root.Count > ExpectedIndex);
    Assert.AreEqual(ExpectedCaption, Root[ExpectedIndex].Caption);
  finally
    Plugin.Deinit(IDEServices);
  end;
  Assert.AreEqual(0, IDEServices.IDE.PluginInfos.Count);
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TestActionsAddedToIDEInsight;
var
  Plugin: IPlugin;
  IDEServices: TMockIDEServices;
begin
  BuildMockedContext(IDEServices);

  Plugin := TIDEPlugin.Create(LintContext.IDEServices);
  try
    Plugin.Init;
    Assert.AreEqual(1, IDEServices.IDE.IDEInsightActions.Count);
    Assert.AreEqual(7, IDEServices.IDE.IDEInsightActions[0].ActionCount);
  finally
    Plugin.Deinit(IDEServices);
  end;
end;

//______________________________________________________________________________________________________________________

procedure TIDEPluginTest.TestIDEInsightActionsCleanedUp;
var
  Plugin: IPlugin;
  IDEServices: TMockIDEServices;
begin
  BuildMockedContext(IDEServices);

  Plugin := TIDEPlugin.Create(LintContext.IDEServices);
  try
    Plugin.Init;
  finally
    Plugin.Deinit(IDEServices);
  end;
  Assert.AreEqual(0, IDEServices.IDE.IDEInsightActions.Count);
end;

//______________________________________________________________________________________________________________________

initialization
  TDUnitX.RegisterTestFixture(TIDEPluginTest);

end.
