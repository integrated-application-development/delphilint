object LintSetupForm: TLintSetupForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'DelphiLint External Resources Setup'
  ClientHeight = 334
  ClientWidth = 898
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 15
  object LeftPanel: TPanel
    Left = 0
    Top = 0
    Width = 345
    Height = 334
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'LeftPanel'
    ShowCaption = False
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 39
      Width = 173
      Height = 30
      Caption = 'External Resources'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 24
      Top = 83
      Width = 291
      Height = 49
      AutoSize = False
      Caption = 
        'DelphiLint integrates a Delphi code analyzer into the Delphi IDE ' +
        'to provide on-the-fly linting. '
      WordWrap = True
    end
    object Label3: TLabel
      Left = 24
      Top = 122
      Width = 291
      Height = 61
      AutoSize = False
      Caption = 
        'There are three primary components to DelphiLint: the IDE plugin ' +
        'itself, a companion Java server that runs in the background, and ' +
        'the IntegraDev SonarDelphi plugin, which the server uses to carry ' +
        'out analysis.'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 24
      Top = 192
      Width = 291
      Height = 86
      AutoSize = False
      Caption = 
        'To run, DelphiLint requires both the server and SonarDelphi, as well ' +
        'as a Java 11 or above executable. Please ensure all resources have ' +
        'been installed.'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 24
      Top = 247
      Width = 291
      Height = 32
      AutoSize = False
      Caption = 'For more detailed installation instructions, please refer to'
      WordWrap = True
    end
    object LinkLabel1: TLinkLabel
      Left = 38
      Top = 262
      Width = 188
      Height = 19
      Caption = 
        '<a href="https://github.com/Integrated-Application-Development/delphilint">the ' +
        'DelphiLint README on GitHub.</a>'
      TabOrder = 0
    end
  end
  object RightPanel: TPanel
    Left = 345
    Top = 0
    Width = 553
    Height = 334
    Align = alClient
    BevelOuter = bvNone
    Caption = 'RightPanel'
    Color = clWindow
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seBorder]
    DesignSize = (
      553
      334)
    object SonarDelphiJarLabel: TLabel
      Left = 32
      Top = 156
      Width = 117
      Height = 15
      Caption = 'SonarDelphi plugin jar'
    end
    object JavaExeLabel: TLabel
      Left = 32
      Top = 37
      Width = 105
      Height = 30
      Caption = 'Java 11+ executable'#13#10
    end
    object ServerJarLabel: TLabel
      Left = 32
      Top = 96
      Width = 104
      Height = 15
      Caption = 'DelphiLint server jar'
    end
    object OkButton: TButton
      Left = 32
      Top = 264
      Width = 481
      Height = 35
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Apply this configuration'
      TabOrder = 5
      OnClick = OkButtonClick
    end
    object RefreshButton: TButton
      Left = 32
      Top = 234
      Width = 481
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Refresh'
      TabOrder = 4
      OnClick = RefreshButtonClick
    end
    object ServerJarBrowseButton: TButton
      Left = 433
      Top = 55
      Width = 80
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Browse...'
      TabOrder = 1
      OnClick = JavaExeBrowseButtonClick
    end
    object ServerJarIndicator: TPanel
      Left = 32
      Top = 114
      Width = 481
      Height = 30
      Alignment = taLeftJustify
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      BorderWidth = 1
      Caption = 'ServerJarIndicator'
      Color = clLime
      ParentBackground = False
      TabOrder = 2
      StyleElements = []
    end
    object SonarDelphiJarIndicator: TPanel
      Left = 32
      Top = 174
      Width = 481
      Height = 30
      Alignment = taLeftJustify
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      BorderWidth = 1
      Caption = 'SonarDelphiJarIndicator'
      Color = clLime
      ParentBackground = False
      TabOrder = 3
      StyleElements = []
    end
    object JavaExeIndicator: TPanel
      Left = 32
      Top = 55
      Width = 395
      Height = 30
      Alignment = taLeftJustify
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      BorderWidth = 1
      Caption = 'JavaExeIndicator'
      Color = clLime
      ParentBackground = False
      TabOrder = 0
      StyleElements = []
    end
  end
  object ExeOpenDialog: TOpenDialog
    Filter = 'Executable files (*.exe)|*.EXE'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 304
    Top = 16
  end
end
