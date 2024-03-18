object LintSetupForm: TLintSetupForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'DelphiLint External Resources Setup'
  ClientHeight = 333
  ClientWidth = 864
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 15
  object LeftPanel: TPanel
    Left = 0
    Top = 0
    Width = 345
    Height = 333
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
      Top = 75
      Width = 291
      Height = 243
      AutoSize = False
      Caption = 
        'DelphiLint integrates a Delphi code analyzer into the Delphi IDE ' +
        'to provide on-the-fly linting.'#13#10#13#10'There are three primary components ' +
        'to DelphiLint: the IDE plugin itself, a companion Java server that ' +
        'runs in the background, and the SonarDelphi plugin, which the server ' +
        'uses to carry out analysis.'#13#10#13#10'While SonarDelphi is automatically ' +
        'downloaded, the server and a Java 11 or above executable must be ' +
        'manually installed.'#13#10#13#10'Please ensure all resources are installed ' +
        'and configured correctly. For more detailed installation instructions, ' +
        'please refer to the DelphiLint README on GitHub.'
      WordWrap = True
    end
  end
  object RightPanel: TPanel
    Left = 345
    Top = 0
    Width = 519
    Height = 333
    Align = alClient
    BevelOuter = bvNone
    Caption = 'RightPanel'
    Color = clWindow
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seBorder]
    DesignSize = (
      519
      333)
    object JavaExeLabel: TLabel
      Left = 32
      Top = 48
      Width = 114
      Height = 30
      Caption = 'Java 11+ executable'#13#10
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ServerJarLabel: TLabel
      Left = 32
      Top = 159
      Width = 113
      Height = 15
      Caption = 'DelphiLint server jar'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 32
      Top = 66
      Width = 444
      Height = 15
      Caption = 
        'By default, DelphiLint uses the Java version in the JAVA_HOME environment ' +
        'variable.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object OkButton: TButton
      Left = 32
      Top = 251
      Width = 405
      Height = 35
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Apply this configuration'
      TabOrder = 4
      OnClick = OkButtonClick
    end
    object RefreshButton: TButton
      Left = 32
      Top = 221
      Width = 405
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Refresh'
      TabOrder = 3
      OnClick = RefreshButtonClick
    end
    object JavaExeBrowseButton: TButton
      Left = 32
      Top = 122
      Width = 97
      Height = 21
      Caption = 'Select override'
      TabOrder = 1
      OnClick = JavaExeBrowseButtonClick
    end
    object ServerJarIndicator: TPanel
      Left = 32
      Top = 177
      Width = 405
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
    object JavaExeIndicator: TPanel
      Left = 32
      Top = 86
      Width = 405
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
    object JavaExeClearButton: TButton
      Left = 135
      Top = 122
      Width = 186
      Height = 21
      Caption = 'Clear override (use JAVA_HOME)'
      TabOrder = 5
      OnClick = JavaExeClearButtonClick
    end
  end
  object ExeOpenDialog: TOpenDialog
    Filter = 'Executable files (*.exe)|*.EXE'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 304
    Top = 16
  end
end
