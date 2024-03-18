object LintSettingsFrame: TLintSettingsFrame
  Left = 0
  Top = 0
  Width = 586
  Height = 628
  Constraints.MinHeight = 170
  Constraints.MinWidth = 586
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    586
    628)
  object ComponentsGroupBox: TGroupBox
    Left = 12
    Top = 521
    Width = 560
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Caption = 'External resources'
    TabOrder = 0
    object BrokenSetupWarningLabel: TLabel
      Left = 175
      Top = 38
      Width = 210
      Height = 15
      Caption = 'External resources are misconfigured.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      StyleElements = [seClient, seBorder]
    end
    object ComponentsButton: TButton
      Left = 12
      Top = 32
      Width = 157
      Height = 25
      Caption = 'Set up external resources'
      TabOrder = 0
      OnClick = ComponentsButtonClick
    end
  end
  object ClientConfigGroupBox: TGroupBox
    Left = 12
    Top = 9
    Width = 560
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'IDE configuration'
    TabOrder = 1
    object ClientAutoShowToolWindowCheckBox: TCheckBox
      Left = 12
      Top = 31
      Width = 333
      Height = 17
      Caption = 'Show the DelphiLint window when an analysis is started'
      TabOrder = 0
    end
    object ClientSaveBeforeAnalysisCheckBox: TCheckBox
      Left = 12
      Top = 54
      Width = 205
      Height = 17
      Hint = 
        'DelphiLint analyses do not reflect unsaved changes, so files should ' +
        'be saved before analyzing. Toggling this option makes this process ' +
        'automatic.'
      Caption = 'Save files before starting analysis'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
  end
  object TokensGroupBox: TGroupBox
    Left = 12
    Top = 280
    Width = 560
    Height = 226
    Anchors = [akLeft, akTop, akRight]
    Caption = 'SonarQube tokens'
    TabOrder = 2
    object TokensGrid: TDBGrid
      Left = 38
      Top = 32
      Width = 507
      Height = 177
      DataSource = TokensDataSource
      Options = [dgEditing, dgTitles, dgIndicator, dgRowLines, dgTabs, dgCancelOnExit]
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clBtnText
      TitleFont.Height = -12
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'ServerURL'
          Title.Caption = 'Server URL'
          Width = 154
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'ProjectKey'
          Title.Caption = 'Project Key'
          Width = 163
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Token'
          Title.Caption = 'Authorization Token'
          Width = 131
          Visible = True
        end>
    end
    object DBNavigator1: TDBNavigator
      Left = 7
      Top = 32
      Width = 25
      Height = 60
      DataSource = TokensDataSource
      VisibleButtons = [nbInsert, nbDelete]
      Hints.Strings = (
        'Add record'
        'Remove record'
        '')
      Kind = dbnVertical
      ConfirmDelete = False
      TabOrder = 1
    end
  end
  object ServerConfigGroupBox: TGroupBox
    Left = 12
    Top = 109
    Width = 560
    Height = 148
    Caption = 'SonarDelphi version configuration'
    TabOrder = 3
    object Label1: TLabel
      Left = 12
      Top = 28
      Width = 434
      Height = 15
      Caption = 
        'DelphiLint falls back to an automatically downloaded version of SonarDelphi ' +
        'when'
    end
    object Label2: TLabel
      Left = 12
      Top = 44
      Width = 397
      Height = 15
      Caption = 
        'in standalone mode, or when "Use server'#39's SonarDelphi version" is ' +
        'disabled.'
    end
    object SonarDelphiVersionComboBox: TComboBox
      Left = 39
      Top = 117
      Width = 195
      Height = 23
      TabOrder = 0
    end
    object SonarDelphiVersionRadioGroup: TRadioGroup
      Left = 12
      Top = 68
      Width = 453
      Height = 49
      DefaultHeaderFont = False
      DoubleBuffered = False
      HeaderFont.Charset = DEFAULT_CHARSET
      HeaderFont.Color = clBtnText
      HeaderFont.Height = -1
      HeaderFont.Name = 'Segoe UI'
      HeaderFont.Style = []
      ItemIndex = 0
      Items.Strings = (
        'Use default SonarDelphi version'
        'Use specific SonarDelphi version')
      ParentDoubleBuffered = False
      ShowFrame = False
      TabOrder = 1
      OnClick = SonarDelphiVersionRadioGroupClick
    end
  end
  object TokensDataSource: TDataSource
    DataSet = TokensDataSet
    Left = 276
    Top = 402
  end
  object TokensDataSet: TClientDataSet
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ServerURL'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 80
      end
      item
        Name = 'ProjectKey'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 80
      end
      item
        Name = 'Token'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 80
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 228
    Top = 402
  end
end
