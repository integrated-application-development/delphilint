object LintSettingsFrame: TLintSettingsFrame
  Left = 0
  Top = 0
  Width = 757
  Height = 437
  Constraints.MinHeight = 200
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    757
    437)
  object TopPageControl: TPageControl
    Left = 3
    Top = 3
    Width = 751
    Height = 430
    ActivePage = GeneralSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 430
    RaggedRight = True
    TabOrder = 0
    StyleElements = [seFont, seClient]
    object GeneralSheet: TTabSheet
      Caption = 'General'
      object GeneralPanel: TPanel
        Left = 0
        Top = 0
        Width = 743
        Height = 400
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Label4: TLabel
          Left = 14
          Top = 8
          Width = 125
          Height = 15
          Caption = 'IDE and user experience'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object BrokenSetupWarningLabel: TLabel
          Left = 181
          Top = 116
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
        object Label2: TLabel
          Left = 14
          Top = 90
          Width = 95
          Height = 15
          Caption = 'External resources'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object ClientSaveBeforeAnalysisCheckBox: TCheckBox
          Left = 18
          Top = 53
          Width = 205
          Height = 17
          Hint = 
            'DelphiLint analyses do not reflect unsaved changes, so files should ' +
            'be saved before analyzing. Toggling this option makes this process ' +
            'automatic.'
          Caption = 'Save files before starting analysis'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object ClientAutoShowToolWindowCheckBox: TCheckBox
          Left = 18
          Top = 30
          Width = 333
          Height = 17
          Caption = 'Show the DelphiLint window when an analysis is started'
          TabOrder = 1
        end
        object ComponentsButton: TButton
          Left = 18
          Top = 111
          Width = 157
          Height = 25
          Caption = 'Set up external resources'
          TabOrder = 2
          OnClick = ComponentsButtonClick
        end
      end
    end
    object StandaloneSheet: TTabSheet
      Caption = 'Standalone Mode'
      ImageIndex = 1
      object StandalonePanel: TPanel
        Left = 0
        Top = 0
        Width = 743
        Height = 400
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 14
          Top = 8
          Width = 167
          Height = 15
          Caption = 'Standalone SonarDelphi version'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object SonarDelphiVersionComboBox: TComboBox
          Left = 39
          Top = 75
          Width = 195
          Height = 23
          TabOrder = 0
        end
        object SonarDelphiVersionRadioGroup: TRadioGroup
          Left = 12
          Top = 26
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
            'Use the default version'
            'Use a specific version')
          ParentDoubleBuffered = False
          ShowFrame = False
          TabOrder = 1
          OnClick = SonarDelphiVersionRadioGroupClick
        end
      end
    end
    object ConnectedSheet: TTabSheet
      Caption = 'Connected Mode'
      ImageIndex = 2
      object ConnectedPanel: TPanel
        Left = 0
        Top = 0
        Width = 743
        Height = 400
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          743
          400)
        object Label3: TLabel
          Left = 14
          Top = 8
          Width = 204
          Height = 15
          Caption = 'SonarQube server authorization tokens'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
        end
        object TokensGrid: TDBGrid
          Left = 56
          Top = 32
          Width = 674
          Height = 179
          Anchors = [akLeft, akTop, akRight, akBottom]
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
          Left = 25
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
    end
  end
  object TokensDataSource: TDataSource
    DataSet = TokensDataSet
    Left = 484
    Top = 98
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
    Left = 484
    Top = 154
  end
end
