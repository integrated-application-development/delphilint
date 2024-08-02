object LintSettingsFrame: TLintSettingsFrame
  Left = 0
  Top = 0
  Width = 772
  Height = 340
  Constraints.MinHeight = 340
  Constraints.MinWidth = 420
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    772
    340)
  object TopPageControl: TPageControl
    Left = 3
    Top = 3
    Width = 769
    Height = 332
    ActivePage = ConnectedSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    RaggedRight = True
    TabOrder = 0
    StyleElements = [seFont, seClient]
    object GeneralSheet: TTabSheet
      Caption = 'General'
      object GeneralPanel: TPanel
        Left = 0
        Top = 0
        Width = 761
        Height = 302
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
        Width = 761
        Height = 302
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object StandaloneRulesListBox: TCheckListBox
          Left = 0
          Top = 185
          Width = 761
          Height = 117
          Align = alClient
          ItemHeight = 15
          PopupMenu = StandaloneRulesPopupMenu
          TabOrder = 0
        end
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 761
          Height = 185
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object Label1: TLabel
            Left = 13
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
          object Label5: TLabel
            Left = 13
            Top = 112
            Width = 87
            Height = 15
            Caption = 'Standalone rules'
          end
          object SonarDelphiVersionComboBox: TComboBox
            Left = 36
            Top = 75
            Width = 116
            Height = 23
            TabOrder = 0
          end
          object SonarDelphiVersionRadioGroup: TRadioGroup
            Left = 11
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
          object StandaloneRulesRadioGroup: TRadioGroup
            Left = 11
            Top = 130
            Width = 221
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
              'Use SonarDelphi'#39's default ruleset'
              'Use a custom ruleset')
            ParentDoubleBuffered = False
            ShowFrame = False
            TabOrder = 2
            OnClick = StandaloneRulesRadioGroupClick
          end
          object VersionRefreshButton: TButton
            Left = 158
            Top = 75
            Width = 43
            Height = 23
            Caption = 'Edit'
            TabOrder = 3
            OnClick = VersionRefreshButtonClick
          end
        end
        object PlaceholderStandaloneRulesPanel: TPanel
          Left = 0
          Top = 185
          Width = 761
          Height = 117
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsSingle
          Caption = 'Click to load ruleset'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBtnText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = PlaceholderStandaloneRulesPanelClick
        end
      end
    end
    object ConnectedSheet: TTabSheet
      Caption = 'Connected Mode'
      ImageIndex = 2
      object ConnectedPanel: TPanel
        Left = 0
        Top = 0
        Width = 761
        Height = 302
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object TokensGrid: TDBGrid
          Left = 25
          Top = 73
          Width = 736
          Height = 229
          Align = alClient
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
              Width = 337
              Visible = True
            end>
        end
        object ConnectedTopPanel: TPanel
          Left = 0
          Top = 0
          Width = 761
          Height = 73
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          DesignSize = (
            761
            73)
          object AuthHeaderLabel: TLabel
            Left = 13
            Top = 8
            Width = 166
            Height = 15
            Caption = 'SonarQube server authorization'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBtnText
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
          end
          object TokenNotePanel: TPanel
            AlignWithMargins = True
            Left = 25
            Top = 33
            Width = 736
            Height = 32
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Margins.Bottom = 10
            Anchors = [akLeft, akTop, akRight]
            BevelOuter = bvNone
            Padding.Left = 4
            Padding.Top = 4
            Padding.Right = 4
            Padding.Bottom = 4
            ParentBackground = False
            TabOrder = 0
            object TokenNoteLabel: TLabel
              AlignWithMargins = True
              Left = 9
              Top = 8
              Width = 620
              Height = 20
              Margins.Left = 5
              Margins.Top = 4
              Align = alClient
              Caption = 
                'User-level tokens (squ_...) are recommended for full compatibility.'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clBtnText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
            end
            object LearnMoreButton: TButton
              Left = 632
              Top = 4
              Width = 100
              Height = 24
              Margins.Left = 5
              Align = alRight
              Caption = 'Learn more...'
              TabOrder = 0
              OnClick = LearnMoreButtonClick
            end
          end
        end
        object Panel3: TPanel
          Left = 0
          Top = 73
          Width = 25
          Height = 229
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 2
          object DBNavigator1: TDBNavigator
            Left = 1
            Top = 0
            Width = 20
            Height = 50
            DataSource = TokensDataSource
            VisibleButtons = [nbInsert, nbDelete]
            Hints.Strings = (
              'Add record'
              'Remove record'
              '')
            Kind = dbnVertical
            ConfirmDelete = False
            TabOrder = 0
          end
        end
      end
    end
  end
  object TokensDataSource: TDataSource
    DataSet = TokensDataSet
    Left = 124
    Top = 178
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
    Left = 268
    Top = 146
  end
  object StandaloneRulesPopupMenu: TPopupMenu
    Left = 359
    Top = 165
    object EnableAll: TMenuItem
      Caption = 'Enable All'
      OnClick = EnableAllClick
    end
    object DisableAll: TMenuItem
      Caption = 'Disable All'
      OnClick = DisableAllClick
    end
  end
end
