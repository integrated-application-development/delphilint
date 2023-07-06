object LintSettingsFrame: TLintSettingsFrame
  Left = 0
  Top = 0
  Width = 986
  Height = 507
  Constraints.MinHeight = 300
  Constraints.MinWidth = 586
  TabOrder = 0
  object SettingsGridPanel: TGridPanel
    Left = 0
    Top = 0
    Width = 986
    Height = 507
    Align = alClient
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = LeftPanel
        Row = 0
      end
      item
        Column = 1
        Control = RightPanel
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    ShowCaption = False
    TabOrder = 0
    object LeftPanel: TPanel
      Left = 0
      Top = 0
      Width = 493
      Height = 507
      Align = alClient
      BevelOuter = bvNone
      Caption = 'LeftPanel'
      ShowCaption = False
      TabOrder = 0
      DesignSize = (
        493
        507)
      object SonarDelphiConfigGroupBox: TGroupBox
        Left = 12
        Top = 99
        Width = 462
        Height = 122
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SonarDelphi configuration'
        TabOrder = 0
        DesignSize = (
          462
          122)
        object SonarDelphiJarEdit: TLabeledEdit
          Left = 11
          Top = 47
          Width = 438
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 151
          EditLabel.Height = 15
          EditLabel.Caption = 'SonarDelphi executable (.jar)'
          TabOrder = 0
          Text = ''
        end
        object SonarDelphiJarBrowseButton: TButton
          Left = 11
          Top = 76
          Width = 75
          Height = 23
          Caption = 'Browse...'
          TabOrder = 1
          OnClick = SonarDelphiJarBrowseButtonClick
        end
      end
      object ClientConfigGroupBox: TGroupBox
        Left = 12
        Top = 16
        Width = 462
        Height = 69
        Anchors = [akLeft, akTop, akRight]
        Caption = 'IDE configuration'
        TabOrder = 1
        object ClientDarkModeCheckBox: TCheckBox
          Left = 11
          Top = 30
          Width = 174
          Height = 17
          Caption = 'Dark mode'
          TabOrder = 0
        end
      end
    end
    object RightPanel: TPanel
      Left = 493
      Top = 0
      Width = 493
      Height = 507
      Align = alClient
      BevelOuter = bvNone
      Caption = 'RightPanel'
      ShowCaption = False
      TabOrder = 1
      DesignSize = (
        493
        507)
      object ServerConfigGroupBox: TGroupBox
        Left = 19
        Top = 16
        Width = 463
        Height = 345
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Server configuration'
        TabOrder = 0
        DesignSize = (
          463
          345)
        object AdvancedLabel: TLabel
          Left = 19
          Top = 263
          Width = 55
          Height = 15
          Caption = 'Advanced'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object ServerJarEdit: TLabeledEdit
          Left = 11
          Top = 106
          Width = 326
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 119
          EditLabel.Height = 15
          EditLabel.Caption = 'Server executable (.jar)'
          TabOrder = 0
          Text = ''
        end
        object ServerJavaExeEdit: TLabeledEdit
          Left = 11
          Top = 194
          Width = 326
          Height = 23
          Anchors = [akLeft, akTop, akRight]
          EditLabel.Width = 82
          EditLabel.Height = 15
          EditLabel.Caption = 'Java executable'
          TabOrder = 1
          Text = ''
        end
        object ServerShowConsoleCheckBox: TCheckBox
          Left = 19
          Top = 283
          Width = 142
          Height = 17
          Caption = 'Show server console'
          TabOrder = 2
        end
        object ServerAutoLaunchCheckBox: TCheckBox
          Left = 19
          Top = 306
          Width = 174
          Height = 17
          Caption = 'Launch server automatically'
          TabOrder = 3
        end
        object ServerStartDelayEdit: TLabeledEdit
          Left = 11
          Top = 46
          Width = 130
          Height = 23
          EditLabel.Width = 134
          EditLabel.Height = 15
          EditLabel.Caption = 'Connection timeout (ms)'
          NumbersOnly = True
          TabOrder = 4
          Text = ''
        end
        object ServerJarBrowseButton: TButton
          Left = 11
          Top = 135
          Width = 75
          Height = 23
          Caption = 'Browse...'
          TabOrder = 5
          OnClick = ServerJarBrowseButtonClick
        end
        object ServerJavaExeBrowseButton: TButton
          Left = 11
          Top = 223
          Width = 75
          Height = 23
          Caption = 'Browse...'
          TabOrder = 6
          OnClick = ServerJavaExeBrowseButtonClick
        end
      end
    end
  end
  object JarOpenDialog: TOpenDialog
    Filter = 'Jar files (*.jar)|*.JAR'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 24
    Top = 448
  end
  object ExeOpenDialog: TOpenDialog
    Filter = 'Executable files (*.exe)|*.EXE'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 120
    Top = 448
  end
end
