object LintOptionsForm: TLintOptionsForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Project Options'
  ClientHeight = 334
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object HeaderPanel: TPanel
    Left = 0
    Top = 0
    Width = 577
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    Caption = 'HeaderPanel'
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    object ProjectNameLabel: TLabel
      Left = 16
      Top = 10
      Width = 74
      Height = 15
      Caption = 'Project name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object ContentPanel: TPanel
    Left = 0
    Top = 35
    Width = 577
    Height = 262
    Align = alClient
    BevelOuter = bvNone
    Caption = 'ContentPanel'
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seBorder]
    object SonarHostGroup: TGroupBox
      Left = 305
      Top = 14
      Width = 266
      Height = 239
      Caption = 'SonarQube connection'
      TabOrder = 0
      DesignSize = (
        266
        239)
      object SonarHostUrlEdit: TLabeledEdit
        Left = 16
        Top = 48
        Width = 234
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 56
        EditLabel.Height = 15
        EditLabel.Caption = 'Server URL'
        TabOrder = 0
        Text = ''
        OnChange = SonarHostUrlEditChange
      end
      object SonarHostTokenEdit: TLabeledEdit
        Left = 16
        Top = 173
        Width = 234
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 105
        EditLabel.Height = 15
        EditLabel.Caption = 'Authorization token'
        TabOrder = 1
        Text = ''
        OnChange = SonarHostTokenEditChange
      end
      object ProjectKeyEdit: TLabeledEdit
        Left = 16
        Top = 112
        Width = 234
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 58
        EditLabel.Height = 15
        EditLabel.Caption = 'Project key'
        TabOrder = 2
        Text = ''
        OnChange = ProjectKeyEditChange
      end
      object CreateTokenButton: TButton
        Left = 16
        Top = 202
        Width = 105
        Height = 25
        Caption = 'Create Token'
        TabOrder = 3
        OnClick = CreateTokenButtonClick
      end
    end
    object AnalysisGroup: TGroupBox
      Left = 16
      Top = 99
      Width = 267
      Height = 145
      Caption = 'Analysis settings'
      TabOrder = 1
      DesignSize = (
        267
        145)
      object ProjectBaseDirEdit: TLabeledEdit
        Left = 16
        Top = 48
        Width = 235
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 74
        EditLabel.Height = 15
        EditLabel.Caption = 'Base directory'
        TabOrder = 0
        Text = ''
        OnChange = ProjectBaseDirEditChange
      end
      object ProjectBaseDirBrowseButton: TButton
        Left = 16
        Top = 77
        Width = 75
        Height = 25
        Caption = 'Browse...'
        TabOrder = 1
        OnClick = ProjectBaseDirBrowseButtonClick
      end
      object ProjectReadPropertiesCheckBox: TCheckBox
        Left = 15
        Top = 115
        Width = 234
        Height = 17
        Caption = 'Read sonar-project.properties if present'
        TabOrder = 2
        OnClick = ProjectReadPropertiesCheckBoxClick
      end
    end
    object AnalysisModeGroupBox: TGroupBox
      Left = 16
      Top = 14
      Width = 267
      Height = 71
      Caption = 'Analysis mode'
      TabOrder = 2
      object AnalysisModeGroup: TRadioGroup
        Left = 0
        Top = 11
        Width = 267
        Height = 59
        Items.Strings = (
          'Standalone'
          'Connected (SonarQube)')
        ShowFrame = False
        TabOrder = 0
        OnClick = AnalysisModeGroupClick
      end
    end
  end
  object FooterPanel: TPanel
    Left = 0
    Top = 297
    Width = 577
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'FooterPanel'
    ShowCaption = False
    TabOrder = 2
    object SaveButton: TButton
      Left = 415
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 0
      OnClick = SaveButtonClick
    end
    object CancelButton: TButton
      Left = 496
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = CancelButtonClick
    end
  end
  object PropertiesOpenDialog: TOpenDialog
    Filter = 'Properties files (*.properties)|*.PROPERTIES'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 480
    Top = 231
  end
end
