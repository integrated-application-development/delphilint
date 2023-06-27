object LintOptionsForm: TLintOptionsForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Project Options'
  ClientHeight = 357
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
    Height = 285
    Align = alClient
    BevelOuter = bvNone
    Caption = 'ContentPanel'
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seBorder]
    object GroupBox1: TGroupBox
      Left = 16
      Top = 16
      Width = 265
      Height = 239
      Caption = 'SonarQube connection'
      TabOrder = 0
      object SonarHostUrlEdit: TLabeledEdit
        Left = 16
        Top = 48
        Width = 233
        Height = 23
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
        Width = 233
        Height = 23
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
        Width = 233
        Height = 23
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
    object GroupBox2: TGroupBox
      Left = 296
      Top = 16
      Width = 265
      Height = 196
      Caption = 'Project analysis'
      TabOrder = 1
      object ProjectBaseDirEdit: TLabeledEdit
        Left = 16
        Top = 48
        Width = 233
        Height = 23
        EditLabel.Width = 74
        EditLabel.Height = 15
        EditLabel.Caption = 'Base directory'
        TabOrder = 0
        Text = ''
        OnChange = ProjectBaseDirEditChange
      end
      object ProjectPropertiesPathEdit: TLabeledEdit
        Left = 16
        Top = 112
        Width = 233
        Height = 23
        EditLabel.Width = 193
        EditLabel.Height = 15
        EditLabel.Caption = 'Sonar project properties (.properties)'
        TabOrder = 1
        Text = ''
        OnChange = ProjectPropertiesPathEditChange
      end
      object ProjectPropertiesBrowseButton: TButton
        Left = 16
        Top = 141
        Width = 75
        Height = 25
        Caption = 'Browse...'
        TabOrder = 2
        OnClick = ProjectPropertiesBrowseButtonClick
      end
    end
  end
  object FooterPanel: TPanel
    Left = 0
    Top = 320
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
    Left = 496
    Top = 271
  end
end
