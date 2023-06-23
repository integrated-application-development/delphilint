object LintOptionsForm: TLintOptionsForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Project Options'
  ClientHeight = 283
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
  object ProjectNameLabel: TLabel
    Left = 15
    Top = 8
    Width = 155
    Height = 37
    Caption = 'Project name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 303
    Top = 58
    Width = 265
    Height = 217
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
  end
  object GroupBox2: TGroupBox
    Left = 15
    Top = 58
    Width = 265
    Height = 97
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
  end
end
