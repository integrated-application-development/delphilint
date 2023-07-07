object LintSettingsFrame: TLintSettingsFrame
  Left = 0
  Top = 0
  Width = 586
  Height = 319
  Constraints.MinHeight = 300
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
    319)
  object ClientConfigGroupBox: TGroupBox
    Left = 12
    Top = 16
    Width = 555
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = 'IDE configuration'
    TabOrder = 0
    object ClientDarkModeCheckBox: TCheckBox
      Left = 11
      Top = 30
      Width = 174
      Height = 17
      Caption = 'Dark mode'
      TabOrder = 0
    end
  end
  object ComponentsGroupBox: TGroupBox
    Left = 12
    Top = 232
    Width = 556
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Caption = 'External resources'
    TabOrder = 1
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
  object ServerConfigGroupBox: TGroupBox
    Left = 12
    Top = 88
    Width = 556
    Height = 129
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Server configuration'
    TabOrder = 2
    object ServerShowConsoleCheckBox: TCheckBox
      Left = 12
      Top = 79
      Width = 142
      Height = 17
      Caption = 'Show server console'
      TabOrder = 0
    end
    object ServerAutoLaunchCheckBox: TCheckBox
      Left = 12
      Top = 102
      Width = 174
      Height = 17
      Caption = 'Launch server automatically'
      TabOrder = 1
    end
    object ServerStartDelayEdit: TLabeledEdit
      Left = 11
      Top = 46
      Width = 190
      Height = 23
      EditLabel.Width = 134
      EditLabel.Height = 15
      EditLabel.Caption = 'Connection timeout (ms)'
      NumbersOnly = True
      TabOrder = 2
      Text = ''
    end
  end
end
