object LintSettingsFrame: TLintSettingsFrame
  Left = 0
  Top = 0
  Width = 586
  Height = 250
  Constraints.MinHeight = 250
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
    250)
  object ComponentsGroupBox: TGroupBox
    Left = 12
    Top = 160
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
    Top = 16
    Width = 556
    Height = 129
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Server configuration'
    TabOrder = 0
    object ServerShowConsoleCheckBox: TCheckBox
      Left = 12
      Top = 79
      Width = 142
      Height = 17
      Caption = 'Show server console'
      TabOrder = 1
    end
    object ServerAutoLaunchCheckBox: TCheckBox
      Left = 12
      Top = 102
      Width = 174
      Height = 17
      Caption = 'Launch server automatically'
      TabOrder = 2
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
      TabOrder = 0
      Text = ''
    end
  end
end
