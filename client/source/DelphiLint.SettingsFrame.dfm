object LintSettingsFrame: TLintSettingsFrame
  Left = 0
  Top = 0
  Width = 586
  Height = 265
  Constraints.MinHeight = 200
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
    265)
  object ComponentsGroupBox: TGroupBox
    Left = 12
    Top = 88
    Width = 560
    Height = 65
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
    Top = 168
    Width = 560
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Server configuration'
    TabOrder = 0
    object ServerShowConsoleCheckBox: TCheckBox
      Left = 12
      Top = 31
      Width = 142
      Height = 17
      Caption = 'Show server console'
      TabOrder = 0
    end
    object ServerAutoLaunchCheckBox: TCheckBox
      Left = 12
      Top = 54
      Width = 237
      Height = 17
      Caption = 'Connect to external DelphiLint server'
      TabOrder = 1
    end
  end
  object ClientConfigGroupBox: TGroupBox
    Left = 12
    Top = 16
    Width = 560
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = 'IDE configuration'
    TabOrder = 2
    object ClientAutoShowToolWindowCheckBox: TCheckBox
      Left = 12
      Top = 31
      Width = 333
      Height = 17
      Caption = 'Show the DelphiLint window when an analysis is started'
      TabOrder = 0
    end
  end
end
