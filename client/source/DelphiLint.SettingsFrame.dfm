object LintSettingsFrame: TLintSettingsFrame
  Left = 0
  Top = 0
  Width = 586
  Height = 192
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
    192)
  object ComponentsGroupBox: TGroupBox
    Left = 12
    Top = 112
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
    Top = 16
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
end
