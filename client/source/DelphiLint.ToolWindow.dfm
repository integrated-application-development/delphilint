object LintToolWindow: TLintToolWindow
  Left = 0
  Top = 0
  Caption = 'DelphiLint'
  ClientHeight = 104
  ClientWidth = 269
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object LintToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 269
    Height = 29
    Images = LintPlugin.LintImages
    TabOrder = 0
    object LintButton: TToolButton
      Left = 0
      Top = 0
      ImageIndex = 0
      Style = tbsDropDown
      OnClick = LintButtonClick
    end
    object ProgPanel: TPanel
      Left = 38
      Top = 0
      Width = 100
      Height = 22
      BevelOuter = bvNone
      Caption = 'ProgPanel'
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
      VerticalAlignment = taAlignTop
      object ProgLabel: TLabel
        Left = 2
        Top = 2
        Width = 53
        Height = 15
        Caption = 'ProgLabel'
      end
      object ProgBar: TProgressBar
        Left = 2
        Top = 16
        Width = 98
        Height = 10
        Step = 1
        TabOrder = 0
      end
    end
  end
end
