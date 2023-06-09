object LintIssueFrame: TLintIssueFrame
  Left = 0
  Top = 0
  Width = 407
  Height = 48
  Anchors = [akLeft, akRight]
  TabOrder = 0
  object LintIssuePanel: TPanel
    Left = 0
    Top = 0
    Width = 407
    Height = 44
    Align = alClient
    Caption = 'LintIssuePanel'
    Constraints.MaxHeight = 44
    Constraints.MinHeight = 44
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      407
      44)
    object IssueKeyLabel: TLabel
      Left = 314
      Top = 8
      Width = 83
      Height = 15
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'delph:IssueKey'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object IssueLineLabel: TLabel
      Left = 10
      Top = 23
      Width = 31
      Height = 15
      Caption = 'XX:YY'
    end
    object IssueMessageLabel: TLabel
      Left = 10
      Top = 8
      Width = 105
      Height = 15
      Caption = 'IssueMessageLabel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end
