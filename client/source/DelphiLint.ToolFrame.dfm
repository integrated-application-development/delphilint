object LintToolFrame: TLintToolFrame
  Left = 0
  Top = 0
  Width = 352
  Height = 247
  TabOrder = 0
  object LintPanel: TPanel
    Left = 0
    Top = 0
    Width = 352
    Height = 51
    Align = alTop
    BevelOuter = bvNone
    Caption = 'LintPanel'
    Color = clNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    object ProgLabel: TLabel
      Left = 30
      Top = 26
      Width = 52
      Height = 15
      Caption = 'Analyzing'
      Layout = tlCenter
    end
    object FileNameLabel: TLabel
      Left = 30
      Top = 10
      Width = 87
      Height = 15
      Caption = 'No file selected'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object ProgImage: TImage
      Left = 9
      Top = 10
      Width = 16
      Height = 16
      Proportional = True
    end
    object ProgBar: TProgressBar
      Left = 88
      Top = 27
      Width = 73
      Height = 16
      Smooth = True
      Style = pbstMarquee
      MarqueeInterval = 30
      TabOrder = 0
    end
    object LintButtonPanel: TPanel
      AlignWithMargins = True
      Left = 262
      Top = 0
      Width = 84
      Height = 51
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 6
      Margins.Bottom = 0
      Align = alRight
      AutoSize = True
      BevelOuter = bvNone
      Caption = 'LintButtonPanel'
      ShowCaption = False
      TabOrder = 1
      DesignSize = (
        84
        51)
      object LintButton: TBitBtn
        Left = 0
        Top = 7
        Width = 84
        Height = 24
        Action = LintPlugin.ActionAnalyzeActiveFile
        Anchors = [akTop, akRight]
        Caption = 'Analyze'
        Images = LintPlugin.LintImages
        ParentDoubleBuffered = True
        TabOrder = 0
      end
    end
  end
  object IssueListBox: TListBox
    Left = 0
    Top = 51
    Width = 352
    Height = 196
    Style = lbOwnerDrawFixed
    Align = alClient
    BorderStyle = bsNone
    ItemHeight = 20
    TabOrder = 1
  end
end
