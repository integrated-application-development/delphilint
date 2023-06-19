object LintToolFrame: TLintToolFrame
  Left = 0
  Top = 0
  Width = 577
  Height = 247
  TabOrder = 0
  object ContentPanel: TPanel
    Left = 0
    Top = 51
    Width = 577
    Height = 196
    Align = alClient
    BevelOuter = bvNone
    Caption = 'ContentPanel'
    ShowCaption = False
    TabOrder = 0
    object RulePanel: TPanel
      Left = 312
      Top = 0
      Width = 265
      Height = 196
      Align = alRight
      BevelOuter = bvNone
      Caption = 'RulePanel'
      ShowCaption = False
      TabOrder = 0
      object RuleDescLabel: TLabel
        AlignWithMargins = True
        Left = 6
        Top = 49
        Width = 253
        Height = 141
        Margins.Left = 6
        Margins.Top = 0
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alClient
        AutoSize = False
        Caption = 'Rule description'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object RuleHeading: TPanel
        Left = 0
        Top = 0
        Width = 265
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        Caption = 'RuleHeading'
        ShowCaption = False
        TabOrder = 0
        object RuleNameLabel: TLabel
          Left = 6
          Top = 6
          Width = 59
          Height = 15
          Caption = 'Rule name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object RuleTypeLabel: TLabel
          Left = 6
          Top = 24
          Width = 49
          Height = 15
          Caption = 'Rule type'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsItalic]
          ParentFont = False
        end
      end
    end
    object IssueListBox: TListBox
      Left = 0
      Top = 0
      Width = 310
      Height = 196
      Style = lbOwnerDrawFixed
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 20
      TabOrder = 1
    end
    object SplitPanel: TPanel
      Left = 310
      Top = 0
      Width = 2
      Height = 196
      Cursor = crHSplit
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 1
      Caption = 'SplitPanel'
      ShowCaption = False
      TabOrder = 2
      OnMouseDown = SplitPanelMouseDown
      OnMouseMove = SplitPanelMouseMove
      OnMouseUp = SplitPanelMouseUp
    end
  end
  object LintPanel: TPanel
    Left = 0
    Top = 0
    Width = 577
    Height = 51
    Align = alTop
    BevelOuter = bvNone
    Caption = 'LintPanel'
    Color = clNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
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
      Left = 487
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
        Action = LintPlugin.ActionAnalyzeShort
        Anchors = [akTop, akRight]
        Caption = 'Analyze'
        Images = LintPlugin.LintImages
        ParentDoubleBuffered = True
        TabOrder = 0
      end
    end
  end
end
