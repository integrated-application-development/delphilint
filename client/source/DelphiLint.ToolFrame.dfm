object LintToolFrame: TLintToolFrame
  Left = 0
  Top = 0
  Width = 708
  Height = 371
  DoubleBuffered = True
  ParentDoubleBuffered = False
  TabOrder = 0
  OnResize = FrameResize
  object ContentPanel: TPanel
    Left = 0
    Top = 50
    Width = 708
    Height = 294
    Align = alClient
    BevelOuter = bvNone
    Caption = 'ContentPanel'
    ShowCaption = False
    TabOrder = 0
    object RulePanel: TPanel
      Left = 443
      Top = 0
      Width = 265
      Height = 294
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
        Height = 239
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
        DesignSize = (
          265
          49)
        object RuleNameLabel: TLabel
          Left = 6
          Top = 6
          Width = 251
          Height = 15
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
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
      Width = 435
      Height = 294
      Style = lbOwnerDrawVariable
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 20
      TabOrder = 1
    end
    object SplitPanel: TPanel
      Left = 435
      Top = 0
      Width = 8
      Height = 294
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
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 708
    Height = 50
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    Caption = 'TopPanel'
    ShowCaption = False
    TabOrder = 1
    object FileHeadingPanel: TPanel
      Left = 0
      Top = 0
      Width = 708
      Height = 50
      Align = alBottom
      Caption = 'FileHeadingPanel'
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
      DesignSize = (
        708
        50)
      object FileStatusLabel: TLabel
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
      object LintButtonPanel: TPanel
        AlignWithMargins = True
        Left = 617
        Top = 1
        Width = 84
        Height = 48
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 6
        Margins.Bottom = 0
        Align = alRight
        AutoSize = True
        BevelOuter = bvNone
        Caption = 'LintButtonPanel'
        ShowCaption = False
        TabOrder = 0
      end
      object LintToolBar: TToolBar
        Left = 608
        Top = 11
        Width = 94
        Height = 29
        Align = alNone
        Anchors = [akTop, akRight]
        ButtonHeight = 30
        ButtonWidth = 74
        Caption = 'LintToolBar'
        Images = LintPlugin.LintImages
        List = True
        ShowCaptions = True
        TabOrder = 1
        object AnalyzeShortButton: TToolButton
          Left = 0
          Top = 0
          Action = LintPlugin.ActionAnalyzeShort
          DropdownMenu = AnalyzePopupMenu
          Style = tbsDropDown
        end
      end
    end
  end
  object StatusPanel: TPanel
    Left = 0
    Top = 344
    Width = 708
    Height = 27
    Align = alBottom
    Caption = 'StatusPanel'
    ShowCaption = False
    TabOrder = 2
    object ProgLabel: TLabel
      Left = 85
      Top = 6
      Width = 52
      Height = 15
      Caption = 'Analyzing'
    end
    object ProgBar: TProgressBar
      Left = 6
      Top = 6
      Width = 73
      Height = 16
      DoubleBuffered = False
      ParentDoubleBuffered = False
      Smooth = True
      Style = pbstMarquee
      MarqueeInterval = 30
      TabOrder = 0
    end
  end
  object AnalyzePopupMenu: TPopupMenu
    Images = LintPlugin.LintImages
    Left = 635
    Top = 310
    object AnalyzeCurrentFile1: TMenuItem
      Action = LintPlugin.ActionAnalyzeActiveFile
    end
    object AnalyzeOpenFiles1: TMenuItem
      Action = LintPlugin.ActionAnalyzeOpenFiles
    end
  end
end
