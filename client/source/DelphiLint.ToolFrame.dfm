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
    TabOrder = 1
    object RulePanel: TPanel
      Left = 443
      Top = 0
      Width = 265
      Height = 294
      Align = alRight
      BevelOuter = bvNone
      Caption = 'RulePanel'
      ShowCaption = False
      TabOrder = 2
      object RuleBrowser: TEdgeBrowser
        Left = 0
        Top = 0
        Width = 265
        Height = 294
        Align = alClient
        TabOrder = 0
        UserDataFolder = '%LOCALAPPDATA%\bds.exe.WebView2'
        OnCreateWebViewCompleted = RuleBrowserCreateWebViewCompleted
        OnNavigationStarting = RuleBrowserNavigationStarting
        OnNewWindowRequested = RuleBrowserNewWindowRequested
      end
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
      TabOrder = 1
      OnMouseDown = SplitPanelMouseDown
      OnMouseMove = SplitPanelMouseMove
      OnMouseUp = SplitPanelMouseUp
    end
    object ResizeIndicatorPanel: TPanel
      Left = 176
      Top = 126
      Width = 185
      Height = 41
      BevelOuter = bvNone
      Caption = 'ResizeIndicatorPanel'
      Color = clGray
      UseDockManager = False
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
      Visible = False
      StyleElements = []
    end
    object IssueControlList: TControlList
      Left = 0
      Top = 0
      Width = 435
      Height = 294
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 38
      ItemMargins.Left = 0
      ItemMargins.Top = 0
      ItemMargins.Right = 0
      ItemMargins.Bottom = 0
      ItemSelectionOptions.SelectedColorAlpha = 240
      ItemSelectionOptions.FocusedColorAlpha = 255
      ItemSelectionOptions.SelectedFontColor = clHighlightText
      ItemSelectionOptions.FocusedFontColor = clHighlightText
      ItemSelectionOptions.UseFontColorForLabels = True
      ParentColor = False
      TabOrder = 3
      OnBeforeDrawItem = IssueControlListBeforeDrawItem
      OnItemClick = IssueControlListItemClick
      OnItemDblClick = IssueControlListItemDblClick
      object IssueMessageLabel: TLabel
        Left = 24
        Top = 4
        Width = 97
        Height = 13
        Caption = 'Issue description'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = True
      end
      object IssueImage: TImage
        Left = 4
        Top = 4
        Width = 14
        Height = 14
      end
      object IssueMetaLabel: TLabel
        Left = 24
        Top = 17
        Width = 79
        Height = 15
        Caption = 'Issue metadata'
      end
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
    TabOrder = 0
    object FileHeadingPanel: TPanel
      Left = 0
      Top = 0
      Width = 708
      Height = 50
      Align = alBottom
      BevelOuter = bvNone
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
        Left = 618
        Top = 0
        Width = 84
        Height = 50
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
        Images = PluginCore.LintImages
        List = True
        ShowCaptions = True
        TabOrder = 1
        object AnalyzeShortButton: TToolButton
          Left = 0
          Top = 0
          Action = PluginCore.ActionAnalyzeShort
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
    BevelOuter = bvNone
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
    Images = PluginCore.LintImages
    Left = 635
    Top = 310
    object AnalyzeCurrentFile1: TMenuItem
      Action = PluginCore.ActionAnalyzeActiveFile
    end
    object AnalyzeOpenFiles1: TMenuItem
      Action = PluginCore.ActionAnalyzeOpenFiles
    end
    object Separator1: TMenuItem
      Caption = '-'
    end
    object ActionClearActiveFile1: TMenuItem
      Action = PluginCore.ActionClearActiveFile
    end
  end
end
