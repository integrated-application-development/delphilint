object LintToolWindow: TLintToolWindow
  Left = 0
  Top = 0
  Caption = 'DelphiLint'
  ClientHeight = 316
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object LintToolBar: TGridPanel
    Left = 0
    Top = 0
    Width = 484
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    Caption = 'LintToolBar'
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAbsolute
        Value = 160.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = FileNameLabel
        Row = 0
      end
      item
        Column = 1
        Control = LintPanel
        Row = 0
      end>
    ExpandStyle = emFixedSize
    Padding.Left = 4
    Padding.Right = 4
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      484
      32)
    object FileNameLabel: TLabel
      Left = 8
      Top = 0
      Width = 318
      Height = 29
      AutoSize = False
      Caption = 'No file selected'
      Layout = tlCenter
    end
    object LintPanel: TPanel
      Left = 324
      Top = 0
      Width = 169
      Height = 32
      Anchors = []
      BevelOuter = bvNone
      Caption = 'LintPanel'
      Color = clNone
      ParentBackground = False
      ShowCaption = False
      TabOrder = 0
      DesignSize = (
        169
        32)
      object LintButton: TSpeedButton
        Left = 132
        Top = 4
        Width = 24
        Height = 24
        Anchors = [akTop, akRight]
        ImageIndex = 0
        Images = LintPlugin.LintImages
        OnClick = LintButtonClick
      end
      object ProgLabel: TLabel
        Left = 78
        Top = 8
        Width = 52
        Height = 15
        Alignment = taRightJustify
        Anchors = [akTop, akRight, akBottom]
        Caption = 'Analyzing'
        Layout = tlCenter
      end
      object ProgBar: TProgressBar
        Left = 8
        Top = 8
        Width = 64
        Height = 16
        Anchors = [akLeft, akTop, akRight]
        Smooth = True
        Style = pbstMarquee
        MarqueeInterval = 30
        TabOrder = 0
      end
    end
  end
  object IssueTreeView: TTreeView
    Left = 0
    Top = 32
    Width = 484
    Height = 284
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Indent = 19
    TabOrder = 1
  end
end
