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
  object IssueTreeView: TTreeView
    Left = 0
    Top = 51
    Width = 484
    Height = 265
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Indent = 19
    TabOrder = 0
  end
  object LintPanel: TPanel
    Left = 0
    Top = 0
    Width = 484
    Height = 51
    Align = alTop
    BevelOuter = bvNone
    Caption = 'LintPanel'
    Color = clNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      484
      51)
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
    object LintButton: TBitBtn
      Left = 387
      Top = 7
      Width = 84
      Height = 24
      Action = LintPlugin.ActionAnalyzeActiveFile
      Anchors = [akTop, akRight]
      Caption = 'Analyze'
      Images = LintPlugin.LintImages
      ParentDoubleBuffered = True
      TabOrder = 1
    end
  end
end
