object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Delphi_SCREEN_CAPTURE v1.0'
  ClientHeight = 671
  ClientWidth = 923
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesktopCenter
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 907
    Height = 441
    Stretch = True
  end
  object messagesLog: TMemo
    Left = 8
    Top = 518
    Width = 911
    Height = 147
    Enabled = False
    Lines.Strings = (
      'messagesLog')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 8
    Top = 455
    Width = 911
    Height = 57
    TabOrder = 1
    object btnStartDXGI: TButton
      Left = 353
      Top = 16
      Width = 75
      Height = 25
      Caption = 'StartDXGI'
      TabOrder = 0
      OnClick = btnStartDXGIClick
    end
    object btnStartGDI: TButton
      Left = 612
      Top = 16
      Width = 75
      Height = 25
      Caption = 'StartGDI'
      TabOrder = 1
      OnClick = btnStartGDIClick
    end
    object btnStartGDIDirty: TButton
      Left = 693
      Top = 16
      Width = 75
      Height = 25
      Caption = 'StartGDIDirty'
      TabOrder = 2
      OnClick = btnStartGDIDirtyClick
    end
    object btnStopDXGI: TButton
      Left = 531
      Top = 16
      Width = 75
      Height = 25
      Caption = 'StopDXGI'
      TabOrder = 3
      OnClick = btnStopDXGIClick
    end
    object btnStopGDI: TButton
      Left = 774
      Top = 16
      Width = 75
      Height = 25
      Caption = 'StopGDI'
      TabOrder = 4
      OnClick = btnStopGDIClick
    end
    object ComboBox1: TComboBox
      Left = 7
      Top = 16
      Width = 322
      Height = 23
      TabOrder = 5
      Text = 'ComboBox1'
      OnChange = ComboBox1Change
    end
    object btnStartDXGIDirty: TButton
      Left = 436
      Top = 16
      Width = 89
      Height = 25
      Caption = 'StartDXGIDirty'
      TabOrder = 6
      OnClick = btnStartDXGIDirtyClick
    end
  end
end
