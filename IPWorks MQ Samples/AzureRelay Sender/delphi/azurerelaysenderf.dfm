object FormAzureRelaySender: TFormAzureRelaySender
  Left = 0
  Top = 0
  Caption = 'Azure Relay Sender Demo'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 619
    Height = 25
    AutoSize = False
    Caption = 
      'This demo shows how to set up an Azure Relay sender using the Az' +
      'ureRelaySender component. First you must configure your Azure Re' +
      'lay credentials in the demo code. Then click connect to establis' +
      'h a connection with the relay receiver.'
    Color = clBtnFace
    ParentColor = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 101
    Width = 136
    Height = 13
    Caption = 'Enter the text to send here:'
  end
  object Label3: TLabel
    Left = 327
    Top = 101
    Width = 121
    Height = 13
    Caption = 'Data from relay receiver:'
  end
  object ConnectButton: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 0
    OnClick = ConnectButtonClick
  end
  object DisconnectButton: TButton
    Left = 89
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 1
    OnClick = DisconnectButtonClick
  end
  object InputBox: TMemo
    Left = 8
    Top = 120
    Width = 300
    Height = 171
    Lines.Strings = (
      'Hello World!')
    TabOrder = 2
  end
  object OutputBox: TMemo
    Left = 327
    Top = 120
    Width = 300
    Height = 171
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object SendButton: TButton
    Left = 150
    Top = 89
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 4
    OnClick = SendButtonClick
  end
  object AzureRelaySender1: TipqAzureRelaySender
    SSLCertStore = 'MY'
    OnConnectionStatus = AzureRelaySender1ConnectionStatus
    OnDataIn = AzureRelaySender1DataIn
    OnDisconnected = AzureRelaySender1Disconnected
    OnSSLServerAuthentication = AzureRelaySender1SSLServerAuthentication
    Left = 536
    Top = 48
  end
end


