object FormAzureRelayReceiver: TFormAzureRelayReceiver
  Left = 0
  Top = 0
  Caption = 'FormAzureRelayReceiver'
  ClientHeight = 314
  ClientWidth = 494
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    494
    314)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 478
    Height = 39
    Caption = 
      'This demo shows how to set up an Azure Relay receiver using the ' +
      'AzureRelayReceiver component. First you must configure your Azur' +
      'e Relay credentials in the demo code. Then click Start to connec' +
      't to the relay service.'
    WordWrap = True
  end
  object Button1: TButton
    Left = 8
    Top = 53
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object txtLog: TMemo
    Left = 8
    Top = 84
    Width = 478
    Height = 222
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Button2: TButton
    Left = 89
    Top = 53
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = Button2Click
  end
  object AzureRelayReceiver1: TipqAzureRelayReceiver
    SSLCertStore = 'MY'
    OnConnectionConnected = AzureRelayReceiver1ConnectionConnected
    OnConnectionDataIn = AzureRelayReceiver1ConnectionDataIn
    OnConnectionStatus = AzureRelayReceiver1ConnectionStatus
    OnSSLServerAuthentication = AzureRelayReceiver1SSLServerAuthentication
    Left = 304
    Top = 48
  end
end


