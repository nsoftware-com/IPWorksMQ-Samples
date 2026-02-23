object FormSqs: TFormSqs
  Left = 0
  Top = 0
  Caption = 'Amazon SQS Queue Manager'
  ClientHeight = 561
  ClientWidth = 564
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 545
    Height = 105
    Caption = 'Amazon Web Services Authentication'
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 32
      Width = 102
      Height = 15
      Caption = 'AWS Access Key ID:'
    end
    object Label2: TLabel
      Left = 24
      Top = 72
      Width = 84
      Height = 15
      Caption = 'AWS Secret Key:'
    end
    object AccessKey: TEdit
      Left = 160
      Top = 29
      Width = 281
      Height = 23
      TabOrder = 0
    end
    object SecretKey: TEdit
      Left = 160
      Top = 69
      Width = 281
      Height = 23
      TabOrder = 1
    end
    object Auth: TButton
      Left = 456
      Top = 68
      Width = 75
      Height = 25
      Caption = 'Go'
      TabOrder = 2
      OnClick = AuthClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 119
    Width = 548
    Height = 169
    Caption = 'Queues'
    TabOrder = 1
    object Button2: TButton
      Left = 448
      Top = 32
      Width = 83
      Height = 25
      Caption = 'CreateQ'
      TabOrder = 0
      OnClick = CreateQClick
    end
    object Button3: TButton
      Left = 448
      Top = 72
      Width = 83
      Height = 25
      Caption = 'Delete Queue'
      TabOrder = 1
      OnClick = DeleteQClick
    end
    object QueueList: TListView
      Left = 24
      Top = 32
      Width = 418
      Height = 121
      Columns = <
        item
          Caption = 'ID'
          Width = 200
        end
        item
          Caption = 'URL'
          Width = 200
        end>
      TabOrder = 2
      OnSelectItem = QueueListSelected
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 294
    Width = 548
    Height = 177
    Caption = 'Read Messages from Queue'
    TabOrder = 2
    object Label3: TLabel
      Left = 24
      Top = 107
      Width = 76
      Height = 15
      Caption = 'Message Data:'
    end
    object Label4: TLabel
      Left = 24
      Top = 136
      Width = 83
      Height = 15
      Caption = 'Receipt Handle:'
    end
    object Label5: TLabel
      Left = 24
      Top = 75
      Width = 63
      Height = 15
      Caption = 'Message ID:'
    end
    object ReadMsg: TButton
      Left = 24
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Read Msg'
      TabOrder = 0
      OnClick = ReadMsgClick
    end
    object MsgId: TEdit
      Left = 128
      Top = 72
      Width = 313
      Height = 23
      TabOrder = 1
    end
    object RecHandle: TEdit
      Left = 128
      Top = 130
      Width = 313
      Height = 23
      TabOrder = 3
    end
    object MsgData: TEdit
      Left = 128
      Top = 101
      Width = 313
      Height = 23
      TabOrder = 2
    end
    object DeleteMsg: TButton
      Left = 456
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 4
      OnClick = DeleteMsgClick
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 477
    Width = 545
    Height = 76
    Caption = 'Add New Messages to Queue'
    TabOrder = 3
    object Label6: TLabel
      Left = 24
      Top = 40
      Width = 76
      Height = 15
      Caption = 'Message Data:'
    end
    object MsgBody: TEdit
      Left = 128
      Top = 32
      Width = 313
      Height = 23
      TabOrder = 0
    end
    object CreateMsg: TButton
      Left = 456
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Add New'
      TabOrder = 1
      OnClick = CreateMsgClick
    end
  end
  object ipqAmazonSQS1: TipqAmazonSQS
    SSLAcceptServerCertStore = 'MY'
    SSLCertStore = 'MY'
    OnMessage = TMessageEvent
    OnQueue = TQueueEvent
    Left = 240
    Top = 320
  end
end


