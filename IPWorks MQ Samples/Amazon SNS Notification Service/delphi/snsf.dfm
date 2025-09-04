object FormSns: TFormSns
  Left = 0
  Top = 0
  Caption = 'FormSns'
  ClientHeight = 614
  ClientWidth = 626
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnActivate = OnActive
  OnResize = OnResize
  DesignSize = (
    626
    614)
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 5
    Top = 0
    Width = 613
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = 'AWS Account Info'
    TabOrder = 0
    DesignSize = (
      613
      57)
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 61
      Height = 15
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Access Key:'
    end
    object Label2: TLabel
      Left = 312
      Top = 24
      Width = 57
      Height = 15
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Secret Key:'
    end
    object txtAccessKey: TEdit
      Left = 83
      Top = 21
      Width = 198
      Height = 23
      Anchors = [akLeft, akTop, akBottom]
      TabOrder = 0
    end
    object txtSecretKey: TEdit
      Left = 375
      Top = 21
      Width = 218
      Height = 23
      Anchors = [akLeft, akTop, akBottom]
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 63
    Width = 613
    Height = 162
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Topics and Subscriptions'
    TabOrder = 1
    DesignSize = (
      613
      162)
    object lvListV: TListView
      Left = 16
      Top = 24
      Width = 465
      Height = 121
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <>
      RowSelect = True
      TabOrder = 0
      OnSelectItem = OnSelectItem
    end
    object btnListTopic: TButton
      Left = 496
      Top = 24
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'List Topics'
      TabOrder = 1
      OnClick = btnListTopicClick
    end
    object btnListSub: TButton
      Left = 496
      Top = 55
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'List Subscriptions'
      TabOrder = 2
      OnClick = btnListSubClick
    end
    object btnCreateTopic: TButton
      Left = 496
      Top = 86
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Create Topic'
      TabOrder = 3
      OnClick = btnCreateTopicClick
    end
    object btnDeleteTopic: TButton
      Left = 496
      Top = 117
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Delete Topic'
      TabOrder = 4
      OnClick = btnDeleteTopicClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 231
    Width = 613
    Height = 162
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Subscription Menagement'
    TabOrder = 2
    DesignSize = (
      613
      162)
    object Label3: TLabel
      Left = 16
      Top = 32
      Width = 99
      Height = 15
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Endpoint Protocol:'
    end
    object Label4: TLabel
      Left = 16
      Top = 64
      Width = 58
      Height = 15
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Topic ARN:'
    end
    object Label5: TLabel
      Left = 16
      Top = 96
      Width = 181
      Height = 15
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Endpoint i.e email add, phone nr..:'
    end
    object Label6: TLabel
      Left = 16
      Top = 128
      Width = 96
      Height = 15
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Subscription ARN:'
    end
    object cboProtocol: TComboBox
      Left = 203
      Top = 32
      Width = 145
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      Items.Strings = (
        'Email'
        'Email-JSON'
        'HTTP'
        'HTTPS'
        'SMS'
        'SQS')
    end
    object txtTopicArn: TEdit
      Left = 203
      Top = 61
      Width = 278
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 1
    end
    object txtEndPoint: TEdit
      Left = 203
      Top = 90
      Width = 278
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 2
    end
    object txtSubArn: TEdit
      Left = 203
      Top = 119
      Width = 278
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 3
    end
    object btnSub: TButton
      Left = 496
      Top = 88
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Subscribe'
      TabOrder = 4
      OnClick = btnSubClick
    end
    object btnUnSub: TButton
      Left = 496
      Top = 119
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Unsubscribe'
      TabOrder = 5
      OnClick = btnUnSubClick
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 404
    Width = 613
    Height = 202
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Publish'
    TabOrder = 3
    DesignSize = (
      613
      202)
    object Label7: TLabel
      Left = 16
      Top = 32
      Width = 58
      Height = 15
      Caption = 'Topic ARN:'
    end
    object Label8: TLabel
      Left = 16
      Top = 64
      Width = 91
      Height = 15
      Caption = 'Message Subject:'
    end
    object Label9: TLabel
      Left = 16
      Top = 96
      Width = 49
      Height = 15
      Caption = 'Message:'
    end
    object txtTopicArnP: TEdit
      Left = 123
      Top = 23
      Width = 358
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
    end
    object txtSubject: TEdit
      Left = 123
      Top = 60
      Width = 358
      Height = 23
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 1
    end
    object txtMessage: TEdit
      Left = 123
      Top = 97
      Width = 358
      Height = 90
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 2
    end
    object btnPublish: TButton
      Left = 496
      Top = 24
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'btnPublish'
      TabOrder = 3
      OnClick = PublishOnClick
    end
  end
  object ipqAmazonSNS1: TipqAmazonSNS
    SSLAcceptServerCertStore = 'MY'
    SSLCertStore = 'MY'
    OnSubscriptionList = OnSubList
    OnTopicList = OnTopicList
    Left = 528
    Top = 247
  end
end


