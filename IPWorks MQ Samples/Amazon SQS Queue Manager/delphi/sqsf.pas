(*
 * IPWorks MQ 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks MQ in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksmq
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit sqsf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IdBaseComponent, IdMessage,
  ipqamazonsqs, ipqcore, ipqtypes, Vcl.ComCtrls;

type
    TFormSqs = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    AccessKey: TEdit;
    SecretKey: TEdit;
    Auth: TButton;
    GroupBox2: TGroupBox;
    Button2: TButton;
    Button3: TButton;
    GroupBox3: TGroupBox;
    ReadMsg: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MsgId: TEdit;
    RecHandle: TEdit;
    MsgData: TEdit;
    DeleteMsg: TButton;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    MsgBody: TEdit;
    CreateMsg: TButton;
    ipqAmazonSQS1: TipqAmazonSQS;
    QueueList: TListView;
    procedure AuthClick(Sender: TObject);
    procedure TQueueEvent(Sender: TObject; const QueueId, URL: string);
    procedure CreateQClick(Sender: TObject);
    procedure QueueListSelected(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure DeleteQClick(Sender: TObject);
    procedure CreateMsgClick(Sender: TObject);
    procedure ReadMsgClick(Sender: TObject);
    procedure TMessageEvent(Sender: TObject; const MessageId, ReceiptHandle,
      MessageData, MD5: string);
    procedure DeleteMsgClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSqs: TFormSqs;
  myQueueId: string;
  myRecHandle: string;
  myMsgId: string;

implementation

{$R *.dfm}

procedure TFormSqs.AuthClick(Sender: TObject);
begin
  ipqAmazonSQS1.AccessKey := AccessKey.Text;
  ipqAmazonSQS1.SecretKey := SecretKey.Text;
  ipqAmazonSQS1.ListQueues();
end;

procedure TFormSqs.DeleteMsgClick(Sender: TObject);
var
  Result: Integer;
begin
  if myRecHandle <> '' then
  begin
    Result := MessageDlg('Delete Message with ID ' + myMsgId + '?', mtConfirmation, [mbYes, mbNo], 0);
    if Result = mrYes then
    begin
      ipqAmazonSQS1.DeleteMessage(myQueueId, myRecHandle);
      MsgId.Text := '';
      RecHandle.Text := '';
      MsgData.Text := '';
    end;
  end
  else
  begin
    MessageDlg('Nothing to delete!', mtError, [mbOK], 0);
  end;
end;

procedure TFormSqs.DeleteQClick(Sender: TObject);
var
  Result: Integer;
begin
  if myQueueId <> '' then
  begin
    Result := MessageDlg('Delete Queue ' + myQueueId + '?', mtConfirmation, [mbYes, mbNo], 0);
    if Result = mrYes then
    begin
      ipqAmazonSQS1.DeleteQueue(myQueueId);
      QueueList.Items.Clear;
      ipqAmazonSQS1.ListQueues();
    end;
  end
  else
  begin
    MessageDlg('Nothing selected!', mtError, [mbOK], 0);
  end;
end;

procedure TFormSqs.CreateMsgClick(Sender: TObject);
begin
  ShowMessage('Msg Id: ' + ipqAmazonSQS1.CreateMessage(myQueueId, MsgBody.Text));
  MsgBody.Text := '';
end;

procedure TFormSqs.CreateQClick(Sender: TObject);
var
  UserInput: string;
begin
  if InputQuery('User Input', 'Name of the Queue: ', UserInput) then
  begin
    ShowMessage('ID: ' +  ipqAmazonSQS1.CreateQueue(UserInput));
    QueueList.Items.Clear;
    ipqAmazonSQS1.ListQueues();
  end;
end;

procedure TFormSqs.QueueListSelected(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
  begin
    myQueueId := Item.Caption;
  end;
end;

procedure TFormSqs.ReadMsgClick(Sender: TObject);
begin
  if myQueueId <> '' then
  begin
    ipqAmazonSQS1.ListMessages(myQueueId);
  end
  else
  begin
    MessageDlg('Nothing selected!', mtError, [mbOK], 0);
  end;
end;

procedure TFormSqs.TMessageEvent(Sender: TObject; const MessageId, ReceiptHandle,
  MessageData, MD5: string);
begin
  MsgId.Text := MessageId;
  RecHandle.Text := ReceiptHandle;
  MsgData.Text := MessageData;

  myRecHandle := ReceiptHandle;
  myMsgId := MessageId;
end;

procedure TFormSqs.TQueueEvent(Sender: TObject; const QueueId, URL: string);
var
  ListItem: TListItem;
begin
  QueueList.ViewStyle := vsReport;
  ListItem := QueueList.Items.Add;
  ListItem.Caption := QueueId;
  ListItem.SubItems.Add(URL);
end;

end.

