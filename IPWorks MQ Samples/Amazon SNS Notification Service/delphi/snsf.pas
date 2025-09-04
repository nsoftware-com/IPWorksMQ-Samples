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
unit snsf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, ipqcore,
  ipqtypes, ipqamazonsns, Unit2;

type
  TFormSns = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    txtAccessKey: TEdit;
    txtSecretKey: TEdit;
    GroupBox2: TGroupBox;
    lvListV: TListView;
    btnListTopic: TButton;
    btnListSub: TButton;
    btnCreateTopic: TButton;
    btnDeleteTopic: TButton;
    GroupBox3: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtTopicArn: TEdit;
    txtEndPoint: TEdit;
    txtSubArn: TEdit;
    btnSub: TButton;
    btnUnSub: TButton;
    GroupBox4: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    txtTopicArnP: TEdit;
    txtSubject: TEdit;
    txtMessage: TEdit;
    btnPublish: TButton;
    cboProtocol: TComboBox;
    ipqAmazonSNS1: TipqAmazonSNS;
    procedure btnListTopicClick(Sender: TObject);
    procedure OnTopicList(Sender: TObject; const TopicArn: string);
    procedure btnListSubClick(Sender: TObject);
    procedure OnSubList(Sender: TObject; const SubscriptionArn, TopicArn, Owner,
      Endpoint: string; Protocol: Integer);
    procedure btnCreateTopicClick(Sender: TObject);
    procedure OnSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnDeleteTopicClick(Sender: TObject);
    procedure PublishOnClick(Sender: TObject);
    procedure btnSubClick(Sender: TObject);
    procedure btnUnSubClick(Sender: TObject);
    procedure OnResize(Sender: TObject);
    procedure OnActive(Sender: TObject);
  private
    isSubListed: boolean;
    procedure HandleDblClick(Sender: TObject);

    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSns: TFormSns;
  FormUnit: TFormUnit;
  selectedTopicArn: string;

implementation

{$R *.dfm}

procedure TFormSns.btnCreateTopicClick(Sender: TObject);
begin
  FormUnit.ShowModal;
  if FormUnit.okClicked then
  begin
    ShowMessage('Topic ARN: ' +  ipqAmazonSNS1.CreateTopic(FormUnit.Edit1.Text));
    btnListTopicClick(Sender);
  end;
end;

procedure TFormSns.btnDeleteTopicClick(Sender: TObject);
var
  Result: Integer;
begin
  if selectedTopicArn <> '' then
  begin
    Result := MessageDlg('Delete Topic: ' + selectedTopicArn + '?', mtConfirmation, [mbYes, mbNo], 0);
    if Result = mrYes then
      begin
        ipqAmazonSNS1.DeleteTopic(selectedTopicArn);
        btnListTopicClick(Sender);
      end;
  end
  else
  begin
    MessageDlg('Nothing selected!', mtError, [mbOK], 0);
  end;
end;

procedure TFormSns.btnListSubClick(Sender: TObject);
var
  Column: TListColumn;
  Column1: TListColumn;
  Column2: TListColumn;
  Column3: TListColumn;
  Column4: TListColumn;
  width: integer;
begin
  ipqAmazonSNS1.AccessKey := txtAccessKey.Text;
  ipqAmazonSNS1.SecretKey := txtSecretKey.Text;

  txtTopicArn.Text := '';
  txtTopicArnP.Text := '';
  txtEndpoint.Text := '';
  txtSubArn.Text := '';
  cboProtocol.Text := '';

  width := (Self.Width div 10) + 60;

  lvListV.ViewStyle := vsReport;
  lvListV.Columns.Clear;
  lvListV.Items.Clear;

  Column := lvListV.Columns.Add;
  Column.Caption := 'Topic ARN';
  Column.Width := width;

  Column1 := lvListV.Columns.Add;
  Column1.Caption := 'Subscription ARN:';
  Column1.Width := width;

  Column2 := lvListV.Columns.Add;
  Column2.Caption := 'Endpoint';
  Column2.Width := width;

  Column3 := lvListV.Columns.Add;
  Column3.Caption := 'Owner';
  Column3.Width := width;

  Column4 := lvListV.Columns.Add;
  Column4.Caption := 'EndPoint protocol';
  Column4.Width := width;

  isSubListed := true;

  repeat ipqAmazonSNS1.ListSubscriptions();
  until ipqAmazonSNS1.SubscriptionMarker = '';
end;

procedure TFormSns.btnListTopicClick(Sender: TObject);
var
  Column: TListColumn;
begin
  ipqAmazonSNS1.AccessKey := txtAccessKey.Text;
  ipqAmazonSNS1.SecretKey := txtSecretKey.Text;

  txtTopicArn.Text := '';
  txtTopicArnP.Text := '';
  txtEndpoint.Text := '';
  txtSubArn.Text := '';
  cboProtocol.Text := '';

  lvListV.ViewStyle := vsReport;
  lvListV.Columns.Clear;
  lvListV.Items.Clear;
  Column := lvListV.Columns.Add;
  Column.Caption := 'Topic ARN';
  Column.Width := 500;

  isSubListed := false;

  repeat ipqAmazonSNS1.ListTopics();
  until ipqAmazonSNS1.TopicMarker = '';

  lvListV.OnDblClick := HandleDblClick;
end;

procedure TFormSns.HandleDblClick(Sender: TObject);
var
  Column: TListColumn;
  Column1: TListColumn;
  Column2: TListColumn;
  Column3: TListColumn;
  Column4: TListColumn;
  width: integer;
begin
  width := (Self.Width div 10) + 60;

  if not isSubListed then
  begin
    lvListV.ViewStyle := vsReport;
    lvListV.Columns.Clear;
    lvListV.Items.Clear;

    txtTopicArn.Text := '';
    txtTopicArnP.Text := '';
    txtEndpoint.Text := '';
    txtSubArn.Text := '';
    cboProtocol.Text := '';

    Column := lvListV.Columns.Add;
    Column.Caption := 'Topic ARN';
    Column.Width := width;

    Column1 := lvListV.Columns.Add;
    Column1.Caption := 'Subscription ARN:';
    Column1.Width := width;

    Column2 := lvListV.Columns.Add;
    Column2.Caption := 'Endpoint';
    Column2.Width := width;

    Column3 := lvListV.Columns.Add;
    Column3.Caption := 'Owner';
    Column3.Width := width;

    Column4 := lvListV.Columns.Add;
    Column4.Caption := 'EndPoint protocol';
    Column4.Width := width;

    ipqAmazonSNS1.ListTopicSubscriptions(selectedTopicArn);

    isSubListed := true;
  end;
end;

procedure TFormSns.OnActive(Sender: TObject);
begin
  Constraints.MinWidth := Self.Width;
  Constraints.MinHeight := Self.Height;
end;

procedure TFormSns.OnResize(Sender: TObject);
var
  I: Integer;
begin
  if isSubListed then
  begin
    for I := 0 to lvListV.Columns.Count - 1 do
    begin
      lvListV.Columns[I].Width := (Self.Width div 10) + 60;
    end;
  end;
end;

procedure TFormSns.OnSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
    begin
      selectedTopicArn := Item.Caption;
      txtTopicArn.Text := Item.Caption;
      txtTopicArnP.Text := Item.Caption;
      if isSubListed then
      begin
        cboProtocol.Text := Item.SubItems[3];
        txtSubArn.Text := Item.SubItems[0];
        txtEndpoint.Text := Item.SubItems[1];
      end;
    end;
end;

procedure TFormSns.OnSubList(Sender: TObject; const SubscriptionArn, TopicArn,
  Owner, Endpoint: string; Protocol: Integer);
var
  ListItem: TListItem;
  protocolStr: string;
begin
  txtTopicArn.Text := '';
  txtTopicArnP.Text := '';
  txtEndpoint.Text := '';
  txtSubArn.Text := '';
  cboProtocol.Text := '';

  case Protocol of
    0: protocolStr := 'Email';
    1: protocolStr := 'Email-JSON';
    2: protocolStr := 'HTTP';
    3: protocolStr := 'HTTPS';
    4: protocolStr := 'SMS';
    5: protocolStr := 'SQS';
  else
    protocolStr := 'not known protocol';
  end;

  ListItem := lvListV.Items.Add;
  ListItem.Caption := TopicArn;
  ListItem.SubItems.Add(SubscriptionArn);
  ListItem.SubItems.Add(Endpoint);
  ListItem.SubItems.Add(Owner);
  ListItem.SubItems.Add(protocolStr);
end;

procedure TFormSns.OnTopicList(Sender: TObject; const TopicArn: string);
var
  ListItem: TListItem;
begin
  ListItem := lvListV.Items.Add;
  ListItem.Caption := TopicArn;
end;

procedure TFormSns.PublishOnClick(Sender: TObject);
begin
  ShowMessage('Message ID: ' +  ipqAmazonSNS1.Publish(txtTopicArnP.Text, txtSubject.Text, txtMessage.Text));
  txtTopicArnP.Text := '';
  txtSubject.Text := '';
  txtMessage.Text := '';
end;

procedure TFormSns.btnSubClick(Sender: TObject);
var
  protocolInt: Integer;
begin
  if cboProtocol.Text = 'Email' then
  begin
    protocolInt := 0;
  end
  else if cboProtocol.Text = 'Email-JSON' then
  begin
    protocolInt := 1;
  end
  else if cboProtocol.Text = 'HTTP' then
  begin
    protocolInt := 2;
  end
  else if cboProtocol.Text = 'HTTPS' then
  begin
    protocolInt := 3;
  end
  else if cboProtocol.Text = 'SMS' then
  begin
    protocolInt := 4;
  end
  else if cboProtocol.Text = 'SQS' then
  begin
    protocolInt := 5;
  end
  else
  begin
    protocolInt := -1;
  end;

  ShowMessage('Subscription ARN for new subscriber: ' +  ipqAmazonSNS1.Subscribe(txtTopicArn.Text, txtEndpoint.Text, protocolInt));
  txtTopicArn.Text := '';
  txtEndpoint.Text := '';
  cboProtocol.Text := '';
end;

procedure TFormSns.btnUnSubClick(Sender: TObject);
begin
  ipqAmazonSNS1.Unsubscribe(txtSubArn.Text);
  ShowMessage('Unsubscribed succesfully user: ' +  txtSubArn.Text);
  txtSubArn.Text := '';
end;

end.
