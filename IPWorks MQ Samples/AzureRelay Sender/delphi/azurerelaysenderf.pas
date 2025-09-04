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
unit azurerelaysenderf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ipqcore, ipqtypes, ipqazurerelaysender;

type
  TFormAzureRelaySender = class(TForm)
    AzureRelaySender1: TipqAzureRelaySender;
    Label1: TLabel;
    ConnectButton: TButton;
    Label2: TLabel;
    Label3: TLabel;
    InputBox: TMemo;
    OutputBox: TMemo;
    DisconnectButton: TButton;
    SendButton: TButton;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure AzureRelaySender1ConnectionStatus(Sender: TObject;
      const ConnectionEvent: string; StatusCode: Integer;
      const Description: string);
    procedure AzureRelaySender1Disconnected(Sender: TObject;
      StatusCode: Integer; const Description: string);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure AzureRelaySender1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    procedure AzureRelaySender1DataIn(Sender: TObject; DataFormat: Integer;
      const Text: string; const TextB: TBytes; EOM, EOL: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAzureRelaySender: TFormAzureRelaySender;

implementation

{$R *.dfm}

procedure TFormAzureRelaySender.OnIdle(Sender: TObject; var Done: Boolean);
begin
  Done:=False;
  AzureRelaySender1.DoEvents;
end;

procedure TFormAzureRelaySender.AzureRelaySender1ConnectionStatus(
  Sender: TObject; const ConnectionEvent: string; StatusCode: Integer;
  const Description: string);
begin
  OutputBox.Lines.Add(ConnectionEvent);
end;

procedure TFormAzureRelaySender.AzureRelaySender1DataIn(Sender: TObject;
  DataFormat: Integer; const Text: string; const TextB: TBytes; EOM,
  EOL: Boolean);
begin
  OutputBox.Lines.Add('Received ' + Text + ' from ' + AzureRelaySender1.HybridConnection);
end;

procedure TFormAzureRelaySender.AzureRelaySender1Disconnected(Sender: TObject;
  StatusCode: Integer; const Description: string);
begin
 OutputBox.Lines.Add('Disconnected');
end;

procedure TFormAzureRelaySender.AzureRelaySender1SSLServerAuthentication(
  Sender: TObject; const CertEncoded: string; const CertEncodedB: TBytes;
  const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
begin
  if (not Accept) then
    Accept := True;
end;

procedure TFormAzureRelaySender.ConnectButtonClick(Sender: TObject);
begin
  // Configure your own Azure Relay credentials here
  AzureRelaySender1.AccessKey := '...';
  AzureRelaySender1.AccessKeyName := '...';
  AzureRelaySender1.NamespaceAddress := '...';
  AzureRelaySender1.HybridConnection := 'hc1';
  AzureRelaySender1.Connect();
  ConnectButton.Enabled := False;
end;

procedure TFormAzureRelaySender.DisconnectButtonClick(Sender: TObject);
begin
  AzureRelaySender1.Disconnect();
  ConnectButton.Enabled := True;
  DisconnectButton.Enabled := False;
end;

procedure TFormAzureRelaySender.SendButtonClick(Sender: TObject);
begin
  OutputBox.Lines.Add('Sending ' + InputBox.Text + ' to receiver');
  AzureRelaySender1.SendText(InputBox.Text);
end;

end.


