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
unit azurerelayreceiverf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ipqcore, ipqtypes, ipqazurerelayreceiver;

type
  TFormAzureRelayReceiver = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    txtLog: TMemo;
    AzureRelayReceiver1: TipqAzureRelayReceiver;

    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure OnIdle(Sender: TObject; var Done: Boolean);

    procedure AzureRelayReceiver1ConnectionConnected(
      Sender: TObject;
      ConnectionId: Integer
    );
    procedure AzureRelayReceiver1ConnectionStatus(
      Sender: TObject;
      const ConnectionEvent: String;
      StatusCode: Integer;
      const Description: String
    );
    procedure Button2Click(Sender: TObject);
    procedure AzureRelayReceiver1SSLServerAuthentication(Sender: TObject;
      const CertEncoded: string; const CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    procedure AzureRelayReceiver1ConnectionDataIn(Sender: TObject; ConnectionId,
      DataFormat: Integer; const Text: string; const TextB: TBytes;
      EOM: Boolean);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAzureRelayReceiver: TFormAzureRelayReceiver;

implementation

{$R *.dfm}

procedure TFormAzureRelayReceiver.OnIdle(Sender: TObject; var Done: Boolean);
begin
  Done:=False;
  AzureRelayReceiver1.DoEvents;
end;

procedure TFormAzureRelayReceiver.Button1Click(Sender: TObject);
begin
  // Configure your Azure Relay credentials here
  AzureRelayReceiver1.AccessKey := '...';
  AzureRelayReceiver1.AccessKeyName := '...';
  AzureRelayReceiver1.NamespaceAddress := '...';
  AzureRelayReceiver1.HybridConnection := 'hc1';
  AzureRelayReceiver1.Listening := True;
end;

procedure TFormAzureRelayReceiver.Button2Click(Sender: TObject);
begin
  AzureRelayReceiver1.Listening := False;
end;

procedure TFormAzureRelayReceiver.AzureRelayReceiver1ConnectionConnected(Sender: TObject; ConnectionId: Integer);
begin
   txtLog.Lines.Add(IntToStr(ConnectionId) + ' connected.');
end;

procedure TFormAzureRelayReceiver.AzureRelayReceiver1ConnectionDataIn(
  Sender: TObject; ConnectionId, DataFormat: Integer; const Text: string;
  const TextB: TBytes; EOM: Boolean);
begin
  txtLog.Lines.Add('Echoing "' +Text+ '" to ' + IntToStr(ConnectionId));
  AzureRelayReceiver1.Send(ConnectionId, TextB);
end;

procedure TFormAzureRelayReceiver.AzureRelayReceiver1ConnectionStatus(Sender: TObject; const ConnectionEvent: string; StatusCode: Integer; const Description: string);
begin
   txtLog.Lines.Add(ConnectionEvent);
end;

procedure TFormAzureRelayReceiver.AzureRelayReceiver1SSLServerAuthentication(
  Sender: TObject; const CertEncoded: string; const CertEncodedB: TBytes;
  const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept := True;
end;

end.

