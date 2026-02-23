unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormUnit = class(TForm)
    ValueB: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    okClicked: bool;
  end;

var
  FormUnit: TFormUnit;

implementation

{$R *.dfm}

procedure TFormUnit.Button1Click(Sender: TObject);
begin
  okClicked := true;
  Close;
end;

procedure TFormUnit.Button2Click(Sender: TObject);
begin
  okClicked := false;
  Close;
end;

end.
