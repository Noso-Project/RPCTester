unit RPCT.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, fpjson, fphttpclient, RPCT.Utils.ParseRPC;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnSend: TButton;
    cbMethod: TComboBox;
    edtServerAddress: TEdit;
    edtParams: TEdit;
    edtReference: TEdit;
    edtAmount: TEdit;
    gbServer: TGroupBox;
    gbMethod: TGroupBox;
    gbParams: TGroupBox;
    lblHelp: TLabel;
    memHelp: TMemo;
    memLog: TMemo;
    panControlHelp: TPanel;
    panHelp: TPanel;
    panControl: TPanel;
    edtServerPort: TSpinEdit;
    procedure btnSendClick(Sender: TObject);
    procedure cbMethodChange(Sender: TObject);
  private

  public

  end;

function GetValidID():integer;
function GetJSONToSend(JSONMethod,JSONparams: string;jsonIDnumber:integer):TJSONStringType;
function GetJSONErrorCode(ErrorCode, JSONIdNumber:integer):string;
Function GetJSONErrorString(ErrorCode:integer):string;

var
  frmMain: TfrmMain;

resourcestring
  rsMethodTest =
    'test is the basic request, to check connectivity. testok is received when everithing is ok';
  rsMethodGetAddressBalance =
    'getaddressbalance returns: balance,incoming,outgoing of the specified addresses';
  rsMethodGetOrderData =
    'getorderinfo returns: timestamp,block,receiver,amount,concept of the specified order';

implementation

{$R *.lfm}

{ TfrmMain }

// change tge method combobox
procedure TfrmMain.cbMethodChange(Sender: TObject);
Begin
edtParams.Text:= '';
edtParams.TextHint:= 'Params';
edtParams.Visible:=false;
edtAmount.Visible:=false;
edtReference.Visible:=false;

case cbMethod.ItemIndex of
  0:begin  // test
    memHelp.Text:= rsMethodTest;
    end;
  1:begin  // getaddressbalance
    memHelp.Text:= rsMethodGetAddressBalance;
    edtParams.TextHint:= 'Params';
    edtParams.Visible:=true;
    end;
  2:begin  // getorderdata
    memHelp.Text := rsMethodGetOrderData;
    edtParams.TextHint:= 'Params';
    edtParams.Visible:=true;
    end;
  3:begin  // getblockinfo
    edtParams.TextHint:= 'Params';
    edtParams.Visible:=true;
    end;
  4:begin  // getmininginfo
    end;
  5:begin  // getpendingorders
    end;
  6:begin  // getmainnetinfo
    end;
  7:begin  // getblockorders
    edtParams.TextHint:= 'Params';
    edtParams.Visible:=true;
    end;
  8:begin  // getnewaddress
    edtParams.TextHint:= 'Params';
    edtParams.Visible:=true;
    end;
  9:begin // sendfunds
    edtParams.TextHint:= 'Recipient';
    edtParams.Visible:=true;
    edtAmount.Visible:=true;
    edtReference.Visible:=true;
    end;
end;
End;

// Returns a valid JSON ID using timestamp
function GetValidID():integer;
const
  id:integer = 0;
Begin
Inc(id);
Result := id;
End;

// REtuns a valid JSON string
function GetJSONToSend(JSONMethod,JSONparams: string;jsonIDnumber:integer):TJSONStringType;
var
  NewJSON : TJSONObject;
  myParams: TStringArray;
  counter : integer;
  paramsarray :  TJSONArray;
Begin
Result := '';
paramsarray := TJSONArray.Create;
if length(JSONparams)>0 then myParams:= JSONparams.Split(' ');
if JSONMethod = '' Then JSONMethod := 'test';
NewJSON := TJSONObject.Create;
   try
      try
      NewJSON.Add('jsonrpc', '2.0');
      NewJSON.Add('method', JSONMethod);
      if length(myparams) > 0 then
         for counter := low(myParams) to high(myParams) do
            if myParams[counter] <>'' then paramsarray.Add(myParams[counter]);
      NewJSON.Add('params', paramsarray);
      NewJSON.Add('id', jsonIDnumber);
      Except on E:Exception do
         begin
         Result := 'ERROR: '+E.Message;
         NewJSON.Free;
         exit;
         end;
      end;
   finally
   Result := NewJSON.AsJSON;
   NewJSON.Free;
   end;
End;

// SENT THE JSON OBJECT
procedure TfrmMain.btnSendClick(Sender: TObject);
var
  RPCClient: TFPHTTPClient;
  JSONtoSEND : TJSONStringType;
  Resultado : String = '';
  RequestBodyStream: TStringStream;
  { No need for the params string list }
begin
JSONtoSEND := GetJSONToSend(cbMethod.Items[cbMethod.ItemIndex],edtParams.Text,GetValidID);
memLog.Lines.Add('-->');
memLog.Lines.Add(JSONtoSEND);
RPCClient := TFPHTTPClient.Create(self);
RPCClient.IOTimeout:=60000; // <-- THIS is too restrictive, ONLY needed if you get stuck for more than 1 minute
   try
      try
      RPCClient.AllowRedirect := True; // <-- I always forget this LOL!!
      RequestBodyStream:= TStringStream.Create(JSONtoSEND, TEncoding.UTF8);
      RPCClient.RequestBody:= RequestBodyStream;
      Resultado := RPCClient.Post('http://'+edtServerAddress.Text+':'+edtServerPort.Text);
      memLog.Lines.Add('<--');
      memLog.Lines.Add(Resultado);
      Except on E:Exception do
        memLog.Lines.Add('Error: '+E.Message);
      end;
   finally
   RequestBodyStream.Free;
   RPCClient.Free;
   end;
End;

function GetJSONErrorCode(ErrorCode, JSONIdNumber:integer):string;
var
  JSONResultado,JSONErrorObj: TJSONObject;
Begin
  result := '';
JSONResultado := TJSONObject.Create;
JSONErrorObj  := TJSONObject.Create;
   try
   JSONResultado.Add('jsonrpc', TJSONString.Create('2.0'));
   JSONErrorObj.Add('code', TJSONIntegerNumber.Create(ErrorCode));
   JSONErrorObj.Add('message', TJSONString.Create(GetJSONErrorString(ErrorCode)));
   JSONResultado.Add('error',JSONErrorObj);
   JSONResultado.Add('id', TJSONIntegerNumber.Create(JSONIdNumber));
   finally
   result := JSONResultado.AsJSON;
   JSONResultado.Free;
   end;
End;

Function GetJSONErrorString(ErrorCode:integer):string;
Begin
if ErrorCode = 400 then result := 'Bad Request'
{...}
else result := 'Unknow error code';
End;



END.

