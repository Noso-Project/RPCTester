unit RPCT.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, fpjson, fphttpclient;

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
    procedure FormCreate(Sender: TObject);
  private
    procedure ClearInputs;
    procedure DisplayHelp;
  public

  end;

function GetValidID():integer;
function GetJSONToSend(JSONMethod,JSONparams: string;jsonIDnumber:integer):TJSONStringType;
function GetJSONErrorCode(ErrorCode, JSONIdNumber:integer):string;
Function GetJSONErrorString(ErrorCode:integer):string;

var
  frmMain: TfrmMain;

const
  cDefaultServerAddress = 'localhost';
  cVersion = '0.1';

resourcestring
  rsFormCaption = 'Noso RPC Tester';
  rsTextHintParams = 'Params';
  rsTextHintRecipient = 'Recipent';
  rsTextHintAmount = 'Amount';
  rsTextHintReference = 'Reference';
  rsMethodTest =
    'test is the basic request, to check connectivity. testok is received when'+
    ' everithing is ok.';
  rsMethodGetAddressBalance =
    'getaddressbalance returns: balance,incoming,outgoing of the specified'+
    ' addresses.';
  rsMethodGetOrderData =
    'getorderinfo returns: timestamp, block, receiver, amount, reference of '+
    'the specified order.';
  rsMethodGetBlockInfo =
    'getblockinfo returns: number, timestart, timeend, timetotal, last20, '+
    'totaltransactions, difficulty, target, solution, lastblockhash, '+
    'nextdifficulty, miner, feespayed, reward of the specified block.';
  rsMethodGetMiningInfo =
    'getmininginfo returns: block, target, difficulty of the block being '+
    'mined.';
  rsMethodGetPendingOrders =
    'getpendingorders returns: a list of pending orders.';
  rsMethodGetMainnetInfo =
    'getmainnetinfo returns: lastblock, lastblockhash, headershash, '+
    'summaryhash, pending, supply.';
  rsMethodGetBlockOrders =
    'getblockorders returns: a list of the orders on the specified block.';
  rsMethodGetNewAddress =
    'getnewaddress returns: the number of new addresses you''ve specified.';
  rsMethodSendFunds =
    'sendfunds returns: the OrderID if the transaction was successful.';

implementation

{$R *.lfm}

{ TfrmMain }

// change tge method combobox
procedure TfrmMain.cbMethodChange(Sender: TObject);
Begin
ClearInputs;
case cbMethod.ItemIndex of
  0:begin  // test
    end;
  1:begin  // getaddressbalance
    edtParams.Visible:= True;
    end;
  2:begin  // getorderdata
    edtParams.Visible:= True;
    end;
  3:begin  // getblockinfo
    edtParams.Visible:= True;
    end;
  4:begin  // getmininginfo
    end;
  5:begin  // getpendingorders
    end;
  6:begin  // getmainnetinfo
    end;
  7:begin  // getblockorders
    edtParams.Visible:= True;
    end;
  8:begin  // getnewaddress
    edtParams.Visible:= True;
    end;
  9:begin // sendfunds
    edtParams.TextHint:= rsTextHintRecipient;
    edtParams.Visible:= True;
    edtAmount.Visible:= True;
    edtReference.Visible:= True;
    end;
end;
DisplayHelp;
End;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption:= rsFormCaption + ' v' + cVersion;
  edtServerAddress.Text:= cDefaultServerAddress;
  cbMethod.ItemIndex:= 0;
  ClearInputs;
  DisplayHelp;
end;

procedure TfrmMain.ClearInputs;
begin
  edtParams.Visible:= False;
  edtParams.Text:= '';
  edtParams.TextHint:= rsTextHintParams;

  edtAmount.Visible:= False;
  edtAmount.Text:= '';
  edtAmount.TextHint:= rsTextHintAmount;

  edtReference.Visible:= False;
  edtReference.Text:= '';
  edtReference.TextHint:= rsTextHintReference;
end;

procedure TfrmMain.DisplayHelp;
Begin
case cbMethod.ItemIndex of
  0:begin
    memHelp.Text:= rsMethodTest;
    end;
  1:begin
    memHelp.Text:= rsMethodGetAddressBalance;
    end;
  2:begin
    memHelp.Text:= rsMethodGetOrderData;
    end;
  3:begin
    memHelp.Text:= rsMethodGetBlockInfo;
    end;
  4:begin
    memHelp.Text:= rsMethodGetMiningInfo;
    end;
  5:begin
    memHelp.Text:= rsMethodGetPendingOrders;
    end;
  6:begin
    memHelp.Text:= rsMethodGetMainnetInfo;
    end;
  7:begin
    memHelp.Text:= rsMethodGetBlockOrders;
    end;
  8:begin
    memHelp.Text:= rsMethodGetNewAddress;
    end;
  9:begin
    memHelp.Text:= rsMethodSendFunds;
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
if cbMethod.ItemIndex = 9 then
   begin
   JSONtoSEND := GetJSONToSend(cbMethod.Items[cbMethod.ItemIndex],edtParams.Text+
   ' '+edtamount.Text+' '+edtreference.Text,GetValidID);
   end
else
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
   JSONErrorObj.Add('code', ErrorCode);
   JSONErrorObj.Add('message', GetJSONErrorString(ErrorCode));
   JSONResultado.Add('error',JSONErrorObj);
   JSONResultado.Add('id', JSONIdNumber);
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

