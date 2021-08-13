unit RPCT.Utils.ParseRPC;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

function ParseRPCJSON(jsonreceived:string):TJSONStringType;

implementation

function ParseRPCJSON(jsonreceived:string):TJSONStringType;
var
  jData : TJSONData;
  jObject : TJSONObject;
  method : string;
Begin
Result := '';
jData := GetJSON(jsonreceived);
jObject := TJSONObject(jData);
method := jObject.Get('method');
if method = 'test' then result := 'TEST RECEIVED'
End;

END.

