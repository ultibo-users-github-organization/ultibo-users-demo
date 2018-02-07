unit FunctionCecDemo;
{$mode objfpc}{$H+}

interface
procedure Main;

implementation
uses
 GlobalConfig,
 GlobalConst,
 GlobalTypes,
 Platform,
 Threads,
 SysUtils,
 Classes,
 Ultibo,
 Logging,
 Console,
 VC4CEC,
 VC4,
 uFunction;

function Swap(const A:cardinal): cardinal; inline;
begin
 result := ((A And $ff) shl 24) + ((A and $ff00) shl 8) + ((A and $ff0000) shr 8) + ((A and $ff000000) shr 24);
end;

procedure CECCallback(Data:Pointer; Reason, Param1, Param2, Param3, Param4 :LongWord); cdecl;
var
 UserControl:Byte;
begin
 Param1:=Swap(Param1);
 Param2:=Swap(Param2);
 Param3:=Swap(Param3);
 Param4:=Swap(Param4);
 UserControl:=(Param1 shr 8) and $ff;
 if (Reason and $ffff) = VC_CEC_BUTTON_PRESSED then
  begin
   Log(Format('pressed %s',[UserControlToString(UserControl)]));
  end
 else if (Reason and $ffff) = VC_CEC_BUTTON_RELEASE then
  begin
   Log(Format('release %s',[UserControlToString(UserControl)]));
   if UserControl = CEC_User_Control_ChannelUp then
    begin
     CecQuitRequested:=True;
    end
   else if UserControl = CEC_User_Control_F1Blue then
    begin
     CecRestartRequested:=True;
    end;
  end
 else
  begin
   Log (format ('Callback Reason %4.4x Params %.8x %.8x %.8x %.8x %s', [Reason And $ffff,Param1,Param2,Param3,Param4,ReasonToString(Reason and $ff)]));
  end;
end;

procedure Show(Text:String);
begin
 ConsoleWriteLn(Text);
end;

procedure Main;
begin
 FunctionEntry;
 Show('CecDemo');
 Show('-------');
 Show('r key, blue (d) remote controller button - restart system');
 Show('q key, channel up remote controller button - quit current function');
 Show('');
 WaitForFunctionQuitRequested;
 FunctionExit;
end;

initialization
 RegisterFunction('CecDemo');
 BCMHostInit;
 vc_cec_set_passive(True);
 vc_cec_register_callback(@CECCallback,Nil);
 vc_cec_register_all;
end.
