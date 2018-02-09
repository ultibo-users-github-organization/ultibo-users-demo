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

function ShortLogAddrToString (initiator: Boolean; addr : byte) : string;
begin
  case addr of
    CEC_AllDevices_eTV           : Result := 'TV  ';
    CEC_AllDevices_eRec1         : Result := 'REC1';
    CEC_AllDevices_eRec2         : Result := 'REC2';
    CEC_AllDevices_eSTB1         : Result := 'STB1';
    CEC_AllDevices_eDVD1         : Result := 'DVD1';
    CEC_AllDevices_eAudioSystem  : Result := 'AUD1';
    CEC_AllDevices_eSTB2         : Result := 'STB2';
    CEC_AllDevices_eSTB3         : Result := 'STB3';
    CEC_AllDevices_eDVD2         : Result := 'DVD2';
    CEC_AllDevices_eRec3         : Result := 'REC3';
    CEC_AllDevices_eSTB4         : Result := 'STB4';
    CEC_AllDevices_eDVD3         : Result := 'DVD3';
    CEC_AllDevices_eRsvd3        : Result := 'RSV3';
    CEC_AllDevices_eRsvd4        : Result := 'RSV4';
    CEC_AllDevices_eFreeUse      : Result := 'FREE';
    CEC_AllDevices_eUnRegistered : if Initiator then Result := 'UNRG' else Result := 'BRDC';
    else                           Result := 'Unknown (' + addr.ToString + ')';
    end;
end;

procedure OsdMessage(Message:String);
begin
 Log(Format('CEC: attempting on screen message display "%s"',[Message]));
 vc_cec_send_SetOSDString(CEC_AllDevices_eTV,CEC_DISPLAY_CONTROL_DEFAULT_TIME,PChar(Message),False);
end;

var
 CecButtonPressedClock:LongWord;
 MyPhysicalAddress:Word=$ffff;
 CurrentlyRoutedPhysicalAddress:Word=$ffff;
 LastRoutingChangeClock:LongWord=0;
 LastRoutingChangeOsdMessageClock:LongWord=0;
 HdmiInputWatchingThreadHandle:TThreadHandle;

function HdmiInputWatchingThread(Parameter:Pointer):PtrInt;
begin
 Result:=0;
 while True do
  begin
   if (CurrentlyRoutedPhysicalAddress <> MyPhysicalAddress) and (LastRoutingChangeOsdMessageClock <> LastRoutingChangeClock) and ((ClockGetCount - LastRoutingChangeClock) >= 10*1000*1000) then
    begin
     OsdMessage('Ultibo Here!');
     LastRoutingChangeOsdMessageClock:=LastRoutingChangeClock;
    end;
   Sleep(1*1000);
  end;
end;

procedure CECCallback(Data:Pointer; Reason, Param1, Param2, Param3, Param4 :LongWord); cdecl;
var
 ClockNow:LongWord;
 Initiator:Byte;
 Follower:Byte;
 OpCode:Byte;
 UserControl:Byte;
 NewlyRoutedPhysicalAddress:Word;
begin
 ClockNow:=ClockGetCount;
 Reason:=Reason and $ffff;
 Param1:=Swap(Param1);
 Param2:=Swap(Param2);
 Param3:=Swap(Param3);
 Param4:=Swap(Param4);
 Initiator:=(Param1 shr 28) and $f;
 Follower:=(Param1 shr 24) and $f;
 OpCode:=(Param1 shr 16) and $ff;
 UserControl:=(Param1 shr 8) and $ff;
 NewlyRoutedPhysicalAddress:=(Param2 shr 16) and $ffff;
 if Reason = VC_CEC_RX then
  begin
   Log(Format('CEC: RX %8.8x %8.8x %8.8x %8.8x %s->%s %s',[Param1,Param2,Param3,Param4,ShortLogAddrToString(True,Initiator),ShortLogAddrToString(False,Follower),OpCodeToString(OpCode)]));
   if (OpCode = CEC_OpCode_RoutingChange) and (NewlyRoutedPhysicalAddress <> CurrentlyRoutedPhysicalAddress) then
    begin
     CurrentlyRoutedPhysicalAddress:=NewlyRoutedPhysicalAddress;
     LastRoutingChangeClock:=ClockNow;
     Log(Format('CEC: routing has changed to physical address 0x%4.4x',[CurrentlyRoutedPhysicalAddress]));
    end;
  end
 else if Reason = VC_CEC_TX then
  begin
   Log(Format('CEC: TX %8.8x %8.8x %8.8x %8.8x %s->%s %s',[Param1,Param2,Param3,Param4,ShortLogAddrToString(True,(Param1 shr 28) and $f),ShortLogAddrToString(False,(Param1 shr 24) and $f),OpCodeToString((Param1 shr 16) and $ff)]));
  end
 else if Reason = VC_CEC_BUTTON_PRESSED then
  begin
   CecButtonPressedClock:=ClockNow;
   Log(Format('CEC: %s pressed',[UserControlToString(UserControl)]));
  end
 else if Reason = VC_CEC_BUTTON_RELEASE then
  begin
   Log(Format('CEC: %s released after %d milliseconds',[UserControlToString(UserControl),(ClockNow - CecButtonPressedClock) div 1000]));
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
   Log (format ('CEC: Callback Reason %4.4x Params %8.8x %8.8x %8.8x %8.8x %s', [Reason,Param1,Param2,Param3,Param4,ReasonToString(Reason and $ff)]));
  end;
end;

procedure Show(Text:String);
begin
 ConsoleWriteLn(Text);
end;

procedure Main;
begin
 FunctionEntry;
 vc_cec_set_osd_name('Ultibo!');
 Show('CecDemo');
 Show('-------');
 Show('r key, blue (d) remote controller button - restart system');
 Show('q key, channel up remote controller button - quit current function');
 Show('');
 Show('Each time this function starts, an attempt is made to set the tv source name to "Ultibo!"');
 Show('  Try changing inputs with the tv remote controller to see if this is displayed');
 Show('');
 Show('When the tv input is selected as something other than this pi, after ten seconds');
 Show('  a message "Ultibo Here!" will be momentarily displayed on the tv using the OSD (On Screen Display)');
 Show('');
 Show(Format('Physical Address 0x%4.4x',[MyPhysicalAddress]));
 WaitForFunctionQuitRequested;
 FunctionExit;
end;

initialization
 RegisterFunction('CecDemo');
 BCMHostInit;
 vc_cec_set_passive(True);
 vc_cec_register_callback(@CECCallback,Nil);
 vc_cec_register_all;
 vc_cec_get_physical_address(MyPhysicalAddress);
 vc_cec_set_osd_name('Ultibo!');
 BeginThread(@HdmiInputWatchingThread,nil,HdmiInputWatchingThreadHandle,THREAD_STACK_DEFAULT_SIZE)
end.
