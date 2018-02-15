unit FunctionUsbDriverDemo;
{$mode delphi}{$H+}

interface

implementation
uses
 GlobalConfig, GlobalConst, GlobalTypes, Platform, Threads, SysUtils, Classes, Ultibo, Logging, Console,
 USB, VC4CEC, Multifunction;

var
 UsbDriver:PUSBDriver;

function USBDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
var
 Message:TMessage;
begin
 Result:=USB_STATUS_INVALID_PARAMETER;
 if Device = nil then Exit;
 if Interrface = nil then Exit;
 if Interrface.Driver <> UsbDriver then Exit;
 Result:=USB_STATUS_SUCCESS;
end;

function UsbDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
var
 Status:LongWord;
begin
 Result:=USB_STATUS_INVALID_PARAMETER;
 {Check Device}
 if Device = nil then Exit;
 Log('');
 Log(Format('UsbDriverDemo: Bind Device%d %4.4x<%s>:%4.4x<%s> sn<%s>',[Device.Address,Device.Descriptor.idVendor,Device.Manufacturer,Device.Descriptor.idProduct,Device.Product,Device.SerialNumber]));
 Log(Format('UsbDriverDemo: Bind Device Class 0x%2.2x<%s>',[Device.Descriptor.bDeviceClass,USBClassCodeToString(Device.Descriptor.bDeviceClass)]));
 if Interrface <> nil then
  Log(Format('               Bind Just Interface %d <%s>',[Interrface.Descriptor.bInterfaceNumber,Interrface.Description]))
 else
  Log(Format('               Bind Entire Device',[]));
 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
 Log(Format('               Bind rejected',[]));
 Log('');
end;

procedure UsbDriverDemoInit;
var
 Status:LongWord;
begin
 UsbDriver:=USBDriverCreate;
 if UsbDriver <> nil then
  begin
   UsbDriver.Driver.DriverName:='USB Driver Demo';
   UsbDriver.DriverBind:=UsbDriverBind;
   UsbDriver.DriverUnbind:=UsbDriverUnbind;
   Status:=USBDriverRegister(UsbDriver);
   if Status <> USB_STATUS_SUCCESS then
    Log('USB Driver Demo: Failed to register USB driver: ' + USBStatusToString(Status))
   else 
    Log('USB Driver Demo driver registered');
  end
 else
  begin
   Log('Failed to create USB Driver Demo driver');
  end;
end;

function Main:Integer;
var
 I:Integer;
 CecEvent:TInputEvent;
 KeyPressed:Boolean;
 Key:Char;
 MainWindow:TWindowHandle;
begin
 MainWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 ConsoleWindowSetDefault(ConsoleDeviceGetDefault,MainWindow);
 FunctionIsActive:=True;

 Show('USB Driver Demo');
 Show('---------------');
 Show('r key, blue (d) remote controller button - restart system');
 Show('g key, stop remote controller button - leave current function and move to guide function');
 Show('j key, channel up remote controller button - leave current function and move to next');
 Show('k key, channel down remote controller button - leave current function and move to previous');
 Show('');
 Show('Insert or re-insert your USB device and observe the log on the right');
 Show('');

 while FunctionIsActive do
  begin
   Key:=Char(0);
   CecEvent:=ReadEvent(@CecEventQueue);
   case CecEvent.Kind of
    KindInputEventCecButtonPressed:
     case CecEvent.ButtonPressed of
      CEC_User_Control_Stop:
       Key:='g';
      CEC_User_Control_ChannelUp:
       Key:='j';
      CEC_User_Control_ChannelDown:
       Key:='k';
      CEC_User_Control_F1Blue:
       Key:='r';
     end;
   end;
   if Key = Char(0) then
    begin
     KeyPressed:=ConsolePeekKey(Key,Nil);
     if KeyPressed then
      begin
       ConsoleGetKey(Key,Nil);
       Log(Format('key %d',[Ord(Key)]));
      end;
    end;
   case Key of
    'g':
     begin
      FunctionIsActive:=False;
      Result:=RequestGuideFunction;
     end;
    's':
     begin
      FunctionIsActive:=False;
      Result:=RequestSameFunction;
     end;
    'j':
     begin
      FunctionIsActive:=False;
      Result:=RequestNextFunctionInOrder;
     end;
    'k':
     begin
      FunctionIsActive:=False;
      Result:=RequestPreviousFunctionInOrder;
     end;
    'r':
     begin
      FunctionIsActive:=False;
      Result:=RequestSystemRestart;
     end;
   end;
   Sleep(10);
  end;

 ConsoleWindowDestroy(MainWindow);
end;

initialization
 RegisterFunction('USB Driver Demo',@Main);
 UsbDriverDemoInit;
end.
