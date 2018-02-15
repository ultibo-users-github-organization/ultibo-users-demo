unit FunctionGuide;
{$mode objfpc}{$H+}

interface

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
 Multifunction,
 VC4CEC;

var
 FunctionNumber:Integer;

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

 Show('Guide to Functions');
 Show('------------------');
 Show('r key, blue (d) remote controller button - restart system');
 Show('g key, stop remote controller button - leave current function and move to guide function');
 Show('j key, channel up remote controller button - leave current function and move to next');
 Show('k key, channel down remote controller button - leave current function and move to previous');
 Show('');
 for I:=0 to Length(Functions) - 1 do
   Show(Format('%d key - %s',[I,Functions[I].Name]));

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
   if (Key >= '0') and (Key <= '9') then
    begin
     FunctionNumber:=Ord(Key) - Ord('0');
     if FunctionNumber <= Length(Functions) - 1 then
      begin
       FunctionIsActive:=False;
       Result:=RequestFunctionNumber(FunctionNumber);
      end;
    end;
   Sleep(10);
  end;

 ConsoleWindowDestroy(MainWindow);
end;

initialization
 RegisterFunction('Guide to Functions',@Main);
end.
