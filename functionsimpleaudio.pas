unit FunctionSimpleAudio;

{$mode objfpc}{$H+}

interface

implementation
uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,fatfs,framebuffer,console,simpleaudio,uUsersDemo,VC4CEC
  { Add additional units here };

var freq2,pan2:double;           // These have to be global
    MainWindow:TWindowHandle;  // To writeln something :)

procedure beep(freq,pan:double);

begin
 freq2:=2*pi*(freq*20000)/44100;
 pan2:=(pan+1)/2;
end;


procedure audiocallback(userdata: Pointer; stream: PUInt8; len:Integer);

// this is our main audio engine

const phase:double=0.0;                 // Current phase. Has to be static variable

var i:integer;
    buffer:PSmallInt;

begin
buffer:=PSmallInt(stream);
for i:=0 to ((len div 4)-1) do          // we will use 16-bit signed samples, 2 chn = 4 bytes per sample
 begin
  buffer[2*i]:=round(pan2*32000*sin(phase));
  buffer[2*i+1]:=round((1-pan2)*32000*sin(phase));
  phase:=phase+freq2;
  if phase>2*pi then phase-=2*pi;
 end;
end;

procedure InitializeAudio;
begin
 SA_OpenAudio(44100,16,2,384,@audiocallback);    // sample rate, bit, channels, buffer length, callback
end;

var
 Pause:Integer;

procedure TogglePause;
begin
 if Pause = 0 then
  Pause:=1
 else
  Pause:=0;
 Log(Format('pauseaudio(%d)',[Pause]));
 pauseaudio(Pause);
end;

procedure Main;
var
 CecEvent:TInputEvent;
 KeyPressed:Boolean;
 Key:Char;
begin
 MainWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 ConsoleWindowSetDefault(ConsoleDeviceGetDefault,MainWindow);
 FunctionIsActive:=True;

 Show('Simple Audio');
 Show('------------');
 Show('r key, blue (d) remote controller button - restart system');
 Show('j key, channel up remote controller button - leave current function and move to next');
 Show('k key, channel down remote controller button - leave current function and move to previous');
 Show('space key, pause remote controller button - pause audio output');
 Show('');

 writeln('now beep 1000 Hz');
 Pause:=1;
 beep(0.05,0);
 TogglePause;

 while FunctionIsActive do
  begin
   Key:=Char(0);
   CecEvent:=ReadEvent(@CecEventQueue);
   case CecEvent.Kind of
    KindInputEventCecButtonPressed:
     case CecEvent.ButtonPressed of
      CEC_User_Control_ChannelUp:
       Key:='j';
      CEC_User_Control_ChannelDown:
       Key:='k';
      CEC_User_Control_Pause:
       Key:=' ';
     end;
   end;
   if Key = Char(0) then
    begin
     KeyPressed:=ConsolePeekKey(Key,Nil);
     if KeyPressed then
      ConsoleGetKey(Key,Nil);
    end;
   case Key of
    'j':
     UpdateFunctionNumber(+1);
    'k':
     UpdateFunctionNumber(-1);
    'r':
     SystemRestart(0);
    ' ':
     TogglePause;
   end;
   Sleep(10);
  end;

 pauseaudio(1);
 ConsoleWindowDestroy(MainWindow);
end;

initialization
 RegisterFunction('Simple Audio',@Main);
 InitializeAudio;
end.
