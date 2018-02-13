unit Multifunction;
{$mode delphi}{$H+}

interface
uses
 GlobalTypes,
 Console,
 Devices,
 VC4CEC;

const
 QueueSize=1024;

type
 TProcedure = procedure;
 KindInputEvent=(KindInputEventNone,KindInputEventCecButtonPressed,KindInputEventCecButtonReleased,KindInputEventKeyboardKey);
 TInputEvent = record
 case Kind:KindInputEvent of
  KindInputEventNone:();
  KindInputEventCecButtonPressed:(ButtonPressed:Byte);
  KindInputEventCecButtonReleased:(ButtonReleased:Byte;ElapsedMilliseconds:LongWord);
  KindInputEventKeyboardKey:(Key:Char);
 end;

 PEventQueue = ^TEventQueue;
 TEventQueue = record
  Events:Array[0..QueueSize - 1] of TInputEvent;
  EventCounter:LongWord;
  ReadCounter:LongWord;
 end;

 TFunction = record
  Name:String;
  Main:TProcedure;
 end;

procedure UpdateFunctionNumber(Delta:Integer);
function ReadEvent(Queue:PEventQueue):TInputEvent;
procedure Log(Message:String);
procedure RegisterFunction(SetName:String;SetMain:TProcedure);
procedure Show(Text:String);
procedure StartLogging;
procedure AddEvent(Queue:PEventQueue;Event:TInputEvent);

var
 Initialized:Boolean=False;
 FunctionNumber:Integer;
 Functions:Array of TFunction;
 CecEventQueue:TEventQueue;
 FunctionIsActive:Boolean;

implementation
uses
 GlobalConfig,
 GlobalConst,
 Platform,
 Threads,
 SysUtils,
 Classes,
 Ultibo,
 Logging,
 Keyboard;

procedure UpdateFunctionNumber(Delta:Integer);
begin
 FunctionNumber:=(FunctionNumber + Delta) mod Length(Functions);
 if FunctionNumber < 0 then
  Inc(FunctionNumber,Length(Functions));
// Log(Format('Function now %d',[Functions[FunctionNumber].Name]));
 FunctionIsActive:=False;
end;

procedure Show(Text:String);
begin
 ConsoleWriteLn(Text);
end;

procedure StartLogging;
begin
 LOGGING_INCLUDE_COUNTER:=False;
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
end;

procedure Log(Message:String);
begin
 LoggingOutput(Message);
end;

procedure AddEvent(Queue:PEventQueue;Event:TInputEvent);
begin
 with Queue^ do
  begin
   Events[EventCounter and (QueueSize - 1)]:=Event;
   Inc(EventCounter);
  end;
end;

function ReadEvent(Queue:PEventQueue):TInputEvent;
begin
 with Queue^ do
  begin
   Result.Kind:=KindInputEventNone;
   if EventCounter <> ReadCounter then
    begin
     Result:=Events[ReadCounter and (QueueSize - 1)];
//   with Result do
//    Log(Format('read event %d %d %d %d %d',[Kind,ButtonPressed,ButtonReleased,ElapsedMilliseconds,Ord(Key)]));
     Inc(ReadCounter);
    end;
  end;
end;

procedure Initialize;
begin
 if not Initialized then
  begin
   SetLength(Functions,0);
   Initialized:=True;
  end;
end;

procedure RegisterFunction(SetName:String;SetMain:TProcedure);
begin
 Initialize;
 SetLength(Functions,Length(Functions) + 1);
 with Functions[Length(Functions) - 1] do
  begin
   Name:=SetName;
   Main:=SetMain;
  end;
end;

initialization
 FunctionNumber:=0;
 FunctionIsActive:=False;
end.
