unit uFunction;
{$mode objfpc}{$H+}

interface
uses
 GlobalTypes,
 Console,
 Devices;

procedure FunctionEntry;
procedure FunctionExit;
procedure Log(Message:String);
function QuitRequested:Boolean;
procedure RegisterFunction(Name:String);
procedure Show(Text:String);
procedure StartLogging;
procedure WaitForFunctionQuitRequested;

type
 TFunction = record
  Name:String;
 end;

var
 Initialized:Boolean=False;
 Functions:Array of TFunction;
 CecQuitRequested:Boolean=False;
 CecRestartRequested:Boolean=False;
 MainWindow:TWindowHandle;

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

procedure FunctionEntry;
begin
 CecQuitRequested:=False;
 Sleep(100); // wait for background color to be established when switching functions
 MainWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 ConsoleWindowSetDefault(ConsoleDeviceGetDefault,MainWindow);
end;

procedure FunctionExit;
begin
 ConsoleWindowDestroy(MainWindow);
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

function QuitRequested:Boolean;
var
 Key:Char;
begin
 if CecQuitRequested then
  Result:=True
 else if CecRestartRequested then
  SystemRestart(0)
 else
  begin
   Result:=ConsolePeekKey(Key,Nil);
   if Result then
    begin
     ConsoleGetKey(Key,Nil);
     if Key = 'r' then
      SystemRestart(0);
    end
  end;
end;

procedure WaitForFunctionQuitRequested;
begin
 while not QuitRequested do
  begin
   Sleep(1);
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

procedure RegisterFunction(Name:String);
begin
 Initialize;
 SetLength(Functions,Length(Functions) + 1);
 Functions[Length(Functions) - 1].Name:=Name;
end;

end.
