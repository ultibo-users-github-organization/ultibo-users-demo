program UltiboUsersDemo;
{$mode objfpc}{$H+}

uses
 RaspberryPi2,BCM2836,BCM2709,
 FATFS,FileSystem,MMC,
 Platform,
 Ultibo,
 SysUtils,
 FunctionCecDemo,
 FunctionClock,
 FunctionDiagnosticsConsole,
 FunctionGuide,
 FunctionH264VideoPlayer,
 FunctionPfd,
 FunctionSimpleAudio,
 FunctionUsbDriverDemo,
 Multifunction;

procedure RestoreDefaultBootConfig;
begin
 while not DirectoryExists ('C:\') Do
  sleep (500);
 if FileExists('default-config.txt') then
  CopyFile('default-config.txt','config.txt',False);
end;

const
 GuideFunctionNumber = 3;

var
 Request:Integer;

begin
 RestoreDefaultBootConfig;
 StartLogging;
 FunctionNumber:=GuideFunctionNumber;
 while True do
  begin
   Request:=Functions[FunctionNumber].Main();
   if Request >= 0 then
    SetFunctionNumber(Request)
   else
    case Request of
     RequestGuideFunction:
      SetFunctionNumber(GuideFunctionNumber);
     RequestNextFunctionInOrder:
      UpdateFunctionNumber(+1);
     RequestPreviousFunctionInOrder:
      UpdateFunctionNumber(-1);
     RequestSameFunction:
      UpdateFunctionNumber(-1);
     RequestSystemRestart:
      SystemRestart(0);
   end;
  end;
end.
