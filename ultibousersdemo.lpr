program UltiboUsersDemo;
{$mode objfpc}{$H+}

uses
 RaspberryPi2,BCM2836,BCM2709,
 FATFS,FileSystem,MMC,
 Ultibo,
 SysUtils,
 FunctionDiagnosticsConsole,
 FunctionCecDemo,
 uFunction;

procedure RestoreDefaultBootConfig;
begin
 while not DirectoryExists ('C:\') Do
  sleep (500);
 if FileExists('default-config.txt') then
  CopyFile('default-config.txt','config.txt',False);
end;

begin
 RestoreDefaultBootConfig;
 StartLogging;
 while True do
  begin
   FunctionDiagnosticsConsole.Main;
   FunctionCecDemo.Main;
  end;
end.
