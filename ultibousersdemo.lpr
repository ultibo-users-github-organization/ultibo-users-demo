program UltiboUsersDemo;
{$mode objfpc}{$H+}

uses
 RaspberryPi2,BCM2836,BCM2709,
 FATFS,FileSystem,MMC,
 Ultibo,
 SysUtils,
 FunctionCecDemo,
 FunctionDiagnosticsConsole,
 FunctionSimpleAudio,
 uUsersDemo;

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
   Functions[FunctionNumber].Main;
  end;
end.
