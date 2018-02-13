unit FunctionClock;
{$mode objfpc}{$H+}

interface
procedure Main;

implementation
uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  BCM2836,
  BCM2709,
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  SysUtils,
  Timezone,
  Platform,
  Winsock2,        {Include the Winsock2 unit so we can get the IP address}
  Devices,
  DateUtils,
  Services, {The services unit includes the NTP client and will automatically include the network}
  UltiboUtils,  {Include Ultibo utils for some command line manipulation}
  OpenVG,       {Include the OpenVG unit so we can use the various types and structures}
  VGShapes,     {Include the VGShapes unit to give us access to all the functions}
  VC4,          {Include the VC4 unit to enable access to the GPU}
  VC4CEC,
  Ultibo,
  iClock,
  Multifunction;

var

 timehour:Integer;
 timemin:Integer;
 timesecs:Integer;

 Counter:LongWord;
 Width:Integer;  {A few variables used by our shapes example}
 Height:Integer;

 posx:Integer;
 posy:Integer;
 dial:Integer;

 WindowHandle:TWindowHandle;
 IPAddress:String;
 Winsock2TCPClient:TWinsock2TCPClient;

procedure Main;
var
 CecEvent:TInputEvent;
 KeyPressed:Boolean;
 Key:Char;
 VideoIsOn:Boolean;
 VideoRequested:Boolean;
begin
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 ConsoleWindowSetDefault(ConsoleDeviceGetDefault,WindowHandle);
 FunctionIsActive:=True;
 VideoIsOn:=False;
 VideoRequested:=False;

 Show('Industrial Clock');
 Show('----------------');
 Show('r key, blue (d) remote controller button - restart system');
 Show('j key, channel up remote controller button - leave current function and move to next');
 Show('k key, channel down remote controller button - leave current function and move to previous');
 Show('enter key, select remote controller button - toggle video output');
 Show('');

 {Print out the current time and date}
 {This will probably show 30-12-99 which is midnight on 30 December 1899, this is the default value of Free Pascal time}
 ConsoleWindowWriteLn(WindowHandle,'The current date and time is ' + DateTimeToStr(Now));

 {Initialize a variable so we can count how long we've been waiting}
 Counter:=0;

 {Create a Winsock2TCPClient so that we can get some local information}
 Winsock2TCPClient:=TWinsock2TCPClient.Create;

 {Print our host name on the screen}
 ConsoleWindowWriteLn(WindowHandle,'Host name is ' + Winsock2TCPClient.LocalHost);

 {Get our local IP address which may be invalid at this point}
 IPAddress:=Winsock2TCPClient.LocalAddress;

 {Check the local IP address}
 if (IPAddress = '') or (IPAddress = '0.0.0.0') or (IPAddress = '255.255.255.255') then
  begin
   ConsoleWindowWriteLn(WindowHandle,'IP address is ' + IPAddress);
   ConsoleWindowWriteLn(WindowHandle,'Waiting for a valid IP address, make sure the network is connected');

   {Wait until we have an IP address}
   while (IPAddress = '') or (IPAddress = '0.0.0.0') or (IPAddress = '255.255.255.255') do
     begin
      {Sleep a bit}
      Sleep(1000);
      {Get the address again}
      IPAddress:=Winsock2TCPClient.LocalAddress;
    end;
   end;

 {Print our IP address on the screen}
 ConsoleWindowWriteLn(WindowHandle,'IP address is ' + IPAddress);
 ConsoleWindowWriteLn(WindowHandle,'');

 {Initialize a variable so we can count how long we've been waiting}
 Counter:=0;

 {Let's wait for a while for the time to be updated}
 while YearOf(Now) < 2000 do
  begin
   {Sleep for a second}
   Sleep(1000);

   {Update our counter}
   Inc(Counter);

   {Check how long we have waited}
   if Counter > 90 then
    begin
     {Print a failure message on the console}
     ConsoleWindowWriteLn(WindowHandle,'Sorry, failed to get the time after 90 seconds. Is the network connected?');

     {Break out of the loop and continue}
     Break;
    end;
  end;

 {Check out counter to see if we got here by success of failure}
 if Counter <= 90 then
  begin
   {We must have been successful so let's print the date and time again}
   ConsoleWindowWriteLn(WindowHandle,'Success, time has been updated from the internet');
   ConsoleWindowWriteLn(WindowHandle,'The date and time is now ' + DateTimeToStr(Now));
  end;

 ConsoleWindowWriteLn(WindowHandle,'The current date and time is ' + DateTimeToStr(Now));

 {Now we can set the timezone to another place and see what the time is there}
   {Australia}
   ConsoleWindowWriteLn(WindowHandle,'Setting Timezone to "E. Australia Standard Time"');
   SetCurrentTimezone('E. Australia Standard Time');
   ConsoleWindowWriteLn(WindowHandle,'The date and time is now ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now));
   ConsoleWindowWriteLn(WindowHandle,'');

 ConsoleWindowWriteLn(WindowHandle,'Starting VGShapes Demo');

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
      CEC_User_Control_F1Blue:
       Key:='r';
      CEC_User_Control_Select:
       Key:=Char(13);
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
    Char(13):
     VideoRequested:=not VideoRequested;
   end;

   if not VideoIsOn and VideoRequested then
    begin
     Width:=ConsoleWindowGetWidth(WindowHandle);
     Height:=ConsoleWindowGetHeight(WindowHandle);

     {Initialize OpenVG and the VGShapes unit}
     VGShapesInit(Width,Height);

     {Start a picture the full width and height of the screen}
     VGShapesStart(Width,Height);

     {Make the background black}
     VGShapesBackground(153,204,255);

     VideoIsOn:=True;
    end
   else if VideoIsOn and not VideoRequested then
    begin
     {Clear our screen, cleanup OpenVG and deinitialize VGShapes}
     VGShapesFinish;
     VideoIsOn:=False;
    end;


   if VideoIsOn then
    begin
     timehour:= HourOf(now);
     timemin:= MinuteOf(now);
     timesecs:= SecondOf(now);

     posx:=Width div 2;
     posy:=Height div 2;
     dial:=5 * Height div 6 ;
     clock(posx,posy,dial,timehour, timemin, timesecs);


     {End our picture and render it to the screen}
     VGShapesEnd;
    end;
   Sleep(100);
  end;

 if VideoIsOn then
  VGShapesFinish;
 ConsoleWindowDestroy(WindowHandle);
end;

initialization
 RegisterFunction('Industrial Clock',@Main);
end.
