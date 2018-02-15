unit FunctionPfd;
{$mode objfpc}{$H+}

interface

implementation
uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  BCM2836,BCM2709,
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  Platform,
  Ultibo,
  SysUtils,
  OpenVG,       {Include the OpenVG unit so we can use the various types and structures}
  VGShapes,     {Include the VGShapes unit to give us access to all the functions}
  VC4,
  VC4CEC,
  artihorizon,          {Include the VC4 unit to enable access to the GPU}
  Multifunction;

var
 Width:Integer;  {A few variables used by our shapes example}
 Height:Integer;
 

 ArtiHoriX:Integer;
 ArtiHoriY:Integer;
 ArtiHorisize:Integer;

 yawangle:Integer;
 rollangle:Integer;
 pitchangle:Integer;

 WindowHandle:TWindowHandle;

function Main:Integer;
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

 Show('Primary Flight Display');
 Show('----------------------');
 Show('r key, blue (d) remote controller button - restart system');
 Show('g key, stop remote controller button - leave current function and move to guide function');
 Show('j key, channel up remote controller button - leave current function and move to next');
 Show('k key, channel down remote controller button - leave current function and move to previous');
 Show('enter key, select remote controller button - toggle video output');
 Show('');

 ConsoleWindowWriteLn(WindowHandle,'Starting PFD Demo');

 Width:=ConsoleWindowGetWidth(WindowHandle);
 Height:=ConsoleWindowGetHeight(WindowHandle);

 ArtiHoriX:=500;
 ArtiHoriY:=500;
 ArtiHorisize:=300;
 yawangle:=0;
 rollangle:=10;
 pitchangle:=10;

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
    Char(13):
     VideoRequested:=not VideoRequested;
   end;

   if not VideoIsOn and VideoRequested then
    begin
     {Initialize OpenVG and the VGShapes unit}
     VGShapesInit(Width,Height); 
 
     {set some values for testing}
 
     {Start a picture the full width and height of the screen}
     VGShapesStart(Width,Height);
 
     {Make the background black}
     VGShapesBackground(0,0,0);

     {add PFD instruments}
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
     horizon(ArtiHoriX, ArtiHoriY, ArtiHorisize, yawangle, rollangle, pitchangle);
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
 RegisterFunction('Primary Flight Display',@Main);
end.
