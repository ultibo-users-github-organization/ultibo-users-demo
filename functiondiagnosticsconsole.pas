unit FunctionDiagnosticsConsole;
{$mode objfpc}{$H+}

interface
procedure Main;

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
 uFunction;

type
  TEDID = packed record
    Header : array [0..7] of byte; // fixed pattern  00 FF FF FF FF FF FF 00
    ManufacturerID : Word;
    ProductID : word;
    SerialNo : cardinal;
    ManufactureWeek : byte;
    ManufactureYear : byte;
    Version : byte;
    Revision : byte;
    VideoInput : byte;
    HSize : byte;                // centimetres
    VSize : byte;                // centimetres
    Gamma : byte;
    Features : byte;
    Color : array [0..9] of byte;
    EstablishTimings : array [0..1] of byte;
    ReservedTiming : byte;
    SupportTimings : array [0..15] of byte;
    DetailedTimings : array [0..71] of byte;
    ExtensionFlag : byte;
    CheckSum : byte;
  end;  
  
var
  EDID : TEDID;
  res : Longword;
      
function PNPID (val : Word; shift : byte) : char;
var
  n : byte;
begin
  Result := '?';
  n := (val shr shift) and $1f;
  if (n = 0) or (n > 26) then exit;
  Result := chr (ord ('A') + n - 1);
end;

function Swap (const a : cardinal): cardinal; inline;
begin
  Result := ((a and $ff) shl 24) + ((a and $ff00) shl 8) +
            ((a and $ff0000) shr 8) + ((a and $ff000000) shr 24);
end;

function Swap16 (w : Word): Word; inline;
begin
  Result := ((w and $ff) shl 8) + ((w and $ff00) shr 8);
end;             

procedure GetEdid;    
begin
  res := EDIDBlockGet (0, @EDID, SizeOf (TEDID));
  if res = ERROR_SUCCESS then
    begin
       EDID.ManufacturerID := Swap16 (EDID.ManufacturerID);
       EDID.ProductID := Swap16 (EDID.ProductID);
       EDID.SerialNo := Swap (EDID.SerialNo);
//     Log ('Serial ' + EDID.SerialNo.ToHexString (8));
     end;
// else
//    Log ('Error retrieving EDID. Code ' + res.ToString);   
end;

procedure Main;
var
 I:Integer;
begin
 FunctionEntry;
 Show('Diagnostics Console');
 Show('-------------------');
 Show('r key, blue (d) remote controller button - restart system');
 Show('q key, channel up remote controller button - quit current function');
 Show('');
 for I:=0 to Length(Functions) - 1 do
  Show(Format('Function %s is available',[Functions[I].Name]));
 Show('');
 Show(Format('Board Type %s',[BoardTypeToString(BoardGetType)]));
 GetEdid;
 Show(Format('TV Manufacturer %s Product %d Year %s EDID version %s.%s',
  [PNPID (EDID.ManufacturerID, 10) + PNPID (EDID.ManufacturerID, 5) + PNPID (EDID.ManufacturerID, 0),
   EDID.ProductID, (EDID.ManufactureYear + 1990).ToString,
   EDID.Version.ToString, EDID.Revision.ToString]));
 WaitForFunctionQuitRequested;
 FunctionExit;
end;

initialization
 RegisterFunction('Diagnostics Console');
end.
