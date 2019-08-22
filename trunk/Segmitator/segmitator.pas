unit segmitator;

{ Trurl Segmitator }

{ Suite of RPN calculators in Object Pascal }

{ Unit implementing seven-segment displays as ASCII art }

{ Version 1.0 (Cook) }

{ (c) Johannes W. Dietrich, 1990 - 2019 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://trurl.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{

Names of segments:

    A
   ___
F |   | B
  |___| G
  |   |
E |___| C  .

    D
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  tASCIILine = string[4];
  tASCIIDisplay = array[0..2] of string;

const

  kSegments: array[0..20] of byte =
    (
     %01111110, // 0
     %00110000, // 1
     %01101101, // 2
     %01111001, // 3
     %00110011, // 4
     %01011011, // 5
     %01011111, // 6
     %01110000, // 7
     %01111111, // 8
     %01111011, // 9
     %11111110, // 0.
     %10110000, // 1.
     %11101101, // 2.
     %11111001, // 3.
     %10110011, // 4.
     %11011011, // 5.
     %11011111, // 6.
     %11110000, // 7.
     %11111111, // 8.
     %11111011, // 9.
     %00000001  // -
     );

  kDot = '.';

function AsciiDigits(n: real): TASCIIDisplay;

implementation

function AsciiA(i: Byte): char;
{ Create string representation for 'A' segment }
begin
  if odd(kSegments[i] shr 6) then
    result := '_'
  else
    result := ' ';
end;

function AsciiB(i: Byte): char;
{ Create string representation for 'B' segment }
begin
  if odd(kSegments[i] shr 5) then
    result := '|'
  else
    result := ' ';
end;

function AsciiC(i: Byte): char;
{ Create string representation for 'C' segment }
begin
  if odd(kSegments[i] shr 4) then
    result := '|'
  else
    result := ' ';
end;

function AsciiD(i: Byte): char;
{ Create string representation for 'D' segment }
begin
  if odd(kSegments[i] shr 3) then
    result := '_'
  else
    result := ' ';
end;

function AsciiE(i: Byte): char;
{ Create string representation for 'E' segment }
begin
  if odd(kSegments[i] shr 2) then
    result := '|'
  else
    result := ' ';
end;

function AsciiF(i: Byte): char;
{ Create string representation for 'F' segment }
begin
  if odd(kSegments[i] shr 1) then
    result := '|'
  else
    result := ' ';
end;

function AsciiG(i: Byte): char;
{ Create string representation for 'G' segment }
begin
  if odd(kSegments[i]) then
    result := '_'
  else
    result := ' ';
end;

function AsciiDot(i: Byte): char;
{ Create string representation for decimal point }
begin
  if odd(kSegments[i] shr 7) then
    result := '.'
  else
    result := ' ';
end;

function AsciiLine(i, j: Byte): tASCIILine;
{ Returns string representation of line i as part of the whole display }
begin
  case i of
  0: result := ' ' + AsciiA(j) + ' ' + ' ';
  1: result := AsciiF(j) + AsciiG(j) + AsciiB(j) + ' ';
  2: result := AsciiE(j) + AsciiD(j) + AsciiC(j) + AsciiDot(j);
  end;
end;

function AsciiDigits(n: real): TASCIIDisplay;
{ Returns array of lines as ASCII representation of the segment display }
var
  i, digit: integer;
  nString: AnsiString;
  theFormat: TFormatSettings;
  line0, line1, line2: string;
begin
  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := kDot;
  nString := FloatToStr(n, theFormat);
  line0 := '';
  line1 := '';
  line2 := '';
  i := 1;
  if length(nString) > 1 then
  begin
    if nString[i] = '-' then  // negative number
    begin
      digit := 20;
      line0 := line0 + AsciiLine(0, digit);
      line1 := line1 + AsciiLine(1, digit);
      line2 := line2 + AsciiLine(2, digit);
    end;
    for i := 2 to length(nString) do
    begin
      if nString[i] <> kDot then  // no decimal dot
        begin
          if TryStrToInt(nString[i - 1], digit) then
            begin
              line0 := line0 + AsciiLine(0, digit);
              line1 := line1 + AsciiLine(1, digit);
              line2 := line2 + AsciiLine(2, digit);
            end;
        end
      else
        begin  // following decimal dot
          digit := StrToInt(nString[i - 1]);
          line0 := line0 + AsciiLine(0, digit + 10);
          line1 := line1 + AsciiLine(1, digit + 10);
          line2 := line2 + AsciiLine(2, digit + 10);
        end;
    end;
  end;
  digit := StrToInt(nString[i]);
  line0 := line0 + AsciiLine(0, digit);
  line1 := line1 + AsciiLine(1, digit);
  line2 := line2 + AsciiLine(2, digit);
  result[0] := line0;
  result[1] := line1;
  result[2] := line2;
end;

end.

