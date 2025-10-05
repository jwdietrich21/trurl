unit segmitator;

{ Trurl Segmitator }

{ Suite of RPN calculators in Object Pascal }

{ Unit implementing seven-segment displays as ASCII art }

{ Version 1.2.0 (El Dorado) }

{ (c) Johannes W. Dietrich, 1990 - 2025 }

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
  Classes, SysUtils, Math;

type
  tASCIILine = string[4];
  tASCIIDisplay = array[0..2] of string;

const

  kSegments: array[0..22] of byte =
    (%01111110, // 0
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
    %00000001, // -
    %01001111, // E
    %00000101  // r
    );

  kDot = '.';

function FloatToFixed(n: real; l: integer; FormatSettings: TFormatSettings): string;
function AsciiDigits(n: real; l: integer; errorState: boolean): TASCIIDisplay;
function AsciiDigits(n: real; errorState: boolean): TASCIIDisplay;
function AsciiDigits(n: real): TASCIIDisplay;

implementation

function AsciiA(i: byte): char;
  { Create string representation for 'A' segment }
begin
  if odd(kSegments[i] shr 6) then
    Result := '_'
  else
    Result := ' ';
end;

function AsciiB(i: byte): char;
  { Create string representation for 'B' segment }
begin
  if odd(kSegments[i] shr 5) then
    Result := '|'
  else
    Result := ' ';
end;

function AsciiC(i: byte): char;
  { Create string representation for 'C' segment }
begin
  if odd(kSegments[i] shr 4) then
    Result := '|'
  else
    Result := ' ';
end;

function AsciiD(i: byte): char;
  { Create string representation for 'D' segment }
begin
  if odd(kSegments[i] shr 3) then
    Result := '_'
  else
    Result := ' ';
end;

function AsciiE(i: byte): char;
  { Create string representation for 'E' segment }
begin
  if odd(kSegments[i] shr 2) then
    Result := '|'
  else
    Result := ' ';
end;

function AsciiF(i: byte): char;
  { Create string representation for 'F' segment }
begin
  if odd(kSegments[i] shr 1) then
    Result := '|'
  else
    Result := ' ';
end;

function AsciiG(i: byte): char;
  { Create string representation for 'G' segment }
begin
  if odd(kSegments[i]) then
    Result := '_'
  else
    Result := ' ';
end;

function AsciiDot(i: byte): char;
  { Create string representation for decimal point }
begin
  if odd(kSegments[i] shr 7) then
    Result := '.'
  else
    Result := ' ';
end;

function AsciiLine(i, j: byte): tASCIILine;
  { Returns string representation of line i as part of the whole display }
begin
  case i of
    0: Result := ' ' + AsciiA(j) + ' ' + ' ';
    1: Result := AsciiF(j) + AsciiG(j) + AsciiB(j) + ' ';
    2: Result := AsciiE(j) + AsciiD(j) + AsciiC(j) + AsciiDot(j);
  end;
end;

function FloatToFixed(n: real; l: integer; FormatSettings: TFormatSettings): string;
  { Returns the text representation of a floating point number with fixed length }
var
  format: string;
  i, li, lf: integer;
begin
  li := length(IntToStr(trunc(n)));
  if li > l then
    Result := 'EEE'
  else
  begin
    lf := l - li;
    format := '0.';
    for i := 1 to lf do
    begin
      format := format + '#';
    end;
    Result := FormatFloat(format, n);
  end;
end;

function AsciiDigits(n: real; l: integer; errorState: boolean): TASCIIDisplay;

  { Returns array of lines as ASCII representation of the segment display }
var
  i, digit: integer;
  nString: ansistring;
  theFormat: TFormatSettings;
  line0, line1, line2: string;
begin
  digit := 0;
  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := kDot;
  line0 := '';
  line1 := '';
  line2 := '';
  if (not errorState) and (not isNan(n)) and not (IsInfinite(n)) then
  begin
    if l = 0 then
      nString := FloatToStr(n, theFormat)
    else
    begin
      nString := FloatToFixed(n, l, theFormat);
    end;
  end;
  if nString = 'Err' then
    errorState := True
  else if nString = 'EEE' then
    n := Math.NaN
  else
  begin
    i := 1;
    if length(nString) > 1 then
    begin
      for i := 2 to length(nString) do
      begin
        if nString[i - 1] = '-' then  // negative number or exponent
        begin
          digit := 20;
          line0 := line0 + AsciiLine(0, digit);
          line1 := line1 + AsciiLine(1, digit);
          line2 := line2 + AsciiLine(2, digit);
        end
        else if LowerCase(nString[i - 1]) = 'e' then  // exponent
        begin
          digit := 21;
          line0 := line0 + AsciiLine(0, digit);
          line1 := line1 + AsciiLine(1, digit);
          line2 := line2 + AsciiLine(2, digit);
        end
        else
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
            if TryStrToInt(nString[i - 1], digit) then
            begin
              line0 := line0 + AsciiLine(0, digit + 10);
              line1 := line1 + AsciiLine(1, digit + 10);
              line2 := line2 + AsciiLine(2, digit + 10);
            end;
          end;
        end;
      end;
    end;
    if length(nString) > 0 then
      digit := StrToInt(nString[i]);
  end;
  if isNaN(n) or IsInfinite(n) then
  begin
    // 'EEE'
    digit := 21;
    line0 := line0 + AsciiLine(0, digit);
    line1 := line1 + AsciiLine(1, digit);
    line2 := line2 + AsciiLine(2, digit);
    digit := 21;
    line0 := line0 + AsciiLine(0, digit);
    line1 := line1 + AsciiLine(1, digit);
    line2 := line2 + AsciiLine(2, digit);
    digit := 21;
  end
  else
  if errorState then
  begin
    // 'Err'
    digit := 21;
    line0 := line0 + AsciiLine(0, digit);
    line1 := line1 + AsciiLine(1, digit);
    line2 := line2 + AsciiLine(2, digit);
    digit := 22;
    line0 := line0 + AsciiLine(0, digit);
    line1 := line1 + AsciiLine(1, digit);
    line2 := line2 + AsciiLine(2, digit);
    digit := 22;
  end;
  line0 := line0 + AsciiLine(0, digit);
  line1 := line1 + AsciiLine(1, digit);
  line2 := line2 + AsciiLine(2, digit);
  Result[0] := line0;
  Result[1] := line1;
  Result[2] := line2;
end;

function AsciiDigits(n: real; errorState: boolean): TASCIIDisplay;
begin
  Result := AsciiDigits(n, 0, errorState);
end;

function AsciiDigits(n: real): TASCIIDisplay;
begin
  Result := AsciiDigits(n, False);
end;

end.
