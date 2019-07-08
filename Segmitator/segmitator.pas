unit segmitator;

{ Trurl Segmitator }

{ Suite of RPN calculators in Object Pascal }

{ Unit implementing seven-segment displays }

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
  Classes, SysUtils, Graphics;

type
  tASCIILine = string[4];
  tASCIIDisplay = array[0..2] of string;

const

  kSegments: array[0..19] of byte =
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
     %11111011  // 9.
     );

  xH: array[0..5] of integer = (0, 1, 8, 9, 8, 1);
  yH: array[0..5] of integer = (1, 0, 0, 1, 2, 2);
  xV: array[0..5] of integer = (1, 2, 2, 1, 0, 0);
  yV: array[0..5] of integer = (0, 1, 8, 9, 8, 1);

function AsciiA(i: Byte): char;
function AsciiB(i: Byte): char;
function AsciiC(i: Byte): char;
function AsciiD(i: Byte): char;
function AsciiE(i: Byte): char;
function AsciiF(i: Byte): char;
function AsciiG(i: Byte): char;
function AsciiDot(i: Byte): char;
function AsciiLine(i, j: Byte): tASCIILine;
function AsciiDigits(n: real): TASCIIDisplay;
procedure DrawDigits(aCanvas: TCanvas; aColor: TColor; n: real);

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
const
  kDot = '.';
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
  if length(nString) > 1 then for i := 2 to length(nString) do
  begin
    if nString[i] <> kDot then
      begin
        if TryStrToInt(nString[i - 1], digit) then
          begin
            line0 := line0 + AsciiLine(0, digit);
            line1 := line1 + AsciiLine(1, digit);
            line2 := line2 + AsciiLine(2, digit);
          end;
      end
    else
      begin
        digit := StrToInt(nString[i - 1]);
        line0 := line0 + AsciiLine(0, digit + 10);
        line1 := line1 + AsciiLine(1, digit + 10);
        line2 := line2 + AsciiLine(2, digit + 10);
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

procedure DrawDigits(aCanvas: TCanvas; aColor: TColor; n: real);
{ Draw seven-segment display representation of n on the specified canvas }
var
  oldBColor, oldPColor: TColor;
  oldBStyle: TBrushStyle;
  oldPStyle: TPenStyle;
  pointsA, pointsB, pointsC, pointsD, pointsE, pointsF, pointsG: array[0..5] of TPoint;
  scale, offsetX, offsetY, posX, posY: integer;
  i: integer;
begin
  scale := 3;
  for i := 0 to 5 do
  begin
    offsetX := 13;
    offsetY := 9;
    posX := offsetX + scale;
    posY := offsetY;
    pointsA[i].x := xH[i] * scale + posX;
    pointsA[i].y := yH[i] * scale + posY;
    posX := offsetX + 9 * scale;
    posY := offsetY + 1 * scale;
    pointsB[i].x := xV[i] * scale + posX;
    pointsB[i].y := yV[i] * scale + posY;
    posY := offsetY + 10 * scale;
    pointsC[i].x := xV[i] * scale + posX;
    pointsC[i].y := yV[i] * scale + posY;
    posX := offsetX + scale;
    posY := offsetY + 18 * scale;
    pointsD[i].x := xH[i] * scale + posX;
    pointsD[i].y := yH[i] * scale + posY;
    posX := offsetX;
    posY := offsetY + 10 * scale;
    pointsE[i].x := xV[i] * scale + posX;
    pointsE[i].y := yV[i] * scale + posY;
    posY := offsetY + 1 * scale;
    pointsF[i].x := xV[i] * scale + posX;
    pointsF[i].y := yV[i] * scale + posY;
    posX := offsetX + scale;
    posY := offsetY + 9 * scale;
    pointsG[i].x := xH[i] * scale + posX;
    pointsG[i].y := yH[i] * scale + posY;
  end;

  oldBColor := aCanvas.Brush.Color;
  oldBStyle := aCanvas.Brush.Style;
  oldPColor := aCanvas.Pen.Color;
  oldPStyle := aCanvas.Pen.Style;
  aCanvas.Brush.Color := aColor;
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Pen.Style := psSolid;
  aCanvas.Pen.Color := oldBColor;

  aCanvas.Polygon(pointsA);
  aCanvas.Polygon(pointsB);
  aCanvas.Polygon(pointsC);
  aCanvas.Polygon(pointsD);
  aCanvas.Polygon(pointsE);
  aCanvas.Polygon(pointsF);
  aCanvas.Polygon(pointsG);

  aCanvas.Pen.Style := oldPStyle;
  aCanvas.Pen.Color := oldPColor;
  aCanvas.Brush.Style := oldBStyle;
  aCanvas.Brush.Color := oldBColor;
end;

end.

