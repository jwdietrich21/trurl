unit segmitatorWidgets;

{ Trurl SegmitatorWidgets }

{ Suite of RPN calculators in Object Pascal }

{ Unit implementing seven-segment displays in graphic representation }

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
  Classes, SysUtils, Graphics, segmitator;

const
  xH: array[0..5] of integer = (0, 1, 8, 9, 8, 1);
  yH: array[0..5] of integer = (1, 0, 0, 1, 2, 2);
  xV: array[0..5] of integer = (1, 2, 2, 1, 0, 0);
  yV: array[0..5] of integer = (0, 1, 8, 9, 8, 1);

type

{ TDisplay }

TDisplay = class
private
  pointsA, pointsB, pointsC, pointsD, pointsE, pointsF, pointsG: array[0..5] of TPoint;
  pointsH: array[0..1] of TPoint;
  lastXPos: integer;
  procedure DrawA(i: Byte);
  procedure DrawB(i: Byte);
  procedure DrawC(i: Byte);
  procedure DrawD(i: Byte);
  procedure DrawE(i: Byte);
  procedure DrawF(i: Byte);
  procedure DrawG(i: Byte);
  procedure DrawDot(i: Byte);
  procedure DrawDigit(i: Byte);
public
  Canvas: TCanvas;
  Color: TColor;
  scale: integer;
  offsetX, offsetY: integer;
  constructor create;
  destructor destroy; override;
  procedure DrawDigits(n: real);
end;

implementation

{ TDisplay }

procedure TDisplay.DrawA(i: Byte);
{ Draw 'A' segment }
var
  j, posX, posY: integer;
begin
  if odd(kSegments[i] shr 6) then
    begin
      for j := 0 to 5 do begin
        posX := lastXPos + 1 * scale;
        posY := offsetY;
        pointsA[j].x := xH[j] * scale + posX;
        pointsA[j].y := yH[j] * scale + posY;
      end;
      Canvas.Polygon(pointsA);
    end;
end;

procedure TDisplay.DrawB(i: Byte);
{ Draw 'B' segment }
var
  j, posX, posY: integer;
begin
  if odd(kSegments[i] shr 5) then
    begin
      for j := 0 to 5 do begin
        posX := lastXPos + 9 * scale;
        posY := offsetY + 1 * scale;
        pointsB[j].x := xV[j] * scale + posX;
        pointsB[j].y := yV[j] * scale + posY;
      end;
      Canvas.Polygon(pointsB);
    end;
end;

procedure TDisplay.DrawC(i: Byte);
{ Draw 'C' segment }
var
  j, posX, posY: integer;
begin
  if odd(kSegments[i] shr 4) then
    begin
      for j := 0 to 5 do begin
        posX := lastXPos + 9 * scale;
        posY := offsetY + 10 * scale;
        pointsC[j].x := xV[j] * scale + posX;
        pointsC[j].y := yV[j] * scale + posY;
      end;
      Canvas.Polygon(pointsC);
    end;
end;

procedure TDisplay.DrawD(i: Byte);
{ Draw 'D' segment }
var
  j, posX, posY: integer;
begin
  if odd(kSegments[i] shr 3) then
    begin
      for j := 0 to 5 do begin
        posX := lastXPos + 1 * scale;
        posY := offsetY + 18 * scale;
        pointsD[j].x := xH[j] * scale + posX;
        pointsD[j].y := yH[j] * scale + posY;
      end;
      Canvas.Polygon(pointsD);
    end;
end;

procedure TDisplay.DrawE(i: Byte);
{ Draw 'E' segment }
var
  j, posX, posY: integer;
begin
  if odd(kSegments[i] shr 2) then
    begin
      for j := 0 to 5 do begin
        posX := lastXPos;
        posY := offsetY + 10 * scale;
        pointsE[j].x := xV[j] * scale + posX;
        pointsE[j].y := yV[j] * scale + posY;
      end;
      Canvas.Polygon(pointsE);
    end;
end;

procedure TDisplay.DrawF(i: Byte);
{ Draw 'F' segment }
var
  j, posX, posY: integer;
begin
  if odd(kSegments[i] shr 1) then
    begin
      for j := 0 to 5 do begin
        posX := lastXPos;
        posY := offsetY + 1 * scale;
        pointsF[j].x := xV[j] * scale + posX;
        pointsF[j].y := yV[j] * scale + posY;
      end;
      Canvas.Polygon(pointsF);
    end;
end;

procedure TDisplay.DrawG(i: Byte);
{ Draw 'G' segment }
var
  j, posX, posY: integer;
begin
  if odd(kSegments[i]) then
    begin
      for j := 0 to 5 do begin
        posX := lastXPos + 1 * scale;
        posY := offsetY + 9 * scale;
        pointsG[j].x := xH[j] * scale + posX;
        pointsG[j].y := yH[j] * scale + posY;
      end;
      Canvas.Polygon(pointsG);
    end;
end;

procedure TDisplay.DrawDot(i: Byte);
{ Draw decimal point }
begin
  if odd(kSegments[i] shr 7) then
    begin
      pointsH[0].x := lastXPos + 12 * scale;
      pointsH[0].y := trunc(offsetY + 17.5 * scale);
      pointsH[1].x := pointsH[0].x + 3 * scale;
      pointsH[1].y := pointsH[0].y + 3 * scale;
      Canvas.Ellipse(pointsH[0].x, pointsH[0].y, pointsH[1].x, pointsH[1].y);
    end;
end;

procedure TDisplay.DrawDigit(i: Byte);
begin
  DrawA(i);
  DrawB(i);
  DrawC(i);
  DrawD(i);
  DrawE(i);
  DrawF(i);
  DrawG(i);
  DrawDot(i);
end;

constructor TDisplay.create;
begin
  inherited create;
  scale := 3;
end;

destructor TDisplay.destroy;
begin
  inherited destroy;
end;

procedure TDisplay.DrawDigits(n: real);
{ Draw seven-segment display representation of n }
var
  oldBColor, oldPColor: TColor;
  oldBStyle: TBrushStyle;
  oldPStyle: TPenStyle;
  oldWidth: longint;
  i, digit: integer;
  nString: AnsiString;
  theFormat: TFormatSettings;
begin
  lastXPos := offsetX;

  Canvas.Clear;

  oldBColor := Canvas.Brush.Color;
  oldBStyle := Canvas.Brush.Style;
  oldPColor := Canvas.Pen.Color;
  oldPStyle := Canvas.Pen.Style;
  oldWidth := Canvas.Pen.Width;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := oldBColor;
  Canvas.Pen.Width := 1 + trunc(0.3 * scale);

  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := kDot;
  nString := FloatToStr(n, theFormat);

  i := 1;
  if length(nString) > 1 then for i := 2 to length(nString) do
  begin
    if nString[i] <> kDot then
      begin
        if TryStrToInt(nString[i - 1], digit) then
          begin
            DrawDigit(digit);
            lastXPos := lastXPos + 18 * scale;
          end;
      end
    else
      begin
        digit := StrToInt(nString[i - 1]);
        DrawDigit(digit + 10);
        lastXPos := lastXPos + 18 * scale;
      end;
  end;
  digit := StrToInt(nString[i]);
  DrawDigit(digit);

  Canvas.Pen.Width := oldWidth;
  Canvas.Pen.Style := oldPStyle;
  Canvas.Pen.Color := oldPColor;
  Canvas.Brush.Style := oldBStyle;
  Canvas.Brush.Color := oldBColor;
end;

end.

