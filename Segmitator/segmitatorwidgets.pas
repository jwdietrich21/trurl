unit segmitatorWidgets;

{ Trurl SegmitatorWidgets }

{ Suite of RPN calculators in Object Pascal }

{ Unit implementing seven-segment displays in graphic representation }

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
  Classes, SysUtils, Graphics, segmitator, Math;

const
  xHr: array[0..5] of integer = (0, 1, 8, 9, 8, 1);
  yHr: array[0..5] of integer = (1, 0, 0, 1, 2, 2);
  xVr: array[0..5] of integer = (1, 2, 2, 1, 0, 0);
  yVr: array[0..5] of integer = (0, 1, 8, 9, 8, 1);
  xHi: array[0..5] of integer = (0, 1, 8, 9, 8, 1);
  yHi: array[0..5] of integer = (1, 0, 0, 1, 2, 2);
  xVi: array[0..5] of integer = (2, 3, 2, 1, 0, 1);
  yVi: array[0..5] of integer = (0, 1, 8, 9, 8, 1);

type

  { TDisplay }

  TDisplay = class
  private
    pointsA, pointsB, pointsC, pointsD, pointsE, pointsF, pointsG: array[0..5] of TPoint;
    pointsH: array[0..1] of TPoint;
    xH, yH, xV, yV: array[0..5] of integer;
    fStyle: TFontStyles;
    lastXPos: integer;
    fn: real;
    procedure DrawA(i: byte);
    procedure DrawB(i: byte);
    procedure DrawC(i: byte);
    procedure DrawD(i: byte);
    procedure DrawE(i: byte);
    procedure DrawF(i: byte);
    procedure DrawG(i: byte);
    procedure DrawDot(i: byte);
    procedure DrawDigit(i: byte);
    procedure DrawDigit(c: char);
    procedure DrawDigits(n: real);
    procedure Error(msg: string);
    procedure SetStyle(theStyle: TFontStyles);
  public
    Canvas: TCanvas;
    Color: TColor;
    scale: integer;
    decScale: integer;
    offsetX, offsetY: integer;
    errorState: boolean;
    l: integer;
    constructor Create;
    destructor Destroy; override;
    property Style: TFontStyles read fStyle write SetStyle;
    property n: real read fn write DrawDigits;
  end;

implementation

{ TDisplay }

procedure TDisplay.DrawA(i: byte);
{ Draw 'A' segment }
var
  j, posX, posY, localOffset: integer;
begin
  if fsItalic in fStyle then
    localOffset := 2 * scale
  else
    localOffset := 0;
  if odd(kSegments[i] shr 6) then
  begin
    for j := 0 to 5 do
    begin
      posX := lastXPos + localOffset + 1 * scale;
      posY := offsetY;
      pointsA[j].x := xH[j] * scale + posX;
      pointsA[j].y := yH[j] * scale + posY;
    end;
    Canvas.Polygon(pointsA);
  end;
end;

procedure TDisplay.DrawB(i: byte);
{ Draw 'B' segment }
var
  j, posX, posY, localOffset: integer;
begin
  if fsItalic in fStyle then
    localOffset := 1 * scale
  else
    localOffset := 0;
  if odd(kSegments[i] shr 5) then
  begin
    for j := 0 to 5 do
    begin
      posX := lastXPos + localOffset + 9 * scale;
      posY := offsetY + 1 * scale;
      pointsB[j].x := xV[j] * scale + posX;
      pointsB[j].y := yV[j] * scale + posY;
    end;
    Canvas.Polygon(pointsB);
  end;
end;

procedure TDisplay.DrawC(i: byte);
{ Draw 'C' segment }
var
  j, posX, posY: integer;
begin
  if odd(kSegments[i] shr 4) then
  begin
    for j := 0 to 5 do
    begin
      posX := lastXPos + 9 * scale;
      posY := offsetY + 10 * scale;
      pointsC[j].x := xV[j] * scale + posX;
      pointsC[j].y := yV[j] * scale + posY;
    end;
    Canvas.Polygon(pointsC);
  end;
end;

procedure TDisplay.DrawD(i: byte);
{ Draw 'D' segment }
var
  j, posX, posY: integer;
begin
  if odd(kSegments[i] shr 3) then
  begin
    for j := 0 to 5 do
    begin
      posX := lastXPos + 1 * scale;
      posY := offsetY + 18 * scale;
      pointsD[j].x := xH[j] * scale + posX;
      pointsD[j].y := yH[j] * scale + posY;
    end;
    Canvas.Polygon(pointsD);
  end;
end;

procedure TDisplay.DrawE(i: byte);
{ Draw 'E' segment }
var
  j, posX, posY: integer;
begin
  if odd(kSegments[i] shr 2) then
  begin
    for j := 0 to 5 do
    begin
      posX := lastXPos;
      posY := offsetY + 10 * scale;
      pointsE[j].x := xV[j] * scale + posX;
      pointsE[j].y := yV[j] * scale + posY;
    end;
    Canvas.Polygon(pointsE);
  end;
end;

procedure TDisplay.DrawF(i: byte);
{ Draw 'F' segment }
var
  j, posX, posY, localOffset: integer;
begin
  if fsItalic in fStyle then
    localOffset := 1 * scale
  else
    localOffset := 0;
  if odd(kSegments[i] shr 1) then
  begin
    for j := 0 to 5 do
    begin
      posX := lastXPos + localOffset;
      posY := offsetY + 1 * scale;
      pointsF[j].x := xV[j] * scale + posX;
      pointsF[j].y := yV[j] * scale + posY;
    end;
    Canvas.Polygon(pointsF);
  end;
end;

procedure TDisplay.DrawG(i: byte);
{ Draw 'G' segment }
var
  j, posX, posY, localOffset: integer;
begin
  if fsItalic in fStyle then
    localOffset := 1 * scale
  else
    localOffset := 0;
  if odd(kSegments[i]) then
  begin
    for j := 0 to 5 do
    begin
      posX := lastXPos + localOffset + 1 * scale;
      posY := offsetY + 9 * scale;
      pointsG[j].x := xH[j] * scale + posX;
      pointsG[j].y := yH[j] * scale + posY;
    end;
    Canvas.Polygon(pointsG);
  end;
end;

procedure TDisplay.DrawDot(i: byte);
{ Draw decimal point }
begin
  if odd(kSegments[i] shr 7) then
  begin
    pointsH[0].x := lastXPos + 12 * scale;
    pointsH[0].y := trunc(offsetY + 17.5 * scale);
    pointsH[1].x := pointsH[0].x + decScale * scale;
    pointsH[1].y := pointsH[0].y + decScale * scale;
    Canvas.Ellipse(pointsH[0].x, pointsH[0].y, pointsH[1].x, pointsH[1].y);
  end;
end;

procedure TDisplay.DrawDigit(i: byte);
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

procedure TDisplay.DrawDigit(c: char);
begin
  case c of
    'e':
    begin
      DrawA(21);
      DrawB(21);
      DrawC(21);
      DrawD(21);
      DrawE(21);
      DrawF(21);
      DrawG(21);
    end;
    'r':
    begin
      DrawA(22);
      DrawB(22);
      DrawC(22);
      DrawD(22);
      DrawE(22);
      DrawF(22);
      DrawG(22);
    end;
  end;
end;

constructor TDisplay.Create;
begin
  inherited Create;
  color := clLime;
  offsetX := 3;
  offsetY := 3;
  Style := [];
  scale := 3;
  decScale := 3;
  xH := xHr;
  yH := yHr;
  xV := xVr;
  yV := yVr;
end;

destructor TDisplay.Destroy;
begin
  inherited Destroy;
end;

procedure TDisplay.DrawDigits(n: real);
{ Draw seven-segment display representation of n }
var
  oldBColor, oldPColor: TColor;
  oldBStyle: TBrushStyle;
  oldPStyle: TPenStyle;
  oldWidth: longint;
  i, digit: integer;
  nString: ansistring;
  theFormat: TFormatSettings;
begin
  fn := n;
  lastXPos := offsetX;
  if assigned(Canvas) then
  begin
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
          if nString[i - 1] = '-' then // negative number or exponent
          begin
            digit := 20;
            DrawDigit(digit);
            lastXPos := lastXPos + 18 * scale;
          end
          else if LowerCase(nString[i - 1]) = 'e' then  // exponent
          begin
            digit := 21;
            DrawDigit(digit);
            lastXPos := lastXPos + 18 * scale;
          end
          else
          begin
            if nString[i] <> kDot then // no decimal dot
            begin
              if TryStrToInt(nString[i - 1], digit) then
              begin
                DrawDigit(digit);
                lastXPos := lastXPos + 18 * scale;
              end;
            end
            else
            begin // following decimal dot
              digit := StrToInt(nString[i - 1]);
              DrawDigit(digit + 10);
              lastXPos := lastXPos + 18 * scale;
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
      DrawDigit(21);
      lastXPos := lastXPos + 18 * scale;
      DrawDigit(21);
      lastXPos := lastXPos + 18 * scale;
      digit := 21;
    end
    else
    if errorState then
    begin
      // 'Err'
      DrawDigit(21);
      lastXPos := lastXPos + 18 * scale;
      DrawDigit(22);
      lastXPos := lastXPos + 18 * scale;
      digit := 22;
    end;

    DrawDigit(digit);

    Canvas.Pen.Width := oldWidth;
    Canvas.Pen.Style := oldPStyle;
    Canvas.Pen.Color := oldPColor;
    Canvas.Brush.Style := oldBStyle;
    Canvas.Brush.Color := oldBColor;
  end
  else
    Error('Canvas not assigned');
end;

procedure TDisplay.Error(msg: string);
begin
  raise Exception(msg) at
  get_caller_addr(get_frame),
  get_caller_frame(get_frame);
end;

procedure TDisplay.SetStyle(theStyle: TFontStyles);
begin
  fStyle := theStyle;
  if fsItalic in fStyle then
  begin
    xH := xHi;
    yH := yHi;
    xV := xVi;
    yV := yVi;
  end
  else
  begin
    xH := xHr;
    yH := yHr;
    xV := xVr;
    yV := yVr;
  end;
end;

end.
