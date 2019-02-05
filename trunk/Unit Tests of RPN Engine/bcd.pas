unit BCD;

{ Trurl }

{ A suite of RPN calculators in Object Pascal }

{ BCD Support }

{ Version 1.0 (Leopolis) }

{ (c) Johannes W. Dietrich, 2003 - 2019 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://trurl.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type

  TSign = (positive, negative);

  TBCDFloat = packed record
    significand: array[0..5] of byte;
    exponent: array[0..1] of byte;
    sigSign, expSign: TSign;
  end;

  function asReal(aNumber: TBCDFloat): real;
  function asExtended(aNumber: TBCDFloat): extended;
  function asBCD(aNumber: real): TBCDFloat;

  {$IFDEF FPC}
  {$IFDEF VER3}
  operator mod(const a, b: real) c: real; inline;
  {$ELSE}
  function fmod(const a, b: real): real; inline;
  {$ENDIF}
  {$ENDIF}

implementation

{$IFDEF FPC}
{$IFDEF VER3}
operator mod(const a, b: real) c: real; inline;
{ implements modulo function for real values. Source: http://wiki.freepascal.org/Mod }
begin
  c := a - b * Int(a / b);
end;
{$ELSE}
function fmod(const a, b: real): real; inline;
{ modulo operator in form of a traditional function for old FPC versions }
begin
  result := a - b * trunc(a / b);
end;
{$ENDIF}
{$ENDIF}

function asReal(aNumber: TBCDFloat): real;
var
  mant: real;
  expo, msign, esign: Int64;
begin
  result := Math.NaN;
  mant := aNumber.significand[5] and $F
          + (aNumber.significand[5] shr 4) * 10
          + (aNumber.significand[4] and $F) * 100
          + (aNumber.significand[4] shr 4) * 1000
          + (aNumber.significand[3] and $F) * 10000
          + (aNumber.significand[3] shr 4) * 100000
          + (aNumber.significand[2] and $F) * 1e6
          + (aNumber.significand[2] shr 4) * 1e7
          + (aNumber.significand[1] and $F) * 1e8
          + (aNumber.significand[1] shr 4) * 1e9
          + (aNumber.significand[0] and $F) * 1e10
          + (aNumber.significand[0] shr 4) * 1e11;
  expo := aNumber.exponent[1] and $F
          + (aNumber.exponent[1] shr 4) * 10
          + (aNumber.exponent[0] and $F) * 100
          + (aNumber.exponent[0] shr 4) * 1000;
  msign := 2 * integer(aNumber.sigSign = positive) - 1;
  esign := 2 * integer(aNumber.expSign = positive) - 1;
  result := msign * mant * power(10, esign * expo);
end;

function asExtended(aNumber: TBCDFloat): extended;
var
  mant: real;
  expo, msign, esign: Int64;
begin
  result := Math.NaN;
  mant := aNumber.significand[5] and $F
          + (aNumber.significand[5] shr 4) * 10
          + (aNumber.significand[4] and $F) * 100
          + (aNumber.significand[4] shr 4) * 1000
          + (aNumber.significand[3] and $F) * 10000
          + (aNumber.significand[3] shr 4) * 100000
          + (aNumber.significand[2] and $F) * 1e6
          + (aNumber.significand[2] shr 4) * 1e7
          + (aNumber.significand[1] and $F) * 1e8
          + (aNumber.significand[1] shr 4) * 1e9
          + (aNumber.significand[0] and $F) * 1e10
          + (aNumber.significand[0] shr 4) * 1e11;
  expo := aNumber.exponent[1] and $F
          + (aNumber.exponent[1] shr 4) * 10
          + (aNumber.exponent[0] and $F) * 100
          + (aNumber.exponent[0] shr 4) * 1000;
  msign := 2 * integer(aNumber.sigSign = positive) - 1;
  esign := 2 * integer(aNumber.expSign = positive) - 1;
  result := msign * mant * power(10, esign * expo);
end;

function asBCD(aNumber: real): TBCDFloat;
var
  i, j, k: integer;
  expo, intpart, fracpart: Int64;
begin
  if sign(aNumber) >= 0 then
    result.sigSign := positive
  else
    result.sigSign := negative;
  expo := floor(log10(abs(aNumber)));
  intpart := trunc(aNumber);
  j := 5; // 12 digits
  k := expo;
  for i := 0 to j do
    result.significand[i] := 0;
  if expo > 0 then
  begin
    if not odd(expo) then
    begin
      result.significand[j] := intpart div trunc((power(10, i - 1))) mod 10;
      dec(k);
      dec(j);
    end;
    for i := k downto 0 do
    begin
      if odd(i) then
      begin
        result.significand[j] := intpart div trunc((power(10, i - 1))) mod 10 or ((intpart div trunc(power(10, i)) mod 10) shl 4);
        dec(j);
      end;
    end;
  end;
  if sign(expo) >= 0 then
    result.expSign := positive
  else
    result.expSign := negative;
  result.exponent[1] := expo mod 10 or ((expo div 10 mod 10) shl 4);
  result.exponent[0] := expo div 100 mod 10 or ((expo div 1000 mod 10) shl 4);
end;

end.

