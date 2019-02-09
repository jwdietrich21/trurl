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
  Classes, SysUtils, Math, StrUtils;

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
const
  digits = 12;
var
  i: integer;
  fraction: extended;
  intstring, fracstring, sigstring: String;
  expo, intpart, expo2: Int64;
begin
  if sign(aNumber) >= 0 then
    result.sigSign := positive
  else
    result.sigSign := negative;
  expo := floor(log10(abs(aNumber)));
  if expo < 0 then
    begin
      expo2 := abs(expo);
      intpart := trunc(abs(aNumber) * 10 ** (digits * expo2)); // not yet correct
      fraction := 0;
    end
  else
    begin
      intpart := trunc(abs(aNumber));
      fraction := frac(abs(aNumber));
    end;
  Str(intpart, intstring);
  Str(fraction: 14: 12, fracstring);
  fracstring := rightStr(fracstring, 12);
  sigstring := leftStr(intstring + fracstring, 12);
  for i := 1 to digits do
  begin
    if not odd(i) then
      begin
        result.significand[i div 2 - 1] := StrToInt(sigstring[i]) or (StrToInt(sigstring[i - 1]) shl 4);
      end;
  end;
  if sign(expo) >= 0 then
    result.expSign := positive
  else
    result.expSign := negative;
  result.exponent[1] := abs(expo) mod 10 or ((abs(expo) div 10 mod 10) shl 4);
  result.exponent[0] := abs(expo) div 100 mod 10 or ((abs(expo) div 1000 mod 10) shl 4);
end;

end.

