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

const
  digits = 12;
  expDig = 4;

type

  TSign = (positive, negative);
  TCarry = byte;
  TNibble = 0..15;
  TSigBytes = packed array[0..(digits div 2 - 1)] of byte;
  TExpBytes = packed array[0..(expDig div 2 - 1)] of byte;
  TSigNibbles = packed array[0..(digits - 1)] of TNibble;
  TExpNibbles = packed array[0..(expDig - 1)] of TNibble;

  TBCDFloat = packed record
    significand: TSigNibbles;
    exponent: TExpNibbles;
    sigSign, expSign: TSign;
  end;

  function BCDSum(Number1, Number2: TBCDFloat): TBCDFloat;
  function BCDZero: TBCDFloat;
  function SigAsNibbles(Bytes: TSigBytes): TSigNibbles;
  function ExpAsNibbles(Bytes: TExpBytes): TExpNibbles;
  function SigAsBytes(Nibbles: TSigNibbles): TSigBytes;
  function ExpAsBytes(Nibbles: TExpNibbles): TExpBytes;
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

function GetSignificand(aNumber: TBCDFloat): Int64;
begin
  result := trunc(aNumber.significand[0] * 100000000000)
            + trunc(aNumber.significand[1] * 10000000000)
            + trunc(aNumber.significand[2] * 1000000000)
            + trunc(aNumber.significand[3] * 100000000)
            + trunc(aNumber.significand[4] * 10000000)
            + trunc(aNumber.significand[5] * 1000000)
            + trunc(aNumber.significand[6] * 100000)
            + trunc(aNumber.significand[7] * 10000)
            + trunc(aNumber.significand[8] * 1000)
            + trunc(aNumber.significand[9] * 100)
            + trunc(aNumber.significand[10] * 10)
            + trunc(aNumber.significand[11]);
end;

function GetExponent(aNumber: TBCDFloat): Int64;
begin
  result := Int64(aNumber.exponent[3])
            + Int64(aNumber.exponent[2]) * 10
            + Int64(aNumber.exponent[1]) * 100
            + Int64(aNumber.exponent[0]) * 1000;
end;

procedure SetExponent(var aNumber: TBCDFloat; Exponent: Int64);
begin
  aNumber.exponent[3] := abs(Exponent) mod 10;
  aNumber.exponent[2] := abs(Exponent) div 10 mod 10;
  aNumber.exponent[1] := abs(Exponent) div 100 mod 10;
  aNumber.exponent[0] := abs(Exponent) div 1000 mod 10;
end;

function BCDSum(Number1, Number2: TBCDFloat): TBCDFloat;
var
  carry: TCarry;
  i, Subtotal, expo: integer;
begin
  if Number1.sigSign = Number2.sigSign then
  begin
    result := BCDZero;
    expo := max(GetExponent(Number1), GetExponent(Number2));
    carry := 0;
    for i := digits - 1 downto 0 do
      begin
        Subtotal := Number1.significand[i] + Number2.significand[i] + carry;
        if Subtotal > 9 then
        begin
          Subtotal := Subtotal + 6;
          carry := 1;
        end
        else
          carry := 0;
        result.significand[i] := Subtotal and $F;
      end;
    if carry > 0 then
      begin
        inc(expo);
        for i := digits - 1 downto 1 do
          result.significand[i] := result.significand[i - 1];
        result.significand[0] := carry;
      end;
    SetExponent(result, expo);
    if Number1.sigSign = negative then
      result.sigSign := negative;
  end
  else
  begin
    // place holder for future handler using a to be implemented subtraction method.
  end;
end;

function BCDZero: TBCDFloat;
var
  i: integer;
begin
  result.sigSign := positive;
  result.expSign := positive;
  for i := 0 to digits - 1 do
    result.significand[i] := 0;
  for i := 0 to expDig - 1 do
    result.exponent[i] := 0;
end;

function SigAsNibbles(Bytes: TSigBytes): TSigNibbles;
var
  i: integer;
begin
  for i := 0 to digits - 1 do
  begin
    if odd(i) then
      begin
        result[i] := Bytes[i div 2] and $F;
      end
    else
      begin
        result[i] := Bytes[i div 2] shr 4;
      end;
  end;
end;

function ExpAsNibbles(Bytes: TExpBytes): TExpNibbles;
var
  i: integer;
begin
  for i := 0 to expDig - 1 do
  begin
    if odd(i) then
      begin
        result[i] := Bytes[i div 2] and $F;
      end
    else
      begin
        result[i] := Bytes[i div 2] shr 4;
      end;
  end;
end;

function SigAsBytes(Nibbles: TSigNibbles): TSigBytes;
var
  i: integer;
begin
  for i := 0 to digits div 2 - 1 do
    result[i] := 16 * Nibbles[i * 2] + Nibbles[i * 2 + 1];
end;

function ExpAsBytes(Nibbles: TExpNibbles): TExpBytes;
var
  i: integer;
begin
  for i := 0 to expDig div 2 - 1 do
    result[i] := 16 * Nibbles[i * 2] + Nibbles[i * 2 + 1];
end;

function asReal(aNumber: TBCDFloat): real;
var
  mant: Int64;
  expo, expo2, msign, esign: Int64;
begin
  result := Math.NaN;
  mant := GetSignificand(aNumber);
  expo := GetExponent(aNumber);
  msign := 2 * Int64(aNumber.sigSign = positive) - 1;
  esign := 2 * Int64(aNumber.expSign = positive) - 1;
  expo2 := 1 + esign * expo - digits;
  result := msign * mant * power(10, expo2);
end;

function asExtended(aNumber: TBCDFloat): extended;
var
  mant: Int64;
  expo, expo2, msign, esign: Int64;
begin
  result := Math.NaN;
  mant := GetSignificand(aNumber);
  expo := GetExponent(aNumber);
  msign := 2 * Int64(aNumber.sigSign = positive) - 1;
  esign := 2 * Int64(aNumber.expSign = positive) - 1;
  expo2 := 1 + esign * expo - digits;
  result := msign * mant * power(10, expo2);
end;

function asBCD(aNumber: real): TBCDFloat;
var
  i: integer;
  fraction: extended;
  intstring, fracstring, sigstring: String;
  expo, expo2, intpart: Int64;
begin
  if sign(aNumber) >= 0 then
    result.sigSign := positive
  else
    result.sigSign := negative;
  expo := floor(log10(abs(aNumber)));
  if expo < 0 then
    begin
      expo2 := abs(expo);
      intpart := trunc(abs(aNumber) * power(10, digits + expo2));
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
    result.significand[i - 1] := StrToInt(sigstring[i]);
  end;
  if sign(expo) >= 0 then
    result.expSign := positive
  else
    result.expSign := negative;
  SetExponent(result, expo);
end;

end.

