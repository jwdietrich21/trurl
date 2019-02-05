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

implementation

function asReal(aNumber: TBCDFloat): real;
var
  mant: real;
  expo, msign, esign: Int64;
begin
  result := Math.NaN;
  mant := aNumber.significand[0] and $F
          + (aNumber.significand[0] shr 4) * 10
          + (aNumber.significand[1] and $F) * 100
          + (aNumber.significand[1] shr 4) * 1000
          + (aNumber.significand[2] and $F) * 10000
          + (aNumber.significand[2] shr 4) * 100000
          + (aNumber.significand[3] and $F) * 1e6
          + (aNumber.significand[3] shr 4) * 1e7
          + (aNumber.significand[4] and $F) * 1e8
          + (aNumber.significand[4] shr 4) * 1e9
          + (aNumber.significand[5] and $F) * 1e10
          + (aNumber.significand[5] shr 4) * 1e11;
  expo := aNumber.exponent[0] and $F
          + (aNumber.exponent[0] shr 4) * 10
          + (aNumber.exponent[1] and $F) * 100
          + (aNumber.exponent[1] shr 4) * 1000;
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
  mant := aNumber.significand[0] and $F
          + (aNumber.significand[0] shr 4) * 10
          + (aNumber.significand[1] and $F) * 100
          + (aNumber.significand[1] shr 4) * 1000
          + (aNumber.significand[2] and $F) * 10000
          + (aNumber.significand[2] shr 4) * 100000
          + (aNumber.significand[3] and $F) * 1e6
          + (aNumber.significand[3] shr 4) * 1e7
          + (aNumber.significand[4] and $F) * 1e8
          + (aNumber.significand[4] shr 4) * 1e9
          + (aNumber.significand[5] and $F) * 1e10
          + (aNumber.significand[5] shr 4) * 1e11;
  expo := aNumber.exponent[0] and $F
          + (aNumber.exponent[0] shr 4) * 10
          + (aNumber.exponent[1] and $F) * 100
          + (aNumber.exponent[1] shr 4) * 1000;
  msign := 2 * integer(aNumber.sigSign = positive) - 1;
  esign := 2 * integer(aNumber.expSign = positive) - 1;
  result := msign * mant * power(10, esign * expo);
end;

end.

