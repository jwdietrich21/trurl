unit segmitator;

{ Trurl Segmitator }

{ Suite of RPN calculators in Object Pascal }

{ Unit implementing seven segment displays }

{ Version 1.0 (Leopolis) }

{ (c) Johannes W. Dietrich, 1990 - 2018 }

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

function AsciiA(i: Byte): char;
function AsciiB(i: Byte): char;
function AsciiC(i: Byte): char;
function AsciiD(i: Byte): char;
function AsciiE(i: Byte): char;
function AsciiF(i: Byte): char;
function AsciiG(i: Byte): char;
function AsciiDot(i: Byte): char;
function AsciiLine(i, j: Byte): tASCIILine;

implementation

function AsciiA(i: Byte): char;
begin
  if odd(kSegments[i] shr 6) then
    result := '_'
  else
    result := ' ';
end;

function AsciiB(i: Byte): char;
begin
  if odd(kSegments[i] shr 5) then
    result := '|'
  else
    result := ' ';
end;

function AsciiC(i: Byte): char;
begin
  if odd(kSegments[i] shr 4) then
    result := '|'
  else
    result := ' ';
end;

function AsciiD(i: Byte): char;
begin
  if odd(kSegments[i] shr 3) then
    result := '_'
  else
    result := ' ';
end;

function AsciiE(i: Byte): char;
begin
  if odd(kSegments[i] shr 2) then
    result := '|'
  else
    result := ' ';
end;

function AsciiF(i: Byte): char;
begin
  if odd(kSegments[i] shr 1) then
    result := '|'
  else
    result := ' ';
end;

function AsciiG(i: Byte): char;
begin
  if odd(kSegments[i]) then
    result := '_'
  else
    result := ' ';
end;

function AsciiDot(i: Byte): char;
begin
  if odd(kSegments[i] shr 7) then
    result := '.'
  else
    result := ' ';
end;

function AsciiLine(i, j: Byte): tASCIILine;
begin
  case i of
  0: result := ' ' + AsciiA(j) + ' ' + ' ';
  1: result := AsciiF(j) + AsciiG(j) + AsciiB(j) + ' ';
  2: result := AsciiE(j) + AsciiD(j) + AsciiC(j) + AsciiDot(j);
  end;
end;

end.

