unit RPNEngine;

{ Trurl }

{ A suite of RPN calculators in Object Pascal }

{ Basic RPN Engine }

{ Version 1.0 (Leopolis) }

{ (c) Johannes W. Dietrich, 2003 - 2018 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://puma-repository.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type

{ TRegisters }

TStack = class
public
  constructor create;
  destructor destroy; override;
  procedure PostBinCalc;
  procedure RollDown;
  procedure RollUp;
public
  x, y, z, t: extended;
end;

{ TEngine }

TEngine = class
public
  Stack: TStack;
  constructor create;
  destructor destroy; override;
  procedure Add;
  procedure Sub;
  procedure Times;
  procedure Divide;
  procedure CHS;
  procedure Inv;
  procedure PWR;
  procedure Sinus;
  procedure Cosinus;
  procedure Tangens;
  procedure ArcSinus;
  procedure ArcCosinus;
    procedure ArcTangens;
  procedure sqroot;
end;

implementation

{ TRegisters }

constructor TStack.create;
begin
  inherited create;
  x := 0;
  y := 0;
  z := 0;
  t := 0;
end;

destructor TStack.destroy;
begin
  inherited destroy;
end;

procedure TStack.PostBinCalc;
{ Partial roll down after executing a binary operator }
begin
  y := z;
  z := t;
end;

procedure TStack.RollDown;
{ Complete roll down }
var
  temp: extended;
begin
  temp := x;
  x := y;
  y := z;
  z := t;
  t := temp;
end;

procedure TStack.RollUp;
{ Roll up after enter and on entry after calculation }
begin
  t := z;
  z := y;
  y := x;
end;

constructor TEngine.create;
begin
  inherited create;
  Stack := TStack.create;
end;

destructor TEngine.destroy;
begin
  Stack.destroy;
  inherited destroy;
end;

procedure TEngine.Add;
begin
  Stack.x := Stack.y + Stack.x;
  Stack.PostBinCalc;
end;

procedure TEngine.Sub;
begin
  Stack.x := Stack.y - Stack.x;
  Stack.PostBinCalc;
end;

procedure TEngine.Times;
begin
  Stack.x := Stack.y * Stack.x;
  Stack.PostBinCalc;
end;

procedure TEngine.Divide;
begin
  if Stack.x = 0 then
    Stack.x := Math.Infinity
  else
    Stack.x := Stack.y / Stack.x;
  Stack.PostBinCalc;
end;

procedure TEngine.CHS;
begin
  Stack.x := -Stack.x;
end;

procedure TEngine.Inv;
begin
  if Stack.x = 0 then
    Stack.x := Math.Infinity
  else
    Stack.x := 1 / Stack.x;
end;

procedure TEngine.PWR;
begin
  Stack.x := exp(ln(Stack.y) * Stack.x);
end;

procedure TEngine.Sinus;
begin
  Stack.x := sin(stack.x);
end;

procedure TEngine.Cosinus;
begin
  Stack.x := cos(stack.x);
end;

procedure TEngine.Tangens;
begin
  Stack.x := tan(stack.x);
end;

procedure TEngine.ArcSinus;
begin
  Stack.x := arcsin(stack.x);
end;

procedure TEngine.ArcCosinus;
begin
  Stack.x := arccos(stack.x);
end;

procedure TEngine.ArcTangens;
begin
  Stack.x := arctan(stack.x);
end;

procedure TEngine.sqroot;
begin
  Stack.x := sqrt(stack.x);
end;

end.

