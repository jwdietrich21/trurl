unit RPNEngine;

{ Trurl }

{ A suite of RPN calculators in Object Pascal }

{ Basic RPN Engine }

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

{ TOperator }

TBinOperator = (PlusOp, MinusOp, MultOp, DivOp, PowerOp);
TUniOperator = (PlusMinusOp, InvertOp, SinOp, CosOp, TanOp, ASinOp, ACosOp, ATanOp, sqrtOp);

{ TStack }

TStack = class
private
  fx, fy, fz, ft: extended;
public
  constructor create;
  destructor destroy; override;
  procedure RollDown;
  procedure DropDown;
  procedure RollUp;
  procedure Push(operand: extended);
  function Pop: extended;
public
  property x: extended read fx write fx;
  property y: extended read fy;
  property z: extended read fz;
  property t: extended read ft;
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
  function rpn(operand1, operand2: extended; binOp: TBinOperator): extended;
  function rpn(operand: extended; uniOp: TUniOperator): extended;
end;

implementation

{ TStack }

constructor TStack.create;
begin
  inherited create;
  x := 0;
  fy := 0;
  fz := 0;
  ft := 0;
end;

destructor TStack.destroy;
begin
  inherited destroy;
end;

procedure TStack.RollDown;
{ Complete roll down }
var
  temp: extended;
begin
  temp := x;
  x := y;
  fy := z;
  fz := t;
  ft := temp;
end;

procedure TStack.DropDown;
{ Drops register contents without recycling }
begin
  x := y;
  fy := z;
  fz := t;
  ft := 0;
end;

procedure TStack.RollUp;
{ Roll up after enter and on entry after calculation }
begin
  ft := z;
  fz := y;
  fy := x;
end;

procedure TStack.Push(operand: extended);
begin
  RollUp;
  x := operand;
end;

function TStack.Pop: extended;
begin
  result := x;
  DropDown;
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
  Stack.Push(rpn(Stack.Pop, Stack.Pop, PlusOp));
end;

procedure TEngine.Sub;
var
  operand1, operand2: extended;
begin
  operand1 := Stack.Pop;
  operand2 := Stack.Pop;
  Stack.Push(rpn(operand2, operand1, MinusOp));
end;

procedure TEngine.Times;
begin
  Stack.Push(rpn(Stack.Pop, Stack.Pop, MultOp));
end;

procedure TEngine.Divide;
var
  operand1, operand2: extended;
begin
  operand1 := Stack.Pop;
  operand2 := Stack.Pop;
  Stack.Push(rpn(operand2, operand1, DivOp));
end;

procedure TEngine.CHS;
begin
  Stack.Push(rpn(Stack.Pop, PlusMinusOp));
end;

procedure TEngine.Inv;
begin
  Stack.Push(rpn(Stack.Pop, InvertOp));
end;

procedure TEngine.PWR;
var
  operand1, operand2: extended;
begin
  operand1 := Stack.Pop;
  operand2 := Stack.Pop;
  Stack.Push(rpn(operand2, operand1, PowerOp));
end;

procedure TEngine.Sinus;
begin
  Stack.Push(rpn(stack.Pop, SinOp));
end;

procedure TEngine.Cosinus;
begin
  Stack.Push(rpn(stack.Pop, CosOp));
end;

procedure TEngine.Tangens;
begin
  Stack.Push(rpn(stack.Pop, TanOp));
end;

procedure TEngine.ArcSinus;
begin
  Stack.Push(rpn(stack.Pop, ASinOp));
end;

procedure TEngine.ArcCosinus;
begin
  Stack.Push(rpn(stack.Pop, ACosOp));
end;

procedure TEngine.ArcTangens;
begin
  Stack.Push(rpn(stack.Pop, ATanOp));
end;

procedure TEngine.sqroot;
begin
  Stack.Push(rpn(stack.Pop, sqrtOp));
end;

function TEngine.rpn(operand: extended; uniOp: TUniOperator): extended;
begin
  case UniOp of
  PlusMinusOp:
    result := -operand;
  InvertOp:
    if operand = 0 then
      result := Math.Infinity
    else
      result := 1 / operand;
  SinOp:
    result := sin(operand);
  CosOp:
    result := cos(operand);
  TanOp:
    result := tan(operand);
  ASinOp:
    result := arcsin(operand);
  ACosOp:
    result := arccos(operand);
  ATanOp:
    result := arctan(operand);
  sqrtOp:
    result := sqrt(operand);
  end;
end;

function TEngine.rpn(operand1, operand2: extended; binOp: TBinOperator
  ): extended;
begin
  case binOp of
  PlusOp:
    result := operand1 + operand2;
  MinusOp:
    result := operand1 - operand2;
  MultOp:
    result := operand1 * operand2;
  DivOp:
    begin
      if operand2 = 0 then
        result := math.Infinity
      else
        result := operand1 / operand2;
    end;
  PowerOp:
    result := exp(ln(operand1) * operand2);
  end;
end;

end.

