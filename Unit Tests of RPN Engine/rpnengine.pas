unit RPNEngine;

{ Trurl }

{ A suite of RPN calculators in Object Pascal }

{ Basic RPN Engine }

{ Version 1.1.0 (Bet) }

{ (c) Johannes W. Dietrich, 2003 - 2025 }

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

{ Operator Types }

TBinOperator = (PlusOp, MinusOp, MultOp, DivOp, PowerOp);
TUniOperator = (PlusMinusOp, InvertOp, SinOp, CosOp, TanOp, ASinOp, ACosOp, ATanOp, sqrtOp);
TAngleMode = (Degree, Radian, Turn, Grad);

const
  RPNEngine_major   = 1;
  RPNEngine_minor   = 1;
  RPNEngine_release = 0;
  RPNEngine_patch   = 100;
  RPNEngine_fullversion = ((RPNEngine_major * 100 + RPNEngine_minor) *
    100 + RPNEngine_release) * 100 + RPNEngine_patch;
  RPNEngine_version = '1.1.0.100';
  RPNEngine_internalversion = 'Bet';

type

{ TStack }

TStack = class
private
  fx, fy, fz, ft: extended;
public
  constructor create;
  destructor destroy; override;
  procedure Clear;
  procedure RollDown;
  procedure DropDown;
  procedure RollUp;
  procedure Push(operand: extended);
  function Pop: extended;
  procedure Error(msg: String);
protected
  fl: extended;
public
  property x: extended read fx write fx;
  property y: extended read fy;
  property z: extended read fz;
  property t: extended read ft;
  property lastx: extended read fl;
end;

{ TEngine }

TEngine = class
public
  Stack: TStack;
  AngleMode: TAngleMode;
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

procedure TStack.Clear;
begin
  fx := 0;
  fy := 0;
  fz := 0;
  ft := 0;
  fl := 0;
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

procedure TStack.Error(msg: String);
begin
  raise exception.create(Msg) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;

constructor TEngine.create;
begin
  inherited create;
  Stack := TStack.create;
  AngleMode := Degree;
end;

destructor TEngine.destroy;
begin
  if assigned(Stack) then
    Stack.destroy;
  inherited destroy;
end;

procedure TEngine.Add;
begin
  Stack.fl := Stack.x;
  Stack.Push(rpn(Stack.Pop, Stack.Pop, PlusOp));
end;

procedure TEngine.Sub;
var
  operand1, operand2: extended;
begin
  Stack.fl := Stack.x;
  operand1 := Stack.Pop;
  operand2 := Stack.Pop;
  Stack.Push(rpn(operand2, operand1, MinusOp));
end;

procedure TEngine.Times;
begin
  Stack.fl := Stack.x;
  Stack.Push(rpn(Stack.Pop, Stack.Pop, MultOp));
end;

procedure TEngine.Divide;
var
  operand1, operand2: extended;
begin
  Stack.fl := Stack.x;
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
  Stack.fl := Stack.x;
  Stack.Push(rpn(Stack.Pop, InvertOp));
end;

procedure TEngine.PWR;
var
  operand1, operand2: extended;
begin
  Stack.fl := Stack.x;
  operand1 := Stack.Pop;
  operand2 := Stack.Pop;
  Stack.Push(rpn(operand2, operand1, PowerOp));
end;

procedure TEngine.Sinus;
begin
  Stack.fl := Stack.x;
  if AngleMode = Degree then
    Stack.Push(rpn(degtorad(stack.Pop), SinOp))
  else if AngleMode = Turn then
    Stack.Push(rpn(cycletorad(stack.Pop), SinOp))
  else if AngleMode = Grad then
    Stack.Push(rpn(gradtorad(stack.Pop), SinOp))
  else
    Stack.Push(rpn(stack.Pop, SinOp));
end;

procedure TEngine.Cosinus;
begin
  Stack.fl := Stack.x;
  if AngleMode = Degree then
    Stack.Push(rpn(degtorad(stack.Pop), CosOp))
  else if AngleMode = Turn then
    Stack.Push(rpn(cycletorad(stack.Pop), CosOp))
  else if AngleMode = Grad then
    Stack.Push(rpn(gradtorad(stack.Pop), CosOp))
  else
    Stack.Push(rpn(stack.Pop, CosOp));
end;

procedure TEngine.Tangens;
begin
  Stack.fl := Stack.x;
  if AngleMode = Degree then
    Stack.Push(rpn(degtorad(stack.Pop), TanOp))
  else if AngleMode = Turn then
    Stack.Push(rpn(cycletorad(stack.Pop), TanOp))
  else if AngleMode = Grad then
    Stack.Push(rpn(gradtorad(stack.Pop), TanOp))
  else
    Stack.Push(rpn(stack.Pop, TanOp));
end;

procedure TEngine.ArcSinus;
begin
  Stack.fl := Stack.x;
  if AngleMode = Degree then
    Stack.Push(radtodeg(rpn(stack.Pop, ASinOp)))
  else if AngleMode = Turn then
    Stack.Push(radtocycle(rpn(stack.Pop, ASinOp)))
  else if AngleMode = Grad then
    Stack.Push(radtograd(rpn(stack.Pop, ASinOp)))
  else
    Stack.Push(rpn(stack.Pop, ASinOp));
end;

procedure TEngine.ArcCosinus;
begin
  Stack.fl := Stack.x;
  if AngleMode = Degree then
    Stack.Push(radtodeg(rpn(stack.Pop, ACosOp)))
  else if AngleMode = Turn then
    Stack.Push(radtocycle(rpn(stack.Pop, ACosOp)))
  else if AngleMode = Grad then
    Stack.Push(radtograd(rpn(stack.Pop, ACosOp)))
  else
    Stack.Push(rpn(stack.Pop, ACosOp));
end;

procedure TEngine.ArcTangens;
begin
  Stack.fl := Stack.x;
  if AngleMode = Degree then
    Stack.Push(radtodeg(rpn(stack.Pop, ATanOp)))
  else if AngleMode = Turn then
    Stack.Push(radtocycle(rpn(stack.Pop, ATanOp)))
  else if AngleMode = Grad then
    Stack.Push(radtograd(rpn(stack.Pop, ATanOp)))
  else
    Stack.Push(rpn(stack.Pop, ATanOp));
end;

procedure TEngine.sqroot;
begin
  Stack.fl := Stack.x;
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
    result := power(operand1, operand2);
  end;
end;

end.

