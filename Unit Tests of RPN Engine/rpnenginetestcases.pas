unit RPNEngineTestCases;

{ Trurl }

{ A suite of RPN calculators in Object Pascal }

{ Unit Tests for Basic RPN Engine }

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
  Classes, SysUtils, Controls, fpcunit, testutils, testregistry,
  BCD, RPNEngine, RPNWidgets;

type

  { TControlTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
    procedure CodeVersionCheck;
  end;

  { TBCDTestCases }

  TBCDTestCases = class(TTestCase)
  published
    procedure asRealTest;
    procedure asBCDTest;
  end;

  { TStackTestCases }

  TStackTestCases = class(TTestCase)
  published
    procedure RollDownTest;
    procedure DropDownTest;
    procedure RollUpTest;
    procedure PushTest;
    procedure PopTest;
    procedure ClearTest;
  end;

  { TEngineSingleFunctionTestCases }

  TEngineSingleFunctionTestCases = class(TTestCase)
  published
    procedure AddFunctionTest;
    procedure SubFunctionTest;
    procedure TimesFunctionTest;
    procedure DivideFunctionTest;
    procedure CHSFunctionTest;
    procedure InvFunctionTest;
    procedure PWRFunctionTest;
    procedure SinFunctionTest;
    procedure CosFunctionTest;
    procedure TanFunctionTest;
    procedure ASinFunctionTest;
    procedure ACosFunctionTest;
    procedure ATanFunctionTest;
    procedure sqrootTest;
  end;

  { TEngineRPNFunctionTestCases }

  TEngineRPNFunctionTestCases = class(TTestCase)
  published
    procedure UnaryFuntionTests;
    procedure BinaryFunctionTests;
    procedure CompoundFunctionTests;
  end;

  { TWidgetTestCases }

  TWidgetTestCases = class(TTestCase)
  published
    procedure DisplayTest;
  end;

implementation

{ TBCDTestCases }

procedure TBCDTestCases.asRealTest;
var
  BCDNumber: TBCDFloat;
  RealNumber: real;
  i: integer;
begin
  BCDNumber.sigSign := positive; // +13
  BCDNumber.significand[5] := (16 * 1) + 3;
  for i := 0 to 4 do
    BCDNumber.significand[i] := 0;
  BCDNumber.expSign := positive;
  BCDNumber.exponent[0] := 0;
  BCDNumber.exponent[1] := 0;
  RealNumber := asReal(BCDNumber);
  AssertEquals(13, RealNumber);

  BCDNumber.sigSign := negative; // -107
  BCDNumber.significand[5] := 7;
  BCDNumber.significand[4] := 1;
  for i := 0 to 3 do
    BCDNumber.significand[i] := 0;
  BCDNumber.expSign := positive;
  BCDNumber.exponent[0] := 0;
  BCDNumber.exponent[1] := 0;
  RealNumber := asReal(BCDNumber);
  AssertEquals(-107, RealNumber);

  BCDNumber.sigSign := positive; // +33e13
  BCDNumber.significand[5] := (16 * 3) + 3;
  for i := 0 to 4 do
    BCDNumber.significand[i] := 0;
  BCDNumber.expSign := positive;
  BCDNumber.exponent[0] := 0;
  BCDNumber.exponent[1] := (16 * 1) + 3;
  RealNumber := asReal(BCDNumber);
  AssertEquals(33e13, RealNumber);

  BCDNumber.sigSign := negative; // -18e-21
  BCDNumber.significand[0] := (16 * 1) + 8;
  for i := 1 to 5 do
    BCDNumber.significand[i] := 0;
  BCDNumber.expSign := negative;
  BCDNumber.exponent[0] := 0;
  BCDNumber.exponent[1] := (16 * 2) + 1;
  RealNumber := asReal(BCDNumber);
  AssertEquals(-18e-21, RealNumber);
end;

procedure TBCDTestCases.asBCDTest;
var
  BCDNumber: TBCDFloat;
begin
  BCDNumber := AsBCD(13);
  AssertTrue(BCDNumber.sigSign = positive);
  AssertTrue(BCDNumber.expSign = positive);
  AssertEquals(19, BCDNumber.significand[0]); // 19 corresponds to 1 * 16 + 3 * 1
  AssertEquals(1, BCDNumber.exponent[1]);  // 1 corresponds to 0 * 16 + 1 * 1
  BCDNumber := AsBCD(3.14159265359);
  AssertTrue(BCDNumber.sigSign = positive);
  AssertTrue(BCDNumber.expSign = positive);
  AssertEquals(49, BCDNumber.significand[0]); // 49 corresponds to 3 * 16 + 1 * 1
  AssertEquals(65, BCDNumber.significand[1]); // 65 corresponds to 4 * 16 + 1 * 1
  AssertEquals(89, BCDNumber.significand[2]); // 89 corresponds to 5 * 16 + 9 * 1
  AssertEquals(38, BCDNumber.significand[3]); // 38 corresponds to 2 * 16 + 6 * 1
  AssertEquals(83, BCDNumber.significand[4]); // 83 corresponds to 5 * 16 + 3 * 1
  AssertEquals(89, BCDNumber.significand[5]); // 89 corresponds to 5 * 16 + 9 * 1
  AssertEquals(0, BCDNumber.exponent[1]);
  BCDNumber := AsBCD(123.456);
  AssertTrue(BCDNumber.sigSign = positive);
  AssertTrue(BCDNumber.expSign = positive);
  AssertEquals(18, BCDNumber.significand[0]); // 18 corresponds to 1 * 16 + 2 * 1
  AssertEquals(52, BCDNumber.significand[1]); // 52 corresponds to 3 * 16 + 4 * 1
  AssertEquals(86, BCDNumber.significand[2]); // 86 corresponds to 5 * 16 + 6 * 1
  BCDNumber := AsBCD(-273.15);
  AssertTrue(BCDNumber.sigSign = negative);
  AssertTrue(BCDNumber.expSign = positive);
  AssertEquals(39, BCDNumber.significand[0]); // 39 corresponds to 2 * 16 + 7 * 1
  AssertEquals(49, BCDNumber.significand[1]); // 49 corresponds to 3 * 16 + 1 * 1
  AssertEquals(80, BCDNumber.significand[2]); // 18 corresponds to 5 * 16 + 0 * 1
  BCDNumber := AsBCD(123.456e9);
  AssertTrue(BCDNumber.sigSign = positive);
  AssertTrue(BCDNumber.expSign = positive);
  AssertEquals(18, BCDNumber.significand[0]); // 18 corresponds to 1 * 16 + 2 * 1
  AssertEquals(52, BCDNumber.significand[1]); // 52 corresponds to 3 * 16 + 4 * 1
  AssertEquals(86, BCDNumber.significand[2]); // 86 corresponds to 5 * 16 + 6 * 1
  BCDNumber := AsBCD(-18e-9);
  AssertTrue(BCDNumber.sigSign = negative);
  AssertTrue(BCDNumber.expSign = negative);
  {AssertEquals(24, BCDNumber.significand[0]); // 24 corresponds to 1 * 16 + 8 * 1
  AssertEquals(33, BCDNumber.exponent[0]);  // 33 corresponds to 1 * 16 + 1 * 1  }
end;

{ TWidgetTestCases }

procedure TWidgetTestCases.DisplayTest;
var
  TestControlX, TestControlY, TestControlZ, TestControlT: TControl;
  TestFrame: TFrame;
begin
  TestFrame := TFrame.create;
  TestFrame.Engine := TEngine.create;
  TestFrame.Engine.Stack := TStack.create;
  TestControlX := TControl.create(nil);
  TestControlY := TControl.create(nil);
  TestControlZ := TControl.create(nil);
  TestControlT := TControl.create(nil);
  TestFrame.XRegDisplay := TestControlX;
  TestFrame.YRegDisplay := TestControlY;
  TestFrame.ZRegDisplay := TestControlZ;
  TestFrame.TRegDisplay := TestControlT;
  TestFrame.Engine.Stack.Push(7);
  TestFrame.Engine.Stack.Push(13);
  TestFrame.Engine.Stack.Push(21);
  TestFrame.Engine.Stack.Push(23);
  TestFrame.DisplayRegisters;
  AssertEquals('23', TestControlX.Caption);
  AssertEquals('21', TestControlY.Caption);
  AssertEquals('13', TestControlZ.Caption);
  AssertEquals('7', TestControlT.Caption);
  TestControlX.destroy;
  TestControlY.destroy;
  TestControlZ.destroy;
  TestControlT.destroy;
  TestFrame.Engine.destroy;
  TestFrame.destroy;
end;

{ TEngineSingleFunctionTestCases }

procedure TEngineSingleFunctionTestCases.AddFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.Stack.Push(7);
  TestEngine.Stack.Push(6);
  TestEngine.Add;
  AssertEquals(13, TestEngine.Stack.Pop);
  AssertEquals(6, TestEngine.Stack.lastx);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.SubFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.Stack.Push(24);
  TestEngine.Stack.Push(3);
  TestEngine.Sub;
  AssertEquals(21, TestEngine.Stack.Pop);
  TestEngine.Stack.Push(3);
  TestEngine.Stack.Push(7);
  TestEngine.Sub;
  AssertEquals(-4, TestEngine.Stack.Pop);
  AssertEquals(7, TestEngine.Stack.lastx);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.TimesFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.Stack.Push(3);
  TestEngine.Stack.Push(7);
  TestEngine.Times;
  AssertEquals(21, TestEngine.Stack.Pop);
  AssertEquals(7, TestEngine.Stack.lastx);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.DivideFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.Stack.Push(24);
  TestEngine.Stack.Push(3);
  TestEngine.Divide;
  AssertEquals(8, TestEngine.Stack.Pop);
  AssertEquals(3, TestEngine.Stack.lastx);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.CHSFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.Stack.Push(23);
  TestEngine.CHS;
  AssertEquals(-23, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.InvFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.Stack.Push(5);
  TestEngine.Inv;
  AssertEquals(0.2, TestEngine.Stack.Pop);
  AssertEquals(5, TestEngine.Stack.lastx);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.PWRFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.Stack.Push(5);
  TestEngine.Stack.Push(2);
  TestEngine.PWR;
  AssertEquals(25, TestEngine.Stack.Pop);
  TestEngine.Stack.Push(2);
  TestEngine.Stack.Push(5);
  TestEngine.PWR;
  AssertEquals(32, TestEngine.Stack.Pop);
  AssertEquals(5, TestEngine.Stack.lastx);
  TestEngine.Stack.Push(-2);
  TestEngine.Stack.Push(5);
  TestEngine.PWR;
  AssertEquals(-32, TestEngine.Stack.Pop);
  AssertEquals(5, TestEngine.Stack.lastx);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.SinFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.AngleMode := Degree;
  TestEngine.Stack.Push(90);
  TestEngine.Sinus;
  AssertEquals(1, TestEngine.Stack.Pop);
  TestEngine.Stack.Push(180);
  TestEngine.Sinus;
  AssertEquals(0, TestEngine.Stack.Pop);
  TestEngine.AngleMode := Radian;
  TestEngine.Stack.Push(2 * pi);
  TestEngine.Sinus;
  AssertEquals(0, TestEngine.Stack.Pop);
  TestEngine.AngleMode := Turn;
  TestEngine.Stack.Push(1/4);
  TestEngine.Sinus;
  AssertEquals(1, TestEngine.Stack.Pop);
  TestEngine.AngleMode := Grad;
  TestEngine.Stack.Push(200);
  TestEngine.Sinus;
  AssertEquals(0, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.CosFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.AngleMode := Degree;
  TestEngine.Stack.Push(90);
  TestEngine.Cosinus;
  AssertEquals(0, TestEngine.Stack.Pop);
  TestEngine.Stack.Push(180);
  TestEngine.Cosinus;
  AssertEquals(-1, TestEngine.Stack.Pop);
  TestEngine.AngleMode := Radian;
  TestEngine.Stack.Push(2 * pi);
  TestEngine.Cosinus;
  AssertEquals(1, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.TanFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.AngleMode := Degree;
  TestEngine.Stack.Push(45);
  TestEngine.Tangens;
  AssertEquals(1, TestEngine.Stack.Pop);
  TestEngine.Stack.Push(180);
  TestEngine.Tangens;
  AssertEquals(0, TestEngine.Stack.Pop);
  TestEngine.AngleMode := Radian;
  TestEngine.Stack.Push(2 * pi);
  TestEngine.Tangens;
  AssertEquals(0, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.ASinFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.AngleMode := Degree;
  TestEngine.Stack.Push(1);
  TestEngine.ArcSinus;
  AssertEquals(90, TestEngine.Stack.Pop);
  TestEngine.Stack.Push(0);
  TestEngine.ArcSinus;
  AssertEquals(0, TestEngine.Stack.Pop);
  TestEngine.AngleMode := Radian;
  TestEngine.Stack.Push(-1);
  TestEngine.ArcSinus;
  AssertEquals(-pi / 2, TestEngine.Stack.Pop);
  TestEngine.AngleMode := Turn;
  TestEngine.Stack.Push(1);
  TestEngine.ArcSinus;
  AssertEquals(1/4, TestEngine.Stack.Pop);
  TestEngine.AngleMode := Grad;
  TestEngine.Stack.Push(1);
  TestEngine.ArcSinus;
  AssertEquals(100, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.ACosFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.AngleMode := Degree;
  TestEngine.Stack.Push(1);
  TestEngine.ArcCosinus;
  AssertEquals(0, TestEngine.Stack.Pop);
  TestEngine.Stack.Push(0);
  TestEngine.ArcCosinus;
  AssertEquals(90, TestEngine.Stack.Pop);
  TestEngine.AngleMode := Radian;
  TestEngine.Stack.Push(-1);
  TestEngine.ArcCosinus;
  AssertEquals(pi, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.ATanFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.AngleMode := Degree;
  TestEngine.Stack.Push(1);
  TestEngine.ArcTangens;
  AssertEquals(45, TestEngine.Stack.Pop);
  TestEngine.Stack.Push(0);
  TestEngine.ArcTangens;
  AssertEquals(0, TestEngine.Stack.Pop);
  TestEngine.AngleMode := Radian;
  TestEngine.Stack.Push(-1);
  TestEngine.ArcTangens;
  AssertEquals(-pi / 4, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.sqrootTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.Stack.Push(16);
  TestEngine.sqroot;
  AssertEquals(4, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

{ TStackTestCases }

procedure TStackTestCases.RollDownTest;
var
  TestStack: TStack;
begin
  TestStack := TStack.create;
  TestStack.x := 42;
  TestStack.RollDown;
  AssertEquals(0, TestStack.x);
  TestStack.RollDown;
  TestStack.RollDown;
  TestStack.RollDown;
  AssertEquals(42, TestStack.x);
  TestStack.destroy;
end;

procedure TStackTestCases.DropDownTest;
var
  TestStack: TStack;
begin
  TestStack := TStack.create;
  TestStack.x := 42;
  TestStack.DropDown;
  AssertEquals(0, TestStack.x);
  TestStack.DropDown;
  TestStack.DropDown;
  TestStack.DropDown;
  AssertEquals(0, TestStack.x);
  TestStack.destroy;
end;

procedure TStackTestCases.RollUpTest;
var
  TestStack: TStack;
begin
  TestStack := TStack.create;
  TestStack.x := 42;
  TestStack.RollUp;
  TestStack.x := 0;
  AssertEquals(0, TestStack.x);
  TestStack.RollDown;
  AssertEquals(42, TestStack.x);
  TestStack.destroy;
end;

procedure TStackTestCases.PushTest;
var
  TestStack: TStack;
begin
  TestStack := TStack.create;
  TestStack.Push(42);
  TestStack.Push(13);
  TestStack.Push(0);
  AssertEquals(0, TestStack.x);
  TestStack.RollDown;
  AssertEquals(13, TestStack.x);
  TestStack.RollDown;
  AssertEquals(42, TestStack.x);
  TestStack.destroy;
end;


procedure TStackTestCases.PopTest;
var
  TestStack: TStack;
begin
  TestStack := TStack.create;
  TestStack.Push(42);
  TestStack.Push(13);
  TestStack.Push(0);
  AssertEquals(0, TestStack.Pop);
  AssertEquals(13, TestStack.Pop);
  AssertEquals(42, TestStack.Pop);
  TestStack.destroy;
end;

procedure TStackTestCases.ClearTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.Stack.Push(4);
  TestEngine.Stack.Push(3);
  TestEngine.Stack.Push(2);
  TestEngine.Stack.Push(1);
  TestEngine.Stack.Clear;
  AssertEquals(0, TestEngine.Stack.Pop);
  AssertEquals(0, TestEngine.Stack.Pop);
  AssertEquals(0, TestEngine.Stack.Pop);
  AssertEquals(0, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

{ TEngineFunctionTestCases }

procedure TEngineRPNFunctionTestCases.UnaryFuntionTests;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  AssertEquals(-1, TestEngine.rpn(1, PlusMinusOp));
  AssertEquals(1, TestEngine.rpn(-1, PlusMinusOp));
  AssertEquals(0.2, TestEngine.rpn(5, InvertOp));
  AssertEquals(4, TestEngine.rpn(16, sqrtOp));
  TestEngine.destroy;
end;

procedure TEngineRPNFunctionTestCases.BinaryFunctionTests;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  AssertEquals(1, TestEngine.rpn(-4, 5, PlusOp));
  AssertEquals(7, TestEngine.rpn(13, 6, MinusOp));
  AssertEquals(24, TestEngine.rpn(3, 8, MultOp));
  AssertEquals(2.5, TestEngine.rpn(5, 2, DivOp));
  AssertEquals(2, TestEngine.rpn(5, 2.5, DivOp));
  AssertEquals(32, TestEngine.rpn(2, 5, PowerOp));
  TestEngine.destroy;
end;

procedure TEngineRPNFunctionTestCases.CompoundFunctionTests;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  AssertEquals(0.8, TestEngine.rpn(TestEngine.rpn(2, 2, PlusOp), 5, DivOp));
  AssertEquals(14, TestEngine.rpn(5, TestEngine.rpn(TestEngine.rpn(TestEngine.rpn(1, 2, PlusOp), 4, MultOp), 3, MinusOp), PlusOp));
  AssertEquals(2, TestEngine.rpn(TestEngine.rpn(4, TestEngine.rpn(2, 5, MultOp), PlusOp), TestEngine.rpn(1, TestEngine.rpn(3, 2, MultOp), PlusOp), DivOp));
  TestEngine.destroy;
end;

{ TControlTestCases }

procedure TControlTestCases.PositiveCheck;
{ Positive check, should always succeed }
begin
  AssertNull('This test is bound to succeed', nil);
end;

procedure TControlTestCases.CodeVersionCheck;
{ The subsequent tests are compatible with RPN Engine version 1.0 }
begin
  AssertEquals(1, RPNEngine_major);
  AssertEquals(0, RPNEngine_minor);
end;

initialization

RegisterTest(TControlTestCases);
RegisterTest(TBCDTestCases);
RegisterTest(TStackTestCases);
RegisterTest(TEngineRPNFunctionTestCases);
RegisterTest(TEngineSingleFunctionTestCases);
RegisterTest(TWidgetTestCases);
end.

