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
  Classes, SysUtils, fpcunit, testutils, testregistry, RPNEngine;

type

  { TControlTestCases }

  TControlTestCases = class(TTestCase)
  published
    procedure PositiveCheck;
    procedure CodeVersionCheck;
  end;

  { TStackTestCases }

  TStackTestCases = class(TTestCase)
  published
    procedure RollDownTest;
    procedure DropDownTest;
    procedure RollUpTest;
    procedure PushTest;
    procedure PopTest;
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
  end;

  { TEngineRPNFunctionTestCases }

  TEngineRPNFunctionTestCases = class(TTestCase)
  published
    procedure UnaryFuntionTests;
    procedure BinaryFunctionTests;
    procedure CompoundFunctionTests;
  end;

implementation

{ TEngineSingleFunctionTestCases }

procedure TEngineSingleFunctionTestCases.AddFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.stack.Push(7);
  TestEngine.stack.Push(6);
  TestEngine.Add;
  AssertEquals(13, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.SubFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.stack.Push(24);
  TestEngine.stack.Push(3);
  TestEngine.Sub;
  AssertEquals(21, TestEngine.Stack.Pop);
  TestEngine.stack.Push(3);
  TestEngine.stack.Push(7);
  TestEngine.Sub;
  AssertEquals(-4, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.TimesFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.stack.Push(3);
  TestEngine.stack.Push(7);
  TestEngine.Times;
  AssertEquals(21, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.DivideFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.stack.Push(24);
  TestEngine.stack.Push(3);
  TestEngine.Divide;
  AssertEquals(8, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.CHSFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.stack.Push(23);
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
  TestEngine.stack.Push(5);
  TestEngine.Inv;
  AssertEquals(0.2, TestEngine.Stack.Pop);
  TestEngine.destroy;
end;

procedure TEngineSingleFunctionTestCases.PWRFunctionTest;
var
  TestEngine: TEngine;
begin
  TestEngine := TEngine.create;
  TestEngine.Stack := TStack.create;
  TestEngine.stack.Push(5);
  TestEngine.stack.Push(2);
  TestEngine.PWR;
  AssertEquals(25, TestEngine.Stack.Pop);
  TestEngine.stack.Push(2);
  TestEngine.stack.Push(5);
  TestEngine.PWR;
  AssertEquals(32, TestEngine.Stack.Pop);
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
RegisterTest(TStackTestCases);
RegisterTest(TEngineRPNFunctionTestCases);
RegisterTest(TEngineSingleFunctionTestCases);
end.

