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

  { TEngineFunctionTestCases }

  TEngineFunctionTestCases = class(TTestCase)
  published
    procedure UnaryFuntionTests;
    procedure BinaryFunctionTests;
    procedure CompoundFunctionTests;
  end;

implementation

{ TEngineFunctionTestCases }

procedure TEngineFunctionTestCases.UnaryFuntionTests;
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

procedure TEngineFunctionTestCases.BinaryFunctionTests;
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

procedure TEngineFunctionTestCases.CompoundFunctionTests;
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
RegisterTest(TEngineFunctionTestCases);

end.

