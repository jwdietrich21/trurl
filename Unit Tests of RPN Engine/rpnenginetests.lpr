program rpnenginetests;

{ Trurl }

{ A suite of RPN calculators in Object Pascal }

{ Unit Tests for Basic RPN Engine }

{ Version 1.2.0 (Gimel) }

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

uses
  Interfaces, Forms, GuiTestRunner, RPNEngineTestCases;

{$R *.res}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

