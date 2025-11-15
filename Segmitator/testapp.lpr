program testapp;

{ Trurl Segmitator }

{ Suite of RPN calculators in Object Pascal }

{ Test application for Segmitator unit }

{ Version 1.3.0 (Fábrica) }

{ (c) Johannes W. Dietrich, 1990 - 2025 }

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
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, testappgui, segmitator, segmitatorWidgets
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TTestAppMainForm, TestAppMainForm);
  Application.BringToFront;
  Application.Run;
end.

