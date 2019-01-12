program trurl_a;

{ Trurl A }

{ Simple RPN calculator in Object Pascal }

{ Main program file }

{ Version 1.0 (Leopolis) }

{ (c) Johannes W. Dietrich, 2003 - 2018 }

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
  Forms, Controls, GUI, RPNEngine, aboutbox;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title := 'Trurl A';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTrurlAboutBox, TrurlAboutBox);
  TrurlAboutBox.FormStyle := fsStayOnTop;
  Application.BringToFront;
  Application.Run;
end.

