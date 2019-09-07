program trurl_g;

{ Trurl G }

{ RPN calculator in Object Pascal, inspired by the Apollo Guidance Computer }

{ Main program file }

{ Version 1.0.1 (Apollo) }

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  LCLVersion, Forms, Controls, GUI, RPNEngine, aboutbox, RPNWidgets;

{$R *.res}

begin
  {$IF (LCL_MAJOR >= 2) OR (LCL_MAJOR >= 1) AND (LCL_MINOR >=8)}
    {$DEFINE NewLaz}
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Title:='Trurl G';
  {$IFDEF NewLaz}
    Application.Scaled := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTrurlAboutBox, TrurlAboutBox);
  TrurlAboutBox.FormStyle := fsStayOnTop;
  Application.BringToFront;
  Application.Run;
end.

