unit testappgui;

{ Trurl Segmitator }

{ Suite of RPN calculators in Object Pascal }

{ Test application for Segmitator unit }

{ Version 1.0 (Leopolis) }

{ (c) Johannes W. Dietrich, 1990 - 2018 }

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls, segmitator;

type

  { TTestAppMainForm }

  TTestAppMainForm = class(TForm)
    TestFloatSpinEdit: TFloatSpinEdit;
    TestASCIIMemo: TMemo;
    procedure TestFloatSpinEditChange(Sender: TObject);
  private

  public

  end;

var
  TestAppMainForm: TTestAppMainForm;

implementation

{$R *.lfm}

{ TTestAppMainForm }

procedure TTestAppMainForm.TestFloatSpinEditChange(Sender: TObject);
var
  i, digit: integer;
  stringToTest: String[11];
  line1, line2, line3: String;
begin
  stringToTest := FloatToStr(TestFloatSpinEdit.Value);
  TestASCIIMemo.Clear;
  TestASCIIMemo.Append(stringToTest);
  line1 := '';
  line2 := '';
  line3 := '';
  i := 1;
  if length(stringToTest) > 1 then for i := 2 to length(stringToTest) do
  begin
    if stringToTest[i] <> '.' then
      begin
        if TryStrToInt(stringToTest[i - 1], digit) then
          begin
            line1 := line1 + AsciiLine(0, digit);
            line2 := line2 + AsciiLine(1, digit);
            line3 := line3 + AsciiLine(2, digit);
          end;
      end
    else
      begin
        digit := StrToInt(stringToTest[i - 1]);
        line1 := line1 + AsciiLine(0, digit + 10);
        line2 := line2 + AsciiLine(1, digit + 10);
        line3 := line3 + AsciiLine(2, digit + 10);
      end;
  end;
  digit := StrToInt(stringToTest[i]);
  line1 := line1 + AsciiLine(0, digit);
  line2 := line2 + AsciiLine(1, digit);
  line3 := line3 + AsciiLine(2, digit);
  TestASCIIMemo.Append(line1);
  TestASCIIMemo.Append(line2);
  TestASCIIMemo.Append(line3);
end;

end.

