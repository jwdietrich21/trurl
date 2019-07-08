unit testappgui;

{ Trurl Segmitator }

{ Suite of RPN calculators in Object Pascal }

{ Test application for Segmitator unit }

{ Version 1.0 (Cook) }

{ (c) Johannes W. Dietrich, 1990 - 2019 }

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
  StdCtrls, ExtCtrls, segmitator;

type

  { TTestAppMainForm }

  TTestAppMainForm = class(TForm)
    FontsCombobox: TComboBox;
    TestPaintbox: TPaintBox;
    TestFloatSpinEdit: TFloatSpinEdit;
    TestASCIIMemo: TMemo;
    procedure FontsComboboxChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure TestFloatSpinEditChange(Sender: TObject);
  private

  public
    courierpos: integer;
    theNumber: real;
  end;

var
  TestAppMainForm: TTestAppMainForm;

implementation

{$R *.lfm}

{ TTestAppMainForm }

procedure TTestAppMainForm.TestFloatSpinEditChange(Sender: TObject);
var
  stringToTest: string[11];
  DisplayStrings: TASCIIDisplay;
begin
  theNumber := TestFloatSpinEdit.Value;
  stringToTest := FloatToStr(theNumber);

  TestASCIIMemo.Clear;
  TestASCIIMemo.Append(stringToTest);
  DisplayStrings := ASCIIDigits(theNumber);
  TestASCIIMemo.Append(DisplayStrings[0]);
  TestASCIIMemo.Append(DisplayStrings[1]);
  TestASCIIMemo.Append(DisplayStrings[2]);

  DrawDigits(TestPaintBox.Canvas, clLime, theNumber);
end;

procedure TTestAppMainForm.FormActivate(Sender: TObject);
begin
  FontsCombobox.Items.Assign(Screen.Fonts);
  courierpos := FontsCombobox.Items.IndexOf('Courier');
  if courierpos = -1 then
    courierpos := FontsCombobox.Items.IndexOf('Courier New');
  if courierpos >= 0 then
    begin
      FontsCombobox.ItemIndex := courierpos;
      TestAsciiMemo.Font.Name := FontsCombobox.Items[FontsCombobox.ItemIndex];
    end;
end;

procedure TTestAppMainForm.FormCreate(Sender: TObject);
begin
  theNumber := 0;
end;

procedure TTestAppMainForm.FormPaint(Sender: TObject);
begin
  TestPaintbox.Canvas.Brush.Style := bsSolid;
  TestPaintbox.Canvas.Brush.Color := TestASCIIMemo.Color;
  TestPaintbox.Canvas.FillRect(0, 0, TestPaintbox.Width, TestPaintbox.Height);
  DrawDigits(TestPaintbox.Canvas, clLime, theNumber);
end;

procedure TTestAppMainForm.FontsComboboxChange(Sender: TObject);
begin
  TestAsciiMemo.Font.Name := FontsCombobox.Items[FontsCombobox.ItemIndex];
end;

end.

