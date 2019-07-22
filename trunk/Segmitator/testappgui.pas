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
  StdCtrls, ExtCtrls, segmitator, segmitatorWidgets;

type

  { TTestAppMainForm }

  TTestAppMainForm = class(TForm)
    ItalicCheckBox: TCheckBox;
    FontsCombobox: TComboBox;
    TestPaintbox: TPaintBox;
    TestFloatSpinEdit: TFloatSpinEdit;
    TestASCIIMemo: TMemo;
    procedure FontsComboboxChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ItalicCheckBoxChange(Sender: TObject);
    procedure TestFloatSpinEditChange(Sender: TObject);
    procedure TestPaintboxPaint(Sender: TObject);
  private
    Display: TDisplay;
  public
    courierpos: integer;
    theNumber: real;
    procedure DrawTestPaintBox(Sender: TObject);
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

  TestPaintBox.Invalidate; // necessary for macOS
end;

procedure TTestAppMainForm.TestPaintboxPaint(Sender: TObject);
begin
  DrawTestPaintBox(Sender);
end;

procedure TTestAppMainForm.DrawTestPaintBox(Sender: TObject);
begin
  TestPaintbox.Canvas.Brush.Style := bsSolid;
  TestPaintbox.Canvas.Brush.Color := TestASCIIMemo.Color;
  TestPaintbox.Canvas.FillRect(0, 0, TestPaintbox.Width, TestPaintbox.Height);
  if ItalicCheckBox.Checked then
    Display.Style := [fsItalic]
  else
    Display.Style := [];
  Display.n := theNumber;
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
  Display := TDisplay.create;
  Display.Canvas := TestPaintBox.Canvas;
  Display.Color := clLime;
  Display.Style := [];
  Display.offsetX := 9;
  Display.offsetY := 9;
end;

procedure TTestAppMainForm.FormDestroy(Sender: TObject);
begin
  Display.destroy;
end;

procedure TTestAppMainForm.ItalicCheckBoxChange(Sender: TObject);
begin
  TestPaintBox.Invalidate; // necessary for macOS
end;

procedure TTestAppMainForm.FontsComboboxChange(Sender: TObject);
begin
  TestAsciiMemo.Font.Name := FontsCombobox.Items[FontsCombobox.ItemIndex];
end;

end.

