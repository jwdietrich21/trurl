unit GUI;

{ Trurl G }

{ RPN calculator in Object Pascal, inspired by the Apollo Guidance Computer }

{ GUI }

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

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LclIntf, LCLType, Menus, ActnList, StdActns, ExtCtrls, Clipbrd, Buttons,
  RPNEngine, RPNWidgets, segmitatorWidgets, aboutbox;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionList1: TActionList;
    NumlockLabel1: TLabel;
    NumlockLabel2: TLabel;
    XRegisterPaintBox: TPaintBox;
    PlusMinusSpeedButton: TSpeedButton;
    DivSpeedButton: TSpeedButton;
    RollDownSpeedButton: TSpeedButton;
    TimesSpeedButton: TSpeedButton;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditPaste1: TEditPaste;
    EditUndo1: TEditUndo;
    DisplayBackgroundPanel: TPanel;
    CSpeedButton: TSpeedButton;
    EightSpeedButton: TSpeedButton;
    SevenSpeedButton: TSpeedButton;
    PlusSpeedButton: TSpeedButton;
    OneSpeedButton: TSpeedButton;
    FourSpeedButton: TSpeedButton;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    Shape16: TShape;
    Shape17: TShape;
    Shape3: TShape;
    NumLockIndicator: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    SixSpeedButton: TSpeedButton;
    NineSpeedButton: TSpeedButton;
    TwoSpeedButton: TSpeedButton;
    ThreeSpeedButton: TSpeedButton;
    DotSpeedButton: TSpeedButton;
    FiveSpeedButton: TSpeedButton;
    YRegisterPaintBox: TPaintBox;
    ZRegisterPaintBox: TPaintBox;
    TRegisterPaintBox: TPaintBox;
    ZeroSpeedButton: TSpeedButton;
    KeyCheckTimer: TTimer;
    EnterSpeedButton: TSpeedButton;
    Shape1: TShape;
    Shape2: TShape;
    MinusSpeedButton: TSpeedButton;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    WinAboutItem: TMenuItem;
    AppleMenu: TMenuItem;
    MacAboutItem: TMenuItem;
    QuitItem: TMenuItem;
    EditMenu: TMenuItem;
    UndoItem: TMenuItem;
    RedoItem: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    Dividier_2_0: TMenuItem;
    Nr0Button: TButton;
    Nr1Button: TButton;
    Nr2Button: TButton;
    Nr3Button: TButton;
    Nr4Button: TButton;
    Nr5Button: TButton;
    Nr6Button: TButton;
    Nr7Button: TButton;
    Nr8Button: TButton;
    Nr9Button: TButton;
    DotButton: TButton;
    RDButton: TButton;
    PlusButton: TButton;
    MinusButton: TButton;
    TimesButton: TButton;
    DivButton: TButton;
    CButton: TButton;
    EnterButton: TButton;
    PlusMinusButton: TButton;
    procedure AdaptMenus;
    procedure ACosButtonClick(Sender: TObject);
    procedure ASinButtonClick(Sender: TObject);
    procedure ATanButtonClick(Sender: TObject);
    procedure CButtonClick(Sender: TObject);
    procedure CosButtonClick(Sender: TObject);
    procedure DivButtonClick(Sender: TObject);
    procedure DotButtonClick(Sender: TObject);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditCut1Execute(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure EnterButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IndicateNumLockState(Sender: TObject);
    procedure InvButtonClick(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure TRegisterPaintBoxPaint(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
    procedure MinusButtonClick(Sender: TObject);
    procedure Nr0ButtonClick(Sender: TObject);
    procedure Nr1ButtonClick(Sender: TObject);
    procedure Nr2ButtonClick(Sender: TObject);
    procedure Nr3ButtonClick(Sender: TObject);
    procedure Nr4ButtonClick(Sender: TObject);
    procedure Nr5ButtonClick(Sender: TObject);
    procedure Nr6ButtonClick(Sender: TObject);
    procedure Nr7ButtonClick(Sender: TObject);
    procedure Nr8ButtonClick(Sender: TObject);
    procedure Nr9ButtonClick(Sender: TObject);
    procedure PlusButtonClick(Sender: TObject);
    procedure PlusMinusButtonClick(Sender: TObject);
    procedure PwrButtonClick(Sender: TObject);
    procedure RDButtonClick(Sender: TObject);
    procedure SinButtonClick(Sender: TObject);
    procedure SqrtButtonClick(Sender: TObject);
    procedure TanButtonClick(Sender: TObject);
    procedure TimesButtonClick(Sender: TObject);
    procedure XRegisterPaintBoxPaint(Sender: TObject);
    procedure YRegisterPaintBoxPaint(Sender: TObject);
    procedure ZRegisterPaintBoxPaint(Sender: TObject);
  private
    DisplayX, DisplayY, DisplayZ, DisplayT: TDisplay;
    XRegisterBuffer: TControl;
  public
    Engine: TEngine;
    Frame: TFrame;
    procedure RedrawDisplay(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AdaptMenus;
  XRegisterBuffer := TControl.create(nil);
  Engine := TEngine.create;
  Frame := TFrame.create;
  Frame.Engine := Engine;
  Frame.XRegDisplay := XRegisterBuffer;
  Frame.YRegDisplay := nil; // YRegisterDisplay;
  Frame.ZRegDisplay := nil; // ZRegisterDisplay;
  Frame.TRegDisplay := nil; // TRegisterDisplay;
  Frame.EntryMode := Number;
  DisplayX := TDisplay.create;
  DisplayX.Canvas := XRegisterPaintBox.Canvas;
  DisplayX.Color := clLime;
  DisplayX.scale := 1;
  DisplayX.Style := [];
  DisplayX.offsetX := 9;
  DisplayX.offsetY := 9;
  DisplayY := TDisplay.create;
  DisplayY.Canvas := YRegisterPaintBox.Canvas;
  DisplayY.Color := clLime;
  DisplayY.scale := 1;
  DisplayY.Style := [];
  DisplayY.offsetX := 9;
  DisplayY.offsetY := 9;
  DisplayZ := TDisplay.create;
  DisplayZ.Canvas := ZRegisterPaintBox.Canvas;
  DisplayZ.Color := clLime;
  DisplayZ.scale := 1;
  DisplayZ.Style := [];
  DisplayZ.offsetX := 9;
  DisplayZ.offsetY := 9;
  DisplayT := TDisplay.create;
  DisplayT.Canvas := TRegisterPaintBox.Canvas;
  DisplayT.Color := clLime;
  DisplayT.scale := 1;
  DisplayT.Style := [];
  DisplayT.offsetX := 9;
  DisplayT.offsetY := 9;
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Frame.destroy;
  Engine.Destroy;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  case key of
    '0': Nr0ButtonClick(Sender);
    '1': Nr1ButtonClick(Sender);
    '2': Nr2ButtonClick(Sender);
    '3': Nr3ButtonClick(Sender);
    '4': Nr4ButtonClick(Sender);
    '5': Nr5ButtonClick(Sender);
    '6': Nr6ButtonClick(Sender);
    '7': Nr7ButtonClick(Sender);
    '8': Nr8ButtonClick(Sender);
    '9': Nr9ButtonClick(Sender);
    '.', ',': DotButtonClick(Sender);
    '+': PlusButtonClick(Sender);
    '-': MinusButtonClick(Sender);
    '/': DivButtonClick(Sender);
    '*': TimesButtonClick(Sender);
    'c', 'C': CButtonClick(Sender);
    {$IFDEF LINUX}
      #13: EnterButtonClick(Sender);
    {$ENDIF}
  end;
  Key := #0; // Necessary for Cocoa widgetset
  IndicateNumLockState(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.MacAboutItemClick(Sender: TObject);
begin
  TrurlAboutBox.ShowModal;
end;

procedure TMainForm.WinAboutItemClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

procedure TMainForm.QuitItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainForm.AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
  begin
    {$IF DEFINED(LCLcarbon) or DEFINED(LCLCocoa)}
      modifierKey := [ssMeta];
      WinAboutItem.Visible := False;
      AppleMenu.Visible := True;
    {$ELSE}
      modifierKey := [ssCtrl];
      WinAboutItem.Visible := True;
      AppleMenu.Visible := False;
    {$ENDIF}
      QuitItem.ShortCut := ShortCut(VK_Q, modifierKey);
      UndoItem.ShortCut := ShortCut(VK_Z, modifierKey);
      RedoItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
      CutItem.ShortCut := ShortCut(VK_X, modifierKey);
      CopyItem.ShortCut := ShortCut(VK_C, modifierKey);
      PasteItem.ShortCut := ShortCut(VK_V, modifierKey);
  end;

procedure TMainForm.EditCopy1Execute(Sender: TObject);
begin
  Clipboard.AsText := XRegisterBuffer.Caption;
end;

procedure TMainForm.EditCut1Execute(Sender: TObject);
begin
  Clipboard.AsText := XRegisterBuffer.Caption;
  Frame.Engine.Stack.DropDown;
  Frame.DisplayRegisters;
  RedrawDisplay(Sender);
end;

procedure TMainForm.EditPaste1Execute(Sender: TObject);
begin
  Frame.InsertString(Clipboard.AsText);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  case key of
    VK_C, VK_CLEAR, VK_BACK, VK_DELETE: CButtonClick(Sender);
    VK_DOWN: RDButtonClick(Sender);
  end;
  IndicateNumLockState(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  IndicateNumLockState(Sender);
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  IndicateNumLockState(Sender);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  IndicateNumLockState(Sender);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  IndicateNumLockState(Sender);
end;

procedure TMainForm.IndicateNumLockState(Sender: TObject);
begin
  {$IF DEFINED(LCLcarbon) or DEFINED(LCLCocoa)}
    NumLockIndicator.Brush.Color := clYellow;
  {$ELSE}
  if Odd(GetKeyState(VK_NUMLOCK)) then
  begin
    NumLockIndicator.Brush.Color := clLime;
    NumLockIndicator.Hint := 'Num Lock is activated. You can enter numbers via the numeric keypad.';
  end
  else
  begin
    NumLockIndicator.Brush.Color := clRed;
    NumLockIndicator.Hint := 'Num Lock is deactivated. Numbers cannot be entered via the numeric keypad.';
  end;
  {$ENDIF}
end;

procedure TMainForm.Nr0ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('0');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.Nr1ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('1');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.Nr2ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('2');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.Nr3ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('3');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.Nr4ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('4');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.Nr5ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('5');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.Nr6ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('6');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.Nr7ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('7');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.Nr8ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('8');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.Nr9ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('9');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.DotButtonClick(Sender: TObject);
begin
  Frame.AppendChar('.');
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.EnterButtonClick(Sender: TObject);
begin
  Frame.HandleEnter;
  RedrawDisplay(Sender);
end;

procedure TMainForm.InvButtonClick(Sender: TObject);
begin
  Frame.HandleInv;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.CButtonClick(Sender: TObject);
begin
  Frame.HandleClear;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.PlusButtonClick(Sender: TObject);
begin
  Frame.HandleAdd;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.MinusButtonClick(Sender: TObject);
begin
  Frame.HandleSub;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.TimesButtonClick(Sender: TObject);
begin
  Frame.HandleTimes;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.XRegisterPaintBoxPaint(Sender: TObject);
begin
  XRegisterPaintBox.Canvas.Brush.Style := bsSolid;
  XRegisterPaintBox.Canvas.Brush.Color := DisplayBackgroundPanel.Brush.Color;
  XRegisterPaintBox.Canvas.FillRect(0, 0, XRegisterPaintBox.Width, XRegisterPaintBox.Height);
  DisplayX.Style := [fsItalic];
  DisplayX.n := Engine.Stack.x;
end;

procedure TMainForm.YRegisterPaintBoxPaint(Sender: TObject);
begin
  YRegisterPaintBox.Canvas.Brush.Style := bsSolid;
  YRegisterPaintBox.Canvas.Brush.Color := DisplayBackgroundPanel.Brush.Color;
  YRegisterPaintBox.Canvas.FillRect(0, 0, YRegisterPaintBox.Width, YRegisterPaintBox.Height);
  DisplayY.Style := [fsItalic];
  DisplayY.n := Engine.Stack.y;
end;

procedure TMainForm.ZRegisterPaintBoxPaint(Sender: TObject);
begin
  ZRegisterPaintBox.Canvas.Brush.Style := bsSolid;
  ZRegisterPaintBox.Canvas.Brush.Color := DisplayBackgroundPanel.Brush.Color;
  ZRegisterPaintBox.Canvas.FillRect(0, 0, ZRegisterPaintBox.Width, ZRegisterPaintBox.Height);
  DisplayZ.Style := [fsItalic];
  DisplayZ.n := Engine.Stack.z;
end;

procedure TMainForm.TRegisterPaintBoxPaint(Sender: TObject);
begin
  TRegisterPaintBox.Canvas.Brush.Style := bsSolid;
  TRegisterPaintBox.Canvas.Brush.Color := DisplayBackgroundPanel.Brush.Color;
  TRegisterPaintBox.Canvas.FillRect(0, 0, TRegisterPaintBox.Width, TRegisterPaintBox.Height);
  DisplayT.Style := [fsItalic];
  DisplayT.n := Engine.Stack.t;
end;

procedure TMainForm.RedrawDisplay(Sender: TObject);
begin
  XRegisterPaintBox.Invalidate;
  YRegisterPaintBox.Invalidate;
  ZRegisterPaintBox.Invalidate;
  TRegisterPaintBox.Invalidate;
end;

procedure TMainForm.DivButtonClick(Sender: TObject);
begin
  Frame.HandleDiv;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.PlusMinusButtonClick(Sender: TObject);
begin
  Frame.HandleCHS;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.PwrButtonClick(Sender: TObject);
begin
  Frame.HandlePWR;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.RDButtonClick(Sender: TObject);
begin
  Frame.HandleRollDown;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.SinButtonClick(Sender: TObject);
begin
  Frame.HandleSin;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.ASinButtonClick(Sender: TObject);
begin
  Frame.HandleASin;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.CosButtonClick(Sender: TObject);
begin
  Frame.HandleCos;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.ACosButtonClick(Sender: TObject);
begin
  Frame.HandleACos;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.TanButtonClick(Sender: TObject);
begin
  Frame.HandleTan;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.ATanButtonClick(Sender: TObject);
begin
  Frame.HandleATan;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

procedure TMainForm.SqrtButtonClick(Sender: TObject);
begin
  Frame.HandleSqrt;
  RedrawDisplay(Sender);
  ActiveControl := EnterButton;
end;

end.


