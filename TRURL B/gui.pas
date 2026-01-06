unit GUI;

{ Trurl B }

{ RPN calculator in Object Pascal, inspired by Braun calculators }

{ GUI }

{ Version 1.0.1 (functio) }

{ (c) Johannes W. Dietrich, 2003 - 2026 }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Math,
  LclIntf, LCLType, Menus, ActnList, StdActns, ExtCtrls, Clipbrd, Buttons,
  RPNEngine, RPNWidgets, segmitatorWidgets, aboutbox
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}, Types;

const
  clInactiveIndicator = $00565557;
  kNumLockOn = 'Num Lock is activated. You can enter numbers via the numeric keypad.';
  kNumLockOff = 'Num Lock is deactivated. Numbers cannot be entered via the numeric keypad.';

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionList1: TActionList;
    ErrorLabel: TLabel;
    Image1: TImage;
    InvSpeedButton: TSpeedButton;
    Panel1: TPanel;
    Panel0: TPanel;
    PanelEnter: TPanel;
    PanelPlusMinus: TPanel;
    PanelInv: TPanel;
    PanelSqr: TPanel;
    PanelSqrt: TPanel;
    PanelTimes: TPanel;
    PanelPlus: TPanel;
    PanelDot: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelC: TPanel;
    PanelMinus: TPanel;
    PanelRD: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelPwr: TPanel;
    PanelDiv: TPanel;
    PwrSpeedButton: TSpeedButton;
    SqrSpeedButton: TSpeedButton;
    SqrtSpeedButton: TSpeedButton;
    TimerPlusMinus: TTimer;
    VirtualEnterButton: TButton;
    PressedButtons: TImageList;
    StandardButtons: TImageList;
    NumlockLabel: TLabel;
    Timer1: TTimer;
    Timer0: TTimer;
    TimerEnter: TTimer;
    TimerClear: TTimer;
    TimerDot: TTimer;
    TimerDiv: TTimer;
    TimerTimes: TTimer;
    TimerMinus: TTimer;
    TimerPlus: TTimer;
    Timer2: TTimer;
    Timer3: TTimer;
    Timer4: TTimer;
    Timer5: TTimer;
    Timer6: TTimer;
    Timer7: TTimer;
    Timer8: TTimer;
    Timer9: TTimer;
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
    NumLockIndicator: TShape;
    ErrorIndicator: TShape;
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
    procedure AdaptMenus;
    procedure ACosButtonClick(Sender: TObject);
    procedure ASinButtonClick(Sender: TObject);
    procedure ATanButtonClick(Sender: TObject);
    procedure CButtonClick(Sender: TObject);
    procedure CosButtonClick(Sender: TObject);
    procedure CSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure CSpeedButtonMouseEnter(Sender: TObject);
    procedure CSpeedButtonMouseLeave(Sender: TObject);
    procedure CSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DivButtonClick(Sender: TObject);
    procedure DivSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DivSpeedButtonMouseEnter(Sender: TObject);
    procedure DivSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DotButtonClick(Sender: TObject);
    procedure DotSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DotSpeedButtonMouseEnter(Sender: TObject);
    procedure DotSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditCut1Execute(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure EightSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EightSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EnterButtonClick(Sender: TObject);
    procedure EnterSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EnterSpeedButtonMouseEnter(Sender: TObject);
    procedure EnterSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FiveSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FiveSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FourSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FourSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Image1Click(Sender: TObject);
    procedure IndicateNumLockState(Sender: TObject);
    procedure IndicateErrorState(Sender: TObject);
    procedure InvButtonClick(Sender: TObject);
    procedure InvSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure InvSpeedButtonMouseEnter(Sender: TObject);
    procedure InvSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MacAboutItemClick(Sender: TObject);
    procedure MinusSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MinusSpeedButtonMouseEnter(Sender: TObject);
    procedure MinusSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure NineSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure NineSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure NumLockIndicatorClick(Sender: TObject);
    procedure NumlockLabelClick(Sender: TObject);
    procedure OneSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure OneSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel0MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel0MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel4MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel5MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel5MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel6MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel7MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel7MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel8MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel8MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel9MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Panel9MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelCMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelCMouseLeave(Sender: TObject);
    procedure PanelCMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelDivMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelDivMouseLeave(Sender: TObject);
    procedure PanelDivMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelDotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelDotMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelEnterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelEnterMouseLeave(Sender: TObject);
    procedure PanelEnterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelInvMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelInvMouseLeave(Sender: TObject);
    procedure PanelInvMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelMinusMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelMinusMouseLeave(Sender: TObject);
    procedure PanelMinusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelPlusMinusMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelPlusMinusMouseLeave(Sender: TObject);
    procedure PanelPlusMinusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelPlusMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelPlusMouseLeave(Sender: TObject);
    procedure PanelPlusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelPwrMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelPwrMouseLeave(Sender: TObject);
    procedure PanelPwrMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelRDMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelRDMouseLeave(Sender: TObject);
    procedure PanelRDMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelSqrMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelSqrMouseLeave(Sender: TObject);
    procedure PanelSqrMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelSqrtMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelSqrtMouseLeave(Sender: TObject);
    procedure PanelSqrtMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelTimesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelTimesMouseLeave(Sender: TObject);
    procedure PanelTimesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PlusMinusSpeedButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PlusMinusSpeedButtonMouseEnter(Sender: TObject);
    procedure PlusMinusSpeedButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PlusSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PlusSpeedButtonMouseEnter(Sender: TObject);
    procedure PlusSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PwrSpeedButtonMouseEnter(Sender: TObject);
    procedure RollDownSpeedButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure RollDownSpeedButtonMouseEnter(Sender: TObject);
    procedure RollDownSpeedButtonMouseLeave(Sender: TObject);
    procedure RollDownSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SevenSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SevenSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Shape2Click(Sender: TObject);
    procedure SixSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SixSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SqrButtonClick(Sender: TObject);
    procedure SqrSpeedButtonMouseEnter(Sender: TObject);
    procedure SqrtSpeedButtonMouseEnter(Sender: TObject);
    procedure ThreeSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ThreeSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Timer0StartTimer(Sender: TObject);
    procedure Timer0Timer(Sender: TObject);
    procedure Timer1StartTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2StartTimer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3StartTimer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure Timer4StartTimer(Sender: TObject);
    procedure Timer4Timer(Sender: TObject);
    procedure Timer5StartTimer(Sender: TObject);
    procedure Timer5Timer(Sender: TObject);
    procedure Timer6StartTimer(Sender: TObject);
    procedure Timer6Timer(Sender: TObject);
    procedure Timer7StartTimer(Sender: TObject);
    procedure Timer7Timer(Sender: TObject);
    procedure Timer8StartTimer(Sender: TObject);
    procedure Timer8Timer(Sender: TObject);
    procedure Timer9StartTimer(Sender: TObject);
    procedure Timer9Timer(Sender: TObject);
    procedure TimerClearStartTimer(Sender: TObject);
    procedure TimerClearTimer(Sender: TObject);
    procedure TimerDivStartTimer(Sender: TObject);
    procedure TimerDivTimer(Sender: TObject);
    procedure TimerDotStartTimer(Sender: TObject);
    procedure TimerDotTimer(Sender: TObject);
    procedure TimerEnterStartTimer(Sender: TObject);
    procedure TimerEnterTimer(Sender: TObject);
    procedure TimerMinusStartTimer(Sender: TObject);
    procedure TimerMinusTimer(Sender: TObject);
    procedure TimerPlusMinusStartTimer(Sender: TObject);
    procedure TimerPlusMinusTimer(Sender: TObject);
    procedure TimerPlusStartTimer(Sender: TObject);
    procedure TimerPlusTimer(Sender: TObject);
    procedure TimerTimesStartTimer(Sender: TObject);
    procedure TimerTimesTimer(Sender: TObject);
    procedure TimesSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TimesSpeedButtonMouseEnter(Sender: TObject);
    procedure TimesSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TRegisterPaintBoxPaint(Sender: TObject);
    procedure TwoSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TwoSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
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
    procedure ZeroSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ZeroSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ZRegisterPaintBoxPaint(Sender: TObject);
  private
    DisplayX, DisplayY, DisplayZ, DisplayT: TDisplay;
    XRegisterBuffer: TControl;
    ErrorState: boolean;
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
const
  kdigitScale = 1;
  kdecScale = 2;
  kStandardRes = 96;
  kDisplayLength = 11;
var
  screenDPI, formDPI, decSize: integer;
begin
  formDPI := PixelsPerInch;
  screenDPI := Screen.PixelsPerInch;
  if screenDPI <= kStandardRes * 1.5 then
    decSize := kdecScale * 2
  else
    decSize := kdecScale;
  AdaptMenus;
  XRegisterBuffer := TControl.Create(nil);
  Engine := TEngine.Create;
  Frame := TFrame.Create;
  Frame.Engine := Engine;
  Frame.XRegDisplay := XRegisterBuffer;
  Frame.YRegDisplay := nil; // YRegisterDisplay;
  Frame.ZRegDisplay := nil; // ZRegisterDisplay;
  Frame.TRegDisplay := nil; // TRegisterDisplay;
  Frame.EntryMode := Number;
  Frame.ReplaceXAfterRollDown := True;
  DisplayX := TDisplay.Create;
  DisplayX.l := kDisplayLength;
  DisplayX.Canvas := XRegisterPaintBox.Canvas;
  DisplayX.Color := clAqua;
  DisplayX.scale := kdigitScale;
  DisplayX.decScale := decSize;
  DisplayX.Style := [];
  DisplayX.offsetX := 9;
  DisplayX.offsetY := 9;
  DisplayY := TDisplay.Create;
  DisplayY.l := kDisplayLength;
  DisplayY.Canvas := YRegisterPaintBox.Canvas;
  DisplayY.Color := clAqua;
  DisplayY.scale := kdigitScale;
  DisplayY.decScale := decSize;
  DisplayY.Style := [];
  DisplayY.offsetX := 9;
  DisplayY.offsetY := 9;
  DisplayZ := TDisplay.Create;
  DisplayZ.l := kDisplayLength;
  DisplayZ.Canvas := ZRegisterPaintBox.Canvas;
  DisplayZ.Color := clAqua;
  DisplayZ.scale := kdigitScale;
  DisplayZ.decScale := decSize;
  DisplayZ.Style := [];
  DisplayZ.offsetX := 9;
  DisplayZ.offsetY := 9;
  DisplayT := TDisplay.Create;
  DisplayT.l := kDisplayLength;
  DisplayT.Canvas := TRegisterPaintBox.Canvas;
  DisplayT.Color := clAqua;
  DisplayT.scale := kdigitScale;
  DisplayT.decScale := decSize;
  DisplayT.Style := [];
  DisplayT.offsetX := 9;
  DisplayT.offsetY := 9;
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Frame.Destroy;
  Engine.Destroy;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  case key of
    '0':
    begin
      Timer0.Enabled := True;
      Nr0ButtonClick(Sender);
    end;
    '1':
    begin
      Timer1.Enabled := True;
      Nr1ButtonClick(Sender);
    end;
    '2':
    begin
      Timer2.Enabled := True;
      Nr2ButtonClick(Sender);
    end;
    '3':
    begin
      Timer3.Enabled := True;
      Nr3ButtonClick(Sender);
    end;
    '4':
    begin
      Timer4.Enabled := True;
      Nr4ButtonClick(Sender);
    end;
    '5':
    begin
      Timer5.Enabled := True;
      Nr5ButtonClick(Sender);
    end;
    '6':
    begin
      Timer6.Enabled := True;
      Nr6ButtonClick(Sender);
    end;
    '7':
    begin
      Timer7.Enabled := True;
      Nr7ButtonClick(Sender);
    end;
    '8':
    begin
      Timer8.Enabled := True;
      Nr8ButtonClick(Sender);
    end;
    '9':
    begin
      Timer9.Enabled := True;
      Nr9ButtonClick(Sender);
    end;
    '.', ',':
    begin
      TimerDot.Enabled := True;
      DotButtonClick(Sender);
    end;
    '+':
    begin
      TimerPlus.Enabled := True;
      PlusButtonClick(Sender);
    end;
    '-':
    begin
      TimerMinus.Enabled := True;
      MinusButtonClick(Sender);
    end;
    '/':
    begin
      TimerDiv.Enabled := True;
      DivButtonClick(Sender);
    end;
    '*':
    begin
      TimerTimes.Enabled := True;
      TimesButtonClick(Sender);
    end;
    'c', 'C':
    begin
      TimerClear.Enabled := True;
      CButtonClick(Sender);
    end;
    'p', 'P':
    begin
      TimerPlusMinus.Enabled := True;
      PlusMinusButtonClick(Sender);
    end;
    {$IF DEFINED(LINUX) or DEFINED(LCLCocoa)
        or DEFINED(LCLQt) or DEFINED(LCLQt5)
        or DEFINED(LCLGtk2) or DEFINED(LCLGtk3)}
    #13:
      begin
        TimerEnter.Enabled := true;
        EnterButtonClick(Sender);
      end;
    {$ENDIF}
  end;
  Key := #0; // Necessary for Cocoa widgetset
  IndicateNumLockState(Sender);
  IndicateErrorState(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.MacAboutItemClick(Sender: TObject);
begin
  TrurlAboutBox.ShowModal;
end;

procedure TMainForm.NumlockLabelClick(Sender: TObject);
var
  keyState: TKeyboardState;
begin
  {$IFDEF WINDOWS}
  if KeyState[VK_NUMLOCK] = 0 then
  begin
    Keybd_Event(VK_NUMLOCK, 1, KEYEVENTF_EXTENDEDKEY or 0, 0) ;
    Keybd_Event(VK_NUMLOCK, 1, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0) ;
  end
  else
  begin
    Keybd_Event(VK_NUMLOCK, 0, KEYEVENTF_EXTENDEDKEY or 0, 0) ;
    Keybd_Event(VK_NUMLOCK, 0, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0) ;
  end;
  {$ENDIF}
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
  RedrawDisplay(Sender);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
    case key of
      VK_C, VK_CLEAR, VK_BACK, VK_DELETE: CButtonClick(Sender);
      VK_DOWN: RDButtonClick(Sender);
    end;
  IndicateNumLockState(Sender);
  IndicateErrorState(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
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
    NumLockIndicator.Hint := kNumLockOn;
  end
  else
  begin
    NumLockIndicator.Brush.Color := clRed;
    NumLockIndicator.Hint := kNumLockOff;
  end;
  NumLockLabel.Hint := NumLockIndicator.Hint;
  {$ENDIF}
end;

procedure TMainForm.IndicateErrorState(Sender: TObject);
begin
  if ErrorState then
    ErrorIndicator.Brush.Color := clRed
  else
    ErrorIndicator.Brush.Color := clInactiveIndicator;
end;

procedure TMainForm.Nr0ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('0');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.ZeroSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  ZeroSpeedButton.Down := True;
  Nr0ButtonClick(Sender);
  ZeroSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.ZeroSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  ZeroSpeedButton.Images := StandardButtons;
  ZeroSpeedButton.Down := False;
end;

procedure TMainForm.Nr1ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('1');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.OneSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  OneSpeedButton.Down := True;
  Nr1ButtonClick(Sender);
  OneSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.OneSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  OneSpeedButton.Images := StandardButtons;
  OneSpeedButton.Down := False;
end;

procedure TMainForm.Panel0MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ZeroSpeedButton.Down := True;
  Nr0ButtonClick(Sender);
  ZeroSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel0MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ZeroSpeedButton.Images := StandardButtons;
  ZeroSpeedButton.Down := False;
end;

procedure TMainForm.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  OneSpeedButton.Down := True;
  Nr1ButtonClick(Sender);
  OneSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  OneSpeedButton.Images := StandardButtons;
  OneSpeedButton.Down := False;
end;

procedure TMainForm.Panel2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  TwoSpeedButton.Down := True;
  Nr2ButtonClick(Sender);
  TwoSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  TwoSpeedButton.Images := StandardButtons;
  TwoSpeedButton.Down := False;
end;

procedure TMainForm.Panel3MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ThreeSpeedButton.Down := True;
  Nr3ButtonClick(Sender);
  ThreeSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ThreeSpeedButton.Images := StandardButtons;
  ThreeSpeedButton.Down := False;
end;

procedure TMainForm.Panel4MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FourSpeedButton.Down := True;
  Nr4ButtonClick(Sender);
  FourSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel4MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FourSpeedButton.Images := StandardButtons;
  FourSpeedButton.Down := False;
end;

procedure TMainForm.Panel5MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FiveSpeedButton.Down := True;
  Nr5ButtonClick(Sender);
  FiveSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel5MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FiveSpeedButton.Images := StandardButtons;
  FiveSpeedButton.Down := False;
end;

procedure TMainForm.Panel6MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SixSpeedButton.Down := True;
  Nr6ButtonClick(Sender);
  SixSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel6MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SixSpeedButton.Images := StandardButtons;
  SixSpeedButton.Down := False;
end;

procedure TMainForm.Panel7MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SevenSpeedButton.Down := True;
  SevenSpeedButton.Images := PressedButtons;
  Nr7ButtonClick(Sender);
end;

procedure TMainForm.Panel7MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SevenSpeedButton.Images := StandardButtons;
  SevenSpeedButton.Down := False;
end;

procedure TMainForm.Panel8MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  EightSpeedButton.Down := True;
  EightSpeedButton.Images := PressedButtons;
  Nr8ButtonClick(Sender);
end;

procedure TMainForm.Panel8MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  EightSpeedButton.Images := StandardButtons;
  EightSpeedButton.Down := False;
end;

procedure TMainForm.Panel9MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  NineSpeedButton.Down := True;
  NineSpeedButton.Images := PressedButtons;
  Nr9ButtonClick(Sender);
end;

procedure TMainForm.Panel9MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  NineSpeedButton.Images := StandardButtons;
  NineSpeedButton.Down := False;
end;

procedure TMainForm.PanelCMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  CSpeedButton.Down := True;
  CButtonClick(Sender);
  CSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelCMouseLeave(Sender: TObject);
begin
  CSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelCMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  CSpeedButton.Images := StandardButtons;
  CSpeedButton.Down := False;
end;

procedure TMainForm.PanelDivMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DivSpeedButton.Down := True;
  DivButtonClick(Sender);
  DivSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelDivMouseLeave(Sender: TObject);
begin
  DivSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelDivMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DivSpeedButton.Images := StandardButtons;
  DivSpeedButton.Down := False;
end;

procedure TMainForm.PanelDotMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DotSpeedButton.Down := True;
  DotButtonClick(Sender);
  DotSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelDotMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DotSpeedButton.Images := StandardButtons;
  DotSpeedButton.Down := False;
end;

procedure TMainForm.PanelEnterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  EnterSpeedButton.Down := True;
  EnterButtonClick(Sender);
  EnterSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelEnterMouseLeave(Sender: TObject);
begin
  EnterSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelEnterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  EnterSpeedButton.Images := StandardButtons;
  EnterSpeedButton.Down := False;
end;

procedure TMainForm.PanelInvMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  InvSpeedButton.Down := True;
  InvButtonClick(Sender);
  InvSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelInvMouseLeave(Sender: TObject);
begin
  InvSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelInvMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  InvSpeedButton.Images := StandardButtons;
  InvSpeedButton.Down := False;
end;

procedure TMainForm.PanelMinusMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  MinusSpeedButton.Down := True;
  MinusButtonClick(Sender);
  MinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelMinusMouseLeave(Sender: TObject);
begin
  MinusSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelMinusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  MinusSpeedButton.Images := StandardButtons;
  MinusSpeedButton.Down := False;
end;

procedure TMainForm.PanelPlusMinusMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  PlusMinusSpeedButton.Down := True;
  PlusMinusButtonClick(Sender);
  PlusMinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelPlusMinusMouseLeave(Sender: TObject);
begin
  PlusMinusSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelPlusMinusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PlusMinusSpeedButton.Images := StandardButtons;
  PlusMinusSpeedButton.Down := False;
end;

procedure TMainForm.PanelPlusMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PlusSpeedButton.Down := True;
  PlusButtonClick(Sender);
  PlusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelPlusMouseLeave(Sender: TObject);
begin
  PlusSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelPlusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PlusSpeedButton.Images := StandardButtons;
  PlusSpeedButton.Down := False;
end;

procedure TMainForm.PanelPwrMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PwrSpeedButton.Down := True;
  PwrButtonClick(Sender);
  PwrSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelPwrMouseLeave(Sender: TObject);
begin
  PwrSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelPwrMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PwrSpeedButton.Images := StandardButtons;
  PwrSpeedButton.Down := False;
end;

procedure TMainForm.PanelRDMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  RollDownSpeedButton.Down := True;
  RDButtonClick(Sender);
  RollDownSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelRDMouseLeave(Sender: TObject);
begin
  RollDownSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelRDMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  RollDownSpeedButton.Images := StandardButtons;
  RollDownSpeedButton.Down := False;
end;

procedure TMainForm.PanelSqrMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SqrSpeedButton.Down := True;
  SqrButtonClick(Sender);
  SqrSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelSqrMouseLeave(Sender: TObject);
begin
  SqrSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelSqrMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SqrSpeedButton.Images := StandardButtons;
  SqrSpeedButton.Down := False;
end;

procedure TMainForm.PanelSqrtMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SqrtSpeedButton.Down := True;
  SqrtButtonClick(Sender);
  SqrtSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelSqrtMouseLeave(Sender: TObject);
begin
  SqrtSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelSqrtMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SqrtSpeedButton.Images := StandardButtons;
  SqrtSpeedButton.Down := False;
end;

procedure TMainForm.PanelTimesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  TimesSpeedButton.Down := True;
  TimesButtonClick(Sender);
  TimesSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelTimesMouseLeave(Sender: TObject);
begin
  TimesSpeedButton.Enabled := True;
end;

procedure TMainForm.PanelTimesMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  TimesSpeedButton.Images := StandardButtons;
  TimesSpeedButton.Down := False;
end;

procedure TMainForm.Nr2ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('2');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.TwoSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  TwoSpeedButton.Down := True;
  Nr2ButtonClick(Sender);
  TwoSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TwoSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  TwoSpeedButton.Images := StandardButtons;
  TwoSpeedButton.Down := False;
end;

procedure TMainForm.Nr3ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('3');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.ThreeSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  ThreeSpeedButton.Down := True;
  Nr3ButtonClick(Sender);
  ThreeSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.ThreeSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  ThreeSpeedButton.Images := StandardButtons;
  ThreeSpeedButton.Down := False;
end;

procedure TMainForm.Timer0StartTimer(Sender: TObject);
begin
  ZeroSpeedButton.Down := True;
  ZeroSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer0Timer(Sender: TObject);
begin
  Timer0.Enabled := False;
  ZeroSpeedButton.Images := StandardButtons;
  ZeroSpeedButton.Down := False;
end;

procedure TMainForm.Timer1StartTimer(Sender: TObject);
begin
  OneSpeedButton.Down := True;
  OneSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  OneSpeedButton.Images := StandardButtons;
  OneSpeedButton.Down := False;
end;

procedure TMainForm.Timer2StartTimer(Sender: TObject);
begin
  TwoSpeedButton.Down := True;
  TwoSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer2Timer(Sender: TObject);
begin
  Timer2.Enabled := False;
  TwoSpeedButton.Images := StandardButtons;
  TwoSpeedButton.Down := False;
end;

procedure TMainForm.Timer3StartTimer(Sender: TObject);
begin
  ThreeSpeedButton.Down := True;
  ThreeSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer3Timer(Sender: TObject);
begin
  Timer3.Enabled := False;
  ThreeSpeedButton.Images := StandardButtons;
  ThreeSpeedButton.Down := False;
end;

procedure TMainForm.Timer4StartTimer(Sender: TObject);
begin
  FourSpeedButton.Down := True;
  FourSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer4Timer(Sender: TObject);
begin
  Timer4.Enabled := False;
  FourSpeedButton.Images := StandardButtons;
  FourSpeedButton.Down := False;
end;

procedure TMainForm.Timer5StartTimer(Sender: TObject);
begin
  FiveSpeedButton.Down := True;
  FiveSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer5Timer(Sender: TObject);
begin
  Timer5.Enabled := False;
  FiveSpeedButton.Images := StandardButtons;
  FiveSpeedButton.Down := False;
end;

procedure TMainForm.Timer6StartTimer(Sender: TObject);
begin
  SixSpeedButton.Down := True;
  SixSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer6Timer(Sender: TObject);
begin
  Timer6.Enabled := False;
  SixSpeedButton.Images := StandardButtons;
  SixSpeedButton.Down := False;
end;

procedure TMainForm.Timer7StartTimer(Sender: TObject);
begin
  SevenSpeedButton.Down := True;
  SevenSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer7Timer(Sender: TObject);
begin
  Timer7.Enabled := False;
  SevenSpeedButton.Images := StandardButtons;
  SevenSpeedButton.Down := False;
end;

procedure TMainForm.Timer8StartTimer(Sender: TObject);
begin
  EightSpeedButton.Down := True;
  EightSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer8Timer(Sender: TObject);
begin
  Timer8.Enabled := False;
  EightSpeedButton.Images := StandardButtons;
  EightSpeedButton.Down := False;
end;

procedure TMainForm.Timer9StartTimer(Sender: TObject);
begin
  NineSpeedButton.Down := True;
  NineSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer9Timer(Sender: TObject);
begin
  Timer9.Enabled := False;
  NineSpeedButton.Images := StandardButtons;
  NineSpeedButton.Down := False;
end;

procedure TMainForm.TimerClearStartTimer(Sender: TObject);
begin
  CSpeedButton.Down := True;
  CSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerClearTimer(Sender: TObject);
begin
  TimerClear.Enabled := False;
  CSpeedButton.Images := StandardButtons;
  CSpeedButton.Down := False;
end;

procedure TMainForm.TimerDivStartTimer(Sender: TObject);
begin
  DivSpeedButton.Down := True;
  DivSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerDivTimer(Sender: TObject);
begin
  TimerDiv.Enabled := False;
  DivSpeedButton.Images := StandardButtons;
  DivSpeedButton.Down := False;
end;

procedure TMainForm.TimerDotStartTimer(Sender: TObject);
begin
  DotSpeedButton.Down := True;
  DotSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerDotTimer(Sender: TObject);
begin
  TimerDot.Enabled := False;
  DotSpeedButton.Images := StandardButtons;
  DotSpeedButton.Down := False;
end;

procedure TMainForm.TimerEnterStartTimer(Sender: TObject);
begin
  EnterSpeedButton.Down := True;
  EnterSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerEnterTimer(Sender: TObject);
begin
  TimerEnter.Enabled := False;
  EnterSpeedButton.Images := StandardButtons;
  EnterSpeedButton.Down := False;
end;

procedure TMainForm.TimerMinusStartTimer(Sender: TObject);
begin
  MinusSpeedButton.Down := True;
  MinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerMinusTimer(Sender: TObject);
begin
  TimerMinus.Enabled := False;
  MinusSpeedButton.Images := StandardButtons;
  MinusSpeedButton.Down := False;
end;

procedure TMainForm.TimerPlusMinusStartTimer(Sender: TObject);
begin
  PlusMinusSpeedButton.Down := True;
  PlusMinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerPlusMinusTimer(Sender: TObject);
begin
  TimerPlusMinus.Enabled := False;
  PlusMinusSpeedButton.Images := StandardButtons;
  PlusMinusSpeedButton.Down := False;
end;

procedure TMainForm.TimerPlusStartTimer(Sender: TObject);
begin
  PlusSpeedButton.Down := True;
  PlusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerPlusTimer(Sender: TObject);
begin
  TimerPlus.Enabled := False;
  PlusSpeedButton.Images := StandardButtons;
  PlusSpeedButton.Down := False;
end;

procedure TMainForm.TimerTimesStartTimer(Sender: TObject);
begin
  TimesSpeedButton.Down := True;
  TimesSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerTimesTimer(Sender: TObject);
begin
  TimerTimes.Enabled := False;
  TimesSpeedButton.Images := StandardButtons;
  TimesSpeedButton.Down := False;
end;

procedure TMainForm.Nr4ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('4');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.FourSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FourSpeedButton.Down := True;
  Nr4ButtonClick(Sender);
  FourSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.FourSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FourSpeedButton.Images := StandardButtons;
  FourSpeedButton.Down := False;
end;

procedure TMainForm.Image1Click(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

procedure TMainForm.Nr5ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('5');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.FiveSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FiveSpeedButton.Down := True;
  Nr5ButtonClick(Sender);
  FiveSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.FiveSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FiveSpeedButton.Images := StandardButtons;
  FiveSpeedButton.Down := False;
end;

procedure TMainForm.Nr6ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('6');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.SixSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  SixSpeedButton.Down := True;
  Nr6ButtonClick(Sender);
  SixSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.SixSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SixSpeedButton.Images := StandardButtons;
  SixSpeedButton.Down := False;
end;

procedure TMainForm.SqrButtonClick(Sender: TObject);
begin
  Frame.HandleSqr;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.SqrSpeedButtonMouseEnter(Sender: TObject);
begin
  SqrSpeedButton.Enabled := False;
end;

procedure TMainForm.SqrtSpeedButtonMouseEnter(Sender: TObject);
begin
  SqrtSpeedButton.Enabled := False;
end;

procedure TMainForm.Nr7ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('7');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.SevenSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  SevenSpeedButton.Down := True;
  Nr7ButtonClick(Sender);
  SevenSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.SevenSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  SevenSpeedButton.Images := StandardButtons;
  SevenSpeedButton.Down := False;
end;

procedure TMainForm.Shape2Click(Sender: TObject);
begin

end;

procedure TMainForm.Nr8ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('8');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.EightSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  EightSpeedButton.Down := True;
  Nr8ButtonClick(Sender);
  EightSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.EightSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  EightSpeedButton.Images := StandardButtons;
  EightSpeedButton.Down := False;
end;

procedure TMainForm.Nr9ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('9');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.NineSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  NineSpeedButton.Down := True;
  Nr9ButtonClick(Sender);
  NineSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.NineSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  NineSpeedButton.Images := StandardButtons;
  NineSpeedButton.Down := False;
end;

procedure TMainForm.NumLockIndicatorClick(Sender: TObject);
begin
  NumlockLabelClick(Sender);
end;

procedure TMainForm.DotButtonClick(Sender: TObject);
begin
  Frame.AppendChar('.');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.DotSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  DotSpeedButton.Down := True;
  DotButtonClick(Sender);
  DotSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.DotSpeedButtonMouseEnter(Sender: TObject);
begin

end;

procedure TMainForm.DotSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DotSpeedButton.Images := StandardButtons;
  DotSpeedButton.Down := False;
end;

procedure TMainForm.EnterButtonClick(Sender: TObject);
begin
  TimerEnter.Enabled := True;
  Frame.HandleEnter;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.EnterSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  EnterSpeedButton.Down := True;
  EnterButtonClick(Sender);
  EnterSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.EnterSpeedButtonMouseEnter(Sender: TObject);
begin
  EnterSpeedButton.Enabled := False;
end;

procedure TMainForm.EnterSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  EnterSpeedButton.Images := StandardButtons;
  EnterSpeedButton.Down := False;
end;

procedure TMainForm.InvButtonClick(Sender: TObject);
begin
  Frame.HandleInv;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.InvSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  InvSpeedButton.Down := True;
  InvButtonClick(Sender);
  InvSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.InvSpeedButtonMouseEnter(Sender: TObject);
begin
  InvSpeedButton.Enabled := False;
end;

procedure TMainForm.InvSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  InvSpeedButton.Images := StandardButtons;
  InvSpeedButton.Down := False;
end;

procedure TMainForm.CButtonClick(Sender: TObject);
begin
  Frame.HandleClear;
  RedrawDisplay(Sender);
  CSpeedButton.Enabled := True;
end;

procedure TMainForm.CSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  CSpeedButton.Down := True;
  CButtonClick(Sender);
  CSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.CSpeedButtonMouseEnter(Sender: TObject);
begin
  CSpeedButton.Enabled := False;
end;

procedure TMainForm.CSpeedButtonMouseLeave(Sender: TObject);
begin
  CSpeedButton.Enabled := True;
end;

procedure TMainForm.CSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  CSpeedButton.Images := StandardButtons;
  CSpeedButton.Down := False;
end;

procedure TMainForm.PlusButtonClick(Sender: TObject);
begin
  Frame.HandleAdd;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.PlusSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  PlusSpeedButton.Down := True;
  PlusButtonClick(Sender);
  PlusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PlusSpeedButtonMouseEnter(Sender: TObject);
begin
  PlusSpeedButton.Enabled := False;
  ;
end;

procedure TMainForm.PlusSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  PlusSpeedButton.Images := StandardButtons;
  PlusSpeedButton.Down := False;
end;

procedure TMainForm.PwrSpeedButtonMouseEnter(Sender: TObject);
begin
  PwrSpeedButton.Enabled := False;
end;

procedure TMainForm.MinusButtonClick(Sender: TObject);
begin
  Frame.HandleSub;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.MinusSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  MinusSpeedButton.Down := True;
  MinusButtonClick(Sender);
  MinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.MinusSpeedButtonMouseEnter(Sender: TObject);
begin
  MinusSpeedButton.Enabled := False;
end;

procedure TMainForm.MinusSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  MinusSpeedButton.Images := StandardButtons;
  MinusSpeedButton.Down := False;
end;

procedure TMainForm.TimesButtonClick(Sender: TObject);
begin
  Frame.HandleTimes;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.TimesSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  TimesSpeedButton.Down := True;
  TimesButtonClick(Sender);
  TimesSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimesSpeedButtonMouseEnter(Sender: TObject);
begin
  TimesSpeedButton.Enabled := False;
end;

procedure TMainForm.TimesSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  TimesSpeedButton.Images := StandardButtons;
  TimesSpeedButton.Down := False;
end;

procedure TMainForm.XRegisterPaintBoxPaint(Sender: TObject);
begin
  XRegisterPaintBox.Canvas.Brush.Style := bsSolid;
  XRegisterPaintBox.Canvas.Brush.Color := DisplayBackgroundPanel.Brush.Color;
  XRegisterPaintBox.Canvas.FillRect(0, 0, XRegisterPaintBox.Width,
    XRegisterPaintBox.Height);
  DisplayX.Style := [fsItalic];
  DisplayX.n := Engine.Stack.x;
end;

procedure TMainForm.YRegisterPaintBoxPaint(Sender: TObject);
begin
  YRegisterPaintBox.Canvas.Brush.Style := bsSolid;
  YRegisterPaintBox.Canvas.Brush.Color := DisplayBackgroundPanel.Brush.Color;
  YRegisterPaintBox.Canvas.FillRect(0, 0, YRegisterPaintBox.Width,
    YRegisterPaintBox.Height);
  DisplayY.Style := [fsItalic];
  DisplayY.n := Engine.Stack.y;
end;

procedure TMainForm.ZRegisterPaintBoxPaint(Sender: TObject);
begin
  ZRegisterPaintBox.Canvas.Brush.Style := bsSolid;
  ZRegisterPaintBox.Canvas.Brush.Color := DisplayBackgroundPanel.Brush.Color;
  ZRegisterPaintBox.Canvas.FillRect(0, 0, ZRegisterPaintBox.Width,
    ZRegisterPaintBox.Height);
  DisplayZ.Style := [fsItalic];
  DisplayZ.n := Engine.Stack.z;
end;

procedure TMainForm.TRegisterPaintBoxPaint(Sender: TObject);
begin
  TRegisterPaintBox.Canvas.Brush.Style := bsSolid;
  TRegisterPaintBox.Canvas.Brush.Color := DisplayBackgroundPanel.Brush.Color;
  TRegisterPaintBox.Canvas.FillRect(0, 0, TRegisterPaintBox.Width,
    TRegisterPaintBox.Height);
  DisplayT.Style := [fsItalic];
  DisplayT.n := Engine.Stack.t;
end;

procedure TMainForm.RedrawDisplay(Sender: TObject);
begin
  if IsInfinite(engine.Stack.x) then
    ErrorState := True
  else
    ErrorState := False;
  XRegisterPaintBox.Invalidate;
  YRegisterPaintBox.Invalidate;
  ZRegisterPaintBox.Invalidate;
  TRegisterPaintBox.Invalidate;
  IndicateErrorState(Sender);
end;

procedure TMainForm.DivButtonClick(Sender: TObject);
begin
  Frame.HandleDiv;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.DivSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  DivSpeedButton.Down := True;
  DivButtonClick(Sender);
  DivSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.DivSpeedButtonMouseEnter(Sender: TObject);
begin
  DivSpeedButton.Enabled := False;
end;

procedure TMainForm.DivSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  DivSpeedButton.Images := StandardButtons;
  DivSpeedButton.Down := False;
end;

procedure TMainForm.PlusMinusButtonClick(Sender: TObject);
begin
  Frame.HandleCHS;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.PlusMinusSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  PlusMinusSpeedButton.Down := True;
  PlusMinusButtonClick(Sender);
  PlusMinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PlusMinusSpeedButtonMouseEnter(Sender: TObject);
begin
  PlusMinusSpeedButton.Enabled := False;
end;


procedure TMainForm.PlusMinusSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  PlusMinusSpeedButton.Images := StandardButtons;
  PlusMinusSpeedButton.Down := False;
end;

procedure TMainForm.PwrButtonClick(Sender: TObject);
begin
  Frame.HandlePWR;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.RDButtonClick(Sender: TObject);
begin
  Frame.HandleRollDown;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.RollDownSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  RollDownSpeedButton.Down := True;
  RDButtonClick(Sender);
  RollDownSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.RollDownSpeedButtonMouseEnter(Sender: TObject);
begin
  RollDownSpeedButton.Enabled := False;
end;

procedure TMainForm.RollDownSpeedButtonMouseLeave(Sender: TObject);
begin
  RollDownSpeedButton.Enabled := True;
end;

procedure TMainForm.RollDownSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  RollDownSpeedButton.Images := StandardButtons;
  RollDownSpeedButton.Down := False;
end;

procedure TMainForm.SinButtonClick(Sender: TObject);
begin
  Frame.HandleSin;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.ASinButtonClick(Sender: TObject);
begin
  Frame.HandleASin;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.CosButtonClick(Sender: TObject);
begin
  Frame.HandleCos;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.ACosButtonClick(Sender: TObject);
begin
  Frame.HandleACos;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.TanButtonClick(Sender: TObject);
begin
  Frame.HandleTan;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.ATanButtonClick(Sender: TObject);
begin
  Frame.HandleATan;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.SqrtButtonClick(Sender: TObject);
begin
  Frame.HandleSqrt;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

end.
