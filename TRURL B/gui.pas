unit GUI;

{ Trurl B }

{ RPN calculator in Object Pascal, inspired by Braun calculators }

{ GUI }

{ Version 1.0.0 () }

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
    VirtualEnterButton: TButton;
    PressedButtons: TImageList;
    StandardButtons: TImageList;
    NumlockLabel1: TLabel;
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
      Shift: TShiftState; X, Y: Integer);
    procedure CSpeedButtonMouseEnter(Sender: TObject);
    procedure CSpeedButtonMouseLeave(Sender: TObject);
    procedure CSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DivButtonClick(Sender: TObject);
    procedure DivSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DivSpeedButtonMouseEnter(Sender: TObject);
    procedure DivSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DotButtonClick(Sender: TObject);
    procedure DotSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DotSpeedButtonMouseEnter(Sender: TObject);
    procedure DotSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditCut1Execute(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure EightSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EightSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EnterButtonClick(Sender: TObject);
    procedure EnterSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EnterSpeedButtonMouseEnter(Sender: TObject);
    procedure EnterSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FiveSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FiveSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FourSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FourSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure IndicateNumLockState(Sender: TObject);
    procedure IndicateErrorState(Sender: TObject);
    procedure InvButtonClick(Sender: TObject);
    procedure InvSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure InvSpeedButtonMouseEnter(Sender: TObject);
    procedure InvSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MacAboutItemClick(Sender: TObject);
    procedure MinusSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MinusSpeedButtonMouseEnter(Sender: TObject);
    procedure MinusSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NineSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NineSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NumlockLabel1Click(Sender: TObject);
    procedure NumlockLabel2Click(Sender: TObject);
    procedure OneSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OneSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel0MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel0MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel4MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel5MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel5MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel6MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel7MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel7MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel8MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel8MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel9MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel9MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelCMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelCMouseLeave(Sender: TObject);
    procedure PanelCMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelDivMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelDivMouseLeave(Sender: TObject);
    procedure PanelDivMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelDotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelDotMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelEnterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelEnterMouseLeave(Sender: TObject);
    procedure PanelEnterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelInvMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelInvMouseLeave(Sender: TObject);
    procedure PanelInvMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelMinusMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelMinusMouseLeave(Sender: TObject);
    procedure PanelMinusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPlusMinusMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPlusMinusMouseLeave(Sender: TObject);
    procedure PanelPlusMinusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPlusMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPlusMouseLeave(Sender: TObject);
    procedure PanelPlusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPwrMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPwrMouseLeave(Sender: TObject);
    procedure PanelPwrMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelRDMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelRDMouseLeave(Sender: TObject);
    procedure PanelRDMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelSqrMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelSqrMouseLeave(Sender: TObject);
    procedure PanelSqrMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelSqrtMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelSqrtMouseLeave(Sender: TObject);
    procedure PanelSqrtMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelTimesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelTimesMouseLeave(Sender: TObject);
    procedure PanelTimesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PlusMinusSpeedButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PlusMinusSpeedButtonMouseEnter(Sender: TObject);
    procedure PlusMinusSpeedButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PlusSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PlusSpeedButtonMouseEnter(Sender: TObject);
    procedure PlusSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PwrSpeedButtonMouseEnter(Sender: TObject);
    procedure RollDownSpeedButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RollDownSpeedButtonMouseEnter(Sender: TObject);
    procedure RollDownSpeedButtonMouseLeave(Sender: TObject);
    procedure RollDownSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SevenSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SevenSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Shape2Click(Sender: TObject);
    procedure SixSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SixSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SqrButtonClick(Sender: TObject);
    procedure SqrSpeedButtonMouseEnter(Sender: TObject);
    procedure SqrtSpeedButtonMouseEnter(Sender: TObject);
    procedure ThreeSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ThreeSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    procedure TimerPlusStartTimer(Sender: TObject);
    procedure TimerPlusTimer(Sender: TObject);
    procedure TimerTimesStartTimer(Sender: TObject);
    procedure TimerTimesTimer(Sender: TObject);
    procedure TimesSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimesSpeedButtonMouseEnter(Sender: TObject);
    procedure TimesSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TRegisterPaintBoxPaint(Sender: TObject);
    procedure TwoSpeedButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TwoSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
      Shift: TShiftState; X, Y: Integer);
    procedure ZeroSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
  Frame.ReplaceXAfterRollDown := true;
  DisplayX := TDisplay.create;
  DisplayX.l := 11;
  DisplayX.Canvas := XRegisterPaintBox.Canvas;
  DisplayX.Color := clAqua;
  DisplayX.scale := 1;
  DisplayX.decScale := 4;
  DisplayX.Style := [];
  DisplayX.offsetX := 9;
  DisplayX.offsetY := 9;
  DisplayY := TDisplay.create;
  DisplayY.l := 11;
  DisplayY.Canvas := YRegisterPaintBox.Canvas;
  DisplayY.Color := clAqua;
  DisplayY.scale := 1;
  DisplayY.decScale := 4;
  DisplayY.Style := [];
  DisplayY.offsetX := 9;
  DisplayY.offsetY := 9;
  DisplayZ := TDisplay.create;
  DisplayZ.l := 11;
  DisplayZ.Canvas := ZRegisterPaintBox.Canvas;
  DisplayZ.Color := clAqua;
  DisplayZ.scale := 1;
  DisplayZ.decScale := 4;
  DisplayZ.Style := [];
  DisplayZ.offsetX := 9;
  DisplayZ.offsetY := 9;
  DisplayT := TDisplay.create;
  DisplayT.l := 11;
  DisplayT.Canvas := TRegisterPaintBox.Canvas;
  DisplayT.Color := clAqua;
  DisplayT.scale := 1;
  DisplayT.decScale := 4;
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
    '0':
      begin
        Timer0.Enabled := true;
        Nr0ButtonClick(Sender);
      end;
    '1':
      begin
        Timer1.Enabled := true;
        Nr1ButtonClick(Sender);
      end;
    '2':
      begin
        Timer2.Enabled := true;
        Nr2ButtonClick(Sender);
      end;
    '3':
      begin
        Timer3.Enabled := true;
        Nr3ButtonClick(Sender);
      end;
    '4':
      begin
        Timer4.Enabled := true;
        Nr4ButtonClick(Sender);
      end;
    '5':
      begin
        Timer5.Enabled := true;
        Nr5ButtonClick(Sender);
      end;
    '6':
      begin
        Timer6.Enabled := true;
        Nr6ButtonClick(Sender);
      end;
    '7':
      begin
        Timer7.Enabled := true;
        Nr7ButtonClick(Sender);
      end;
    '8':
      begin
        Timer8.Enabled := true;
        Nr8ButtonClick(Sender);
      end;
    '9':
      begin
        Timer9.Enabled := true;
        Nr9ButtonClick(Sender);
      end;
    '.', ',':
      begin
        TimerDot.Enabled := true;
        DotButtonClick(Sender);
      end;
    '+':
      begin
        TimerPlus.Enabled := true;
        PlusButtonClick(Sender);
      end;
    '-':
      begin
        TimerMinus.Enabled := true;
        MinusButtonClick(Sender);
      end;
    '/':
      begin
        TimerDiv.Enabled := true;
        DivButtonClick(Sender);
      end;
    '*':
      begin
        TimerTimes.Enabled := true;
        TimesButtonClick(Sender);
      end;
    'c', 'C':
      begin
        TimerClear.Enabled := true;
        CButtonClick(Sender);
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

procedure TMainForm.NumlockLabel1Click(Sender: TObject);
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

procedure TMainForm.NumlockLabel2Click(Sender: TObject);
begin
  NumlockLabel1Click(Sender);
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
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ZeroSpeedButton.Down := true;
  Nr0ButtonClick(Sender);
  ZeroSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.ZeroSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ZeroSpeedButton.Images := StandardButtons;
  ZeroSpeedButton.Down := false;
end;

procedure TMainForm.Nr1ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('1');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.OneSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OneSpeedButton.Down := true;
  Nr1ButtonClick(Sender);
  OneSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.OneSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OneSpeedButton.Images := StandardButtons;
  OneSpeedButton.Down := false;
end;

procedure TMainForm.Panel0MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ZeroSpeedButton.Down := true;
  Nr0ButtonClick(Sender);
  ZeroSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel0MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ZeroSpeedButton.Images := StandardButtons;
  ZeroSpeedButton.Down := false;
end;

procedure TMainForm.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  OneSpeedButton.Down := true;
  Nr1ButtonClick(Sender);
  OneSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  OneSpeedButton.Images := StandardButtons;
  OneSpeedButton.Down := false;
end;

procedure TMainForm.Panel2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TwoSpeedButton.Down := true;
  Nr2ButtonClick(Sender);
  TwoSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TwoSpeedButton.Images := StandardButtons;
  TwoSpeedButton.Down := false;
end;

procedure TMainForm.Panel3MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ThreeSpeedButton.Down := true;
  Nr3ButtonClick(Sender);
  ThreeSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ThreeSpeedButton.Images := StandardButtons;
  ThreeSpeedButton.Down := false;
end;

procedure TMainForm.Panel4MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FourSpeedButton.Down := true;
  Nr4ButtonClick(Sender);
  FourSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel4MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FourSpeedButton.Images := StandardButtons;
  FourSpeedButton.Down := false;
end;

procedure TMainForm.Panel5MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FiveSpeedButton.Down := true;
  Nr5ButtonClick(Sender);
  FiveSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel5MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FiveSpeedButton.Images := StandardButtons;
  FiveSpeedButton.Down := false;
end;

procedure TMainForm.Panel6MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SixSpeedButton.Down := true;
  Nr6ButtonClick(Sender);
  SixSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Panel6MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SixSpeedButton.Images := StandardButtons;
  SixSpeedButton.Down := false;
end;

procedure TMainForm.Panel7MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SevenSpeedButton.Down := true;
  SevenSpeedButton.Images := PressedButtons;
  Nr7ButtonClick(Sender);
end;

procedure TMainForm.Panel7MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SevenSpeedButton.Images := StandardButtons;
  SevenSpeedButton.Down := false;
end;

procedure TMainForm.Panel8MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EightSpeedButton.Down := true;
  EightSpeedButton.Images := PressedButtons;
  Nr8ButtonClick(Sender);
end;

procedure TMainForm.Panel8MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EightSpeedButton.Images := StandardButtons;
  EightSpeedButton.Down := false;
end;

procedure TMainForm.Panel9MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  NineSpeedButton.Down := true;
  NineSpeedButton.Images := PressedButtons;
  Nr9ButtonClick(Sender);
end;

procedure TMainForm.Panel9MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  NineSpeedButton.Images := StandardButtons;
  NineSpeedButton.Down := false;
end;

procedure TMainForm.PanelCMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CSpeedButton.Down := true;
  CButtonClick(Sender);
  CSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelCMouseLeave(Sender: TObject);
begin
  CSpeedButton.enabled := true;
end;

procedure TMainForm.PanelCMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CSpeedButton.Images := StandardButtons;
  CSpeedButton.Down := false;
end;

procedure TMainForm.PanelDivMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DivSpeedButton.Down := true;
  DivButtonClick(Sender);
  DivSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelDivMouseLeave(Sender: TObject);
begin
  DivSpeedButton.enabled := true;
end;

procedure TMainForm.PanelDivMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DivSpeedButton.Images := StandardButtons;
  DivSpeedButton.Down := false;
end;

procedure TMainForm.PanelDotMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DotSpeedButton.Down := true;
  DotButtonClick(Sender);
  DotSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelDotMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DotSpeedButton.Images := StandardButtons;
  DotSpeedButton.Down := false;
end;

procedure TMainForm.PanelEnterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EnterSpeedButton.Down := true;
  EnterButtonClick(Sender);
  EnterSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelEnterMouseLeave(Sender: TObject);
begin
  EnterSpeedButton.Enabled := true;
end;

procedure TMainForm.PanelEnterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EnterSpeedButton.Images := StandardButtons;
  EnterSpeedButton.Down := false;
end;

procedure TMainForm.PanelInvMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  InvSpeedButton.Down := true;
  InvButtonClick(Sender);
  InvSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelInvMouseLeave(Sender: TObject);
begin
  InvSpeedButton.Enabled := true;
end;

procedure TMainForm.PanelInvMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  InvSpeedButton.Images := StandardButtons;
  InvSpeedButton.Down := false;
end;

procedure TMainForm.PanelMinusMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MinusSpeedButton.Down := true;
  MinusButtonClick(Sender);
  MinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelMinusMouseLeave(Sender: TObject);
begin
  MinusSpeedButton.Enabled := true;
end;

procedure TMainForm.PanelMinusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MinusSpeedButton.Images := StandardButtons;
  MinusSpeedButton.Down := false;
end;

procedure TMainForm.PanelPlusMinusMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PlusMinusSpeedButton.Down := true;
  PlusMinusButtonClick(Sender);
  PlusMinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelPlusMinusMouseLeave(Sender: TObject);
begin
  PlusMinusSpeedButton.Enabled := true;
end;

procedure TMainForm.PanelPlusMinusMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PlusMinusSpeedButton.Images := StandardButtons;
  PlusMinusSpeedButton.Down := false;
end;

procedure TMainForm.PanelPlusMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PlusSpeedButton.Down := true;
  PlusButtonClick(Sender);
  PlusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelPlusMouseLeave(Sender: TObject);
begin
  PlusSpeedButton.Enabled := true;
end;

procedure TMainForm.PanelPlusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PlusSpeedButton.Images := StandardButtons;
  PlusSpeedButton.Down := false;
end;

procedure TMainForm.PanelPwrMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PwrSpeedButton.Down := true;
  PwrButtonClick(Sender);
  PwrSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelPwrMouseLeave(Sender: TObject);
begin
  PwrSpeedButton.Enabled := true;
end;

procedure TMainForm.PanelPwrMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PwrSpeedButton.Images := StandardButtons;
  PwrSpeedButton.Down := false;
end;

procedure TMainForm.PanelRDMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  RollDownSpeedButton.Down := true;
  RDButtonClick(Sender);
  RollDownSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelRDMouseLeave(Sender: TObject);
begin
  RollDownSpeedButton.Enabled := true;
end;

procedure TMainForm.PanelRDMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  RollDownSpeedButton.Images := StandardButtons;
  RollDownSpeedButton.Down := false;
end;

procedure TMainForm.PanelSqrMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SqrSpeedButton.Down := true;
  SqrButtonClick(Sender);
  SqrSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelSqrMouseLeave(Sender: TObject);
begin
  SqrSpeedButton.Enabled := true;
end;

procedure TMainForm.PanelSqrMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SqrSpeedButton.Images := StandardButtons;
  SqrSpeedButton.Down := false;
end;

procedure TMainForm.PanelSqrtMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SqrtSpeedButton.Down := true;
  SqrtButtonClick(Sender);
  SqrtSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelSqrtMouseLeave(Sender: TObject);
begin
  SqrtSpeedButton.Enabled := true;
end;

procedure TMainForm.PanelSqrtMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SqrtSpeedButton.Images := StandardButtons;
  SqrtSpeedButton.Down := false;
end;

procedure TMainForm.PanelTimesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TimesSpeedButton.Down := true;
  TimesButtonClick(Sender);
  TimesSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PanelTimesMouseLeave(Sender: TObject);
begin
  TimesSpeedButton.Enabled := true;
end;

procedure TMainForm.PanelTimesMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TimesSpeedButton.Images := StandardButtons;
  TimesSpeedButton.Down := false;
end;

procedure TMainForm.Nr2ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('2');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.TwoSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TwoSpeedButton.Down := true;
  Nr2ButtonClick(Sender);
  TwoSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TwoSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TwoSpeedButton.Images := StandardButtons;
  TwoSpeedButton.Down := false;
end;

procedure TMainForm.Nr3ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('3');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.ThreeSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ThreeSpeedButton.Down := true;
  Nr3ButtonClick(Sender);
  ThreeSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.ThreeSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ThreeSpeedButton.Images := StandardButtons;
  ThreeSpeedButton.Down := false;
end;

procedure TMainForm.Timer0StartTimer(Sender: TObject);
begin
  ZeroSpeedButton.Down := true;
  ZeroSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer0Timer(Sender: TObject);
begin
  Timer0.Enabled := false;
  ZeroSpeedButton.Images := StandardButtons;
  ZeroSpeedButton.Down := false;
end;

procedure TMainForm.Timer1StartTimer(Sender: TObject);
begin
  OneSpeedButton.Down := true;
  OneSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  OneSpeedButton.Images := StandardButtons;
  OneSpeedButton.Down := false;
end;

procedure TMainForm.Timer2StartTimer(Sender: TObject);
begin
  TwoSpeedButton.Down := true;
  TwoSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer2Timer(Sender: TObject);
begin
  Timer2.Enabled := false;
  TwoSpeedButton.Images := StandardButtons;
  TwoSpeedButton.Down := false;
end;

procedure TMainForm.Timer3StartTimer(Sender: TObject);
begin
  ThreeSpeedButton.Down := true;
  ThreeSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer3Timer(Sender: TObject);
begin
  Timer3.Enabled := false;
  ThreeSpeedButton.Images := StandardButtons;
  ThreeSpeedButton.Down := false;
end;

procedure TMainForm.Timer4StartTimer(Sender: TObject);
begin
  FourSpeedButton.Down := true;
  FourSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer4Timer(Sender: TObject);
begin
  Timer4.Enabled := false;
  FourSpeedButton.Images := StandardButtons;
  FourSpeedButton.Down := false;
end;

procedure TMainForm.Timer5StartTimer(Sender: TObject);
begin
  FiveSpeedButton.Down := true;
  FiveSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer5Timer(Sender: TObject);
begin
  Timer5.Enabled := false;
  FiveSpeedButton.Images := StandardButtons;
  FiveSpeedButton.Down := false;
end;

procedure TMainForm.Timer6StartTimer(Sender: TObject);
begin
  SixSpeedButton.Down := true;
  SixSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer6Timer(Sender: TObject);
begin
  Timer6.Enabled := false;
  SixSpeedButton.Images := StandardButtons;
  SixSpeedButton.Down := false;
end;

procedure TMainForm.Timer7StartTimer(Sender: TObject);
begin
  SevenSpeedButton.Down := true;
  SevenSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer7Timer(Sender: TObject);
begin
  Timer7.Enabled := false;
  SevenSpeedButton.Images := StandardButtons;
  SevenSpeedButton.Down := false;
end;

procedure TMainForm.Timer8StartTimer(Sender: TObject);
begin
  EightSpeedButton.Down := true;
  EightSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer8Timer(Sender: TObject);
begin
  Timer8.Enabled := false;
  EightSpeedButton.Images := StandardButtons;
  EightSpeedButton.Down := false;
end;

procedure TMainForm.Timer9StartTimer(Sender: TObject);
begin
  NineSpeedButton.Down := true;
  NineSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.Timer9Timer(Sender: TObject);
begin
  Timer9.Enabled := false;
  NineSpeedButton.Images := StandardButtons;
  NineSpeedButton.Down := false;
end;

procedure TMainForm.TimerClearStartTimer(Sender: TObject);
begin
  CSpeedButton.Down := true;
  CSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerClearTimer(Sender: TObject);
begin
  TimerClear.Enabled := false;
  CSpeedButton.Images := StandardButtons;
  CSpeedButton.Down := false;
end;

procedure TMainForm.TimerDivStartTimer(Sender: TObject);
begin
  DivSpeedButton.Down := true;
  DivSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerDivTimer(Sender: TObject);
begin
  TimerDiv.Enabled := false;
  DivSpeedButton.Images := StandardButtons;
  DivSpeedButton.Down := false;
end;

procedure TMainForm.TimerDotStartTimer(Sender: TObject);
begin
  DotSpeedButton.Down := true;
  DotSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerDotTimer(Sender: TObject);
begin
  TimerDot.Enabled := false;
  DotSpeedButton.Images := StandardButtons;
  DotSpeedButton.Down := false;
end;

procedure TMainForm.TimerEnterStartTimer(Sender: TObject);
begin
  EnterSpeedButton.Down := true;
  EnterSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerEnterTimer(Sender: TObject);
begin
  TimerEnter.Enabled := false;
  EnterSpeedButton.Images := StandardButtons;
  EnterSpeedButton.Down := false;
end;

procedure TMainForm.TimerMinusStartTimer(Sender: TObject);
begin
  MinusSpeedButton.Down := true;
  MinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerMinusTimer(Sender: TObject);
begin
  TimerMinus.Enabled := false;
  MinusSpeedButton.Images := StandardButtons;
  MinusSpeedButton.Down := false;
end;

procedure TMainForm.TimerPlusStartTimer(Sender: TObject);
begin
  PlusSpeedButton.Down := true;
  PlusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerPlusTimer(Sender: TObject);
begin
  TimerPlus.Enabled := false;
  PlusSpeedButton.Images := StandardButtons;
  PlusSpeedButton.Down := false;
end;

procedure TMainForm.TimerTimesStartTimer(Sender: TObject);
begin
  TimesSpeedButton.Down := true;
  TimesSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimerTimesTimer(Sender: TObject);
begin
  TimerTimes.Enabled := false;
  TimesSpeedButton.Images := StandardButtons;
  TimesSpeedButton.Down := false;
end;

procedure TMainForm.Nr4ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('4');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.FourSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FourSpeedButton.Down := true;
  Nr4ButtonClick(Sender);
  FourSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.FourSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FourSpeedButton.Images := StandardButtons;
  FourSpeedButton.Down := false;
end;

procedure TMainForm.Nr5ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('5');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.FiveSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FiveSpeedButton.Down := true;
  Nr5ButtonClick(Sender);
  FiveSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.FiveSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FiveSpeedButton.Images := StandardButtons;
  FiveSpeedButton.Down := false;
end;

procedure TMainForm.Nr6ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('6');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.SixSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SixSpeedButton.Down := true;
  Nr6ButtonClick(Sender);
  SixSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.SixSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SixSpeedButton.Images := StandardButtons;
  SixSpeedButton.Down := false;
end;

procedure TMainForm.SqrButtonClick(Sender: TObject);
begin
  Frame.HandleSqr;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.SqrSpeedButtonMouseEnter(Sender: TObject);
begin
  SqrSpeedButton.Enabled := false;
end;

procedure TMainForm.SqrtSpeedButtonMouseEnter(Sender: TObject);
begin
  SqrtSpeedButton.Enabled := false;
end;

procedure TMainForm.Nr7ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('7');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.SevenSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SevenSpeedButton.Down := true;
  Nr7ButtonClick(Sender);
  SevenSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.SevenSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SevenSpeedButton.Images := StandardButtons;
  SevenSpeedButton.Down := false;
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
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EightSpeedButton.Down := true;
  Nr8ButtonClick(Sender);
  EightSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.EightSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EightSpeedButton.Images := StandardButtons;
  EightSpeedButton.Down := false;
end;

procedure TMainForm.Nr9ButtonClick(Sender: TObject);
begin
  Frame.AppendChar('9');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.NineSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  NineSpeedButton.Down := true;
  Nr9ButtonClick(Sender);
  NineSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.NineSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  NineSpeedButton.Images := StandardButtons;
  NineSpeedButton.Down := false;
end;

procedure TMainForm.DotButtonClick(Sender: TObject);
begin
  Frame.AppendChar('.');
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.DotSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DotSpeedButton.Down := true;
  DotButtonClick(Sender);
  DotSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.DotSpeedButtonMouseEnter(Sender: TObject);
begin

end;

procedure TMainForm.DotSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DotSpeedButton.Images := StandardButtons;
  DotSpeedButton.Down := false;
end;

procedure TMainForm.EnterButtonClick(Sender: TObject);
begin
  TimerEnter.Enabled := true;
  Frame.HandleEnter;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.EnterSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EnterSpeedButton.Down := true;
  EnterButtonClick(Sender);
  EnterSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.EnterSpeedButtonMouseEnter(Sender: TObject);
begin
  EnterSpeedButton.Enabled := false;
end;

procedure TMainForm.EnterSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EnterSpeedButton.Images := StandardButtons;
  EnterSpeedButton.Down := false;
end;

procedure TMainForm.InvButtonClick(Sender: TObject);
begin
  Frame.HandleInv;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.InvSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  InvSpeedButton.Down := true;
  InvButtonClick(Sender);
  InvSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.InvSpeedButtonMouseEnter(Sender: TObject);
begin
  InvSpeedButton.Enabled := false;
end;

procedure TMainForm.InvSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  InvSpeedButton.Images := StandardButtons;
  InvSpeedButton.Down := false;
end;

procedure TMainForm.CButtonClick(Sender: TObject);
begin
  Frame.HandleClear;
  RedrawDisplay(Sender);
  CSpeedButton.enabled := true;
end;

procedure TMainForm.CSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CSpeedButton.Down := true;
  CButtonClick(Sender);
  CSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.CSpeedButtonMouseEnter(Sender: TObject);
begin
  CSpeedButton.enabled := false;
end;

procedure TMainForm.CSpeedButtonMouseLeave(Sender: TObject);
begin
  CSpeedButton.enabled := true;
end;

procedure TMainForm.CSpeedButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CSpeedButton.Images := StandardButtons;
  CSpeedButton.Down := false;
end;

procedure TMainForm.PlusButtonClick(Sender: TObject);
begin
  Frame.HandleAdd;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.PlusSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PlusSpeedButton.Down := true;
  PlusButtonClick(Sender);
  PlusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PlusSpeedButtonMouseEnter(Sender: TObject);
begin
  PlusSpeedButton.Enabled := false;;
end;

procedure TMainForm.PlusSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PlusSpeedButton.Images := StandardButtons;
  PlusSpeedButton.Down := false;
end;

procedure TMainForm.PwrSpeedButtonMouseEnter(Sender: TObject);
begin
  PwrSpeedButton.Enabled := false;
end;

procedure TMainForm.MinusButtonClick(Sender: TObject);
begin
  Frame.HandleSub;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.MinusSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MinusSpeedButton.Down := true;
  MinusButtonClick(Sender);
  MinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.MinusSpeedButtonMouseEnter(Sender: TObject);
begin
  MinusSpeedButton.Enabled := false;
end;

procedure TMainForm.MinusSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MinusSpeedButton.Images := StandardButtons;
  MinusSpeedButton.Down := false;
end;

procedure TMainForm.TimesButtonClick(Sender: TObject);
begin
  Frame.HandleTimes;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.TimesSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimesSpeedButton.Down := true;
  TimesButtonClick(Sender);
  TimesSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.TimesSpeedButtonMouseEnter(Sender: TObject);
begin
  TimesSpeedButton.Enabled := false;
end;

procedure TMainForm.TimesSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TimesSpeedButton.Images := StandardButtons;
  TimesSpeedButton.Down := false;
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
  if IsInfinite(engine.Stack.x) then
    ErrorState := true
  else
    ErrorState := false;
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
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DivSpeedButton.Down := true;
  DivButtonClick(Sender);
  DivSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.DivSpeedButtonMouseEnter(Sender: TObject);
begin
  DivSpeedButton.Enabled := false;
end;

procedure TMainForm.DivSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DivSpeedButton.Images := StandardButtons;
  DivSpeedButton.Down := false;
end;

procedure TMainForm.PlusMinusButtonClick(Sender: TObject);
begin
  Frame.HandleCHS;
  RedrawDisplay(Sender);
  ActiveControl := VirtualEnterButton;
end;

procedure TMainForm.PlusMinusSpeedButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PlusMinusSpeedButton.Down := true;
  PlusMinusButtonClick(Sender);
  PlusMinusSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.PlusMinusSpeedButtonMouseEnter(Sender: TObject);
begin
  PlusMinusSpeedButton.Enabled := false;
end;


procedure TMainForm.PlusMinusSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PlusMinusSpeedButton.Images := StandardButtons;
  PlusMinusSpeedButton.Down := false;
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
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  RollDownSpeedButton.Down := true;
  RDButtonClick(Sender);
  RollDownSpeedButton.Images := PressedButtons;
end;

procedure TMainForm.RollDownSpeedButtonMouseEnter(Sender: TObject);
begin
  RollDownSpeedButton.Enabled := false;
end;

procedure TMainForm.RollDownSpeedButtonMouseLeave(Sender: TObject);
begin
  RollDownSpeedButton.Enabled := true
end;

procedure TMainForm.RollDownSpeedButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  RollDownSpeedButton.Images := StandardButtons;
  RollDownSpeedButton.Down := false;
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


