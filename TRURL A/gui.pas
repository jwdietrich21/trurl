unit GUI;

{ Trurl A }

{ Simple RPN calculator in Object Pascal }

{ GUI }

{ Version 1.0 (Leopolis) }

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
  LCLType, Menus, ActnList, StdActns, ExtCtrls, Clipbrd,
  RPNEngine, aboutbox;

type

  TEntryMode = (PostOper, PostEnter, Number);

  { TMainForm }

  TMainForm = class(TForm)
    ActionList1: TActionList;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditPaste1: TEditPaste;
    EditUndo1: TEditUndo;
    DisplayBackgroundPanel: TPanel;
    XRegisterDisplay: TLabel;
    YRegisterDisplay: TLabel;
    ZRegisterDisplay: TLabel;
    TRegisterDisplay: TLabel;
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
    PwrButton: TButton;
    InvButton: TButton;
    PlusMinusButton: TButton;
    SqrtButton: TButton;
    SinButton: TButton;
    ASinButton: TButton;
    CosButton: TButton;
    ACosButton: TButton;
    TanButton: TButton;
    ATanButton: TButton;
    procedure AdaptMenus;
    procedure ACosButtonClick(Sender: TObject);
    procedure ASinButtonClick(Sender: TObject);
    procedure ATanButtonClick(Sender: TObject);
    procedure CButtonClick(Sender: TObject);
    procedure CosButtonClick(Sender: TObject);
    procedure DivButtonClick(Sender: TObject);
    procedure DotButtonClick(Sender: TObject);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EnterButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure InvButtonClick(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
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
  private

  public
    Engine: TEngine;
    EntryMode: TEntryMode;
    procedure DisplayRegisters;
    procedure AppendChar(ch: char);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AdaptMenus;
  Engine := TEngine.create;
  EntryMode := Number;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Engine.Destroy;
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
    {$IFDEF LCLcarbon}
    modifierKey := [ssMeta];
    WinAboutItem.Visible := False;
    AppleMenu.Visible := True;
    {$ELSE}
    {$IFDEF LCLCocoa}
    modifierKey := [ssMeta];
    WinAboutItem.Visible := False;
    AppleMenu.Visible := True;
    {$ELSE}
    modifierKey := [ssCtrl];
    WinAboutItem.Visible := True;
    AppleMenu.Visible := False;
    {$ENDIF}
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
  Clipboard.AsText := XRegisterDisplay.Caption;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    VK_0, VK_NUMPAD0: Nr0ButtonClick(Sender);
    VK_1, VK_NUMPAD1: Nr1ButtonClick(Sender);
    VK_2, VK_NUMPAD2: Nr2ButtonClick(Sender);
    VK_3, VK_NUMPAD3: Nr3ButtonClick(Sender);
    VK_4, VK_NUMPAD4: Nr4ButtonClick(Sender);
    VK_5, VK_NUMPAD5: Nr5ButtonClick(Sender);
    VK_6, VK_NUMPAD6: Nr6ButtonClick(Sender);
    VK_7, VK_NUMPAD7: Nr7ButtonClick(Sender);
    VK_8, VK_NUMPAD8: Nr8ButtonClick(Sender);
    VK_9, VK_NUMPAD9: Nr9ButtonClick(Sender);
    VK_DECIMAL, VK_LCL_POINT, VK_OEM_COMMA: DotButtonClick(Sender);
    VK_ADD, VK_OEM_PLUS: PlusButtonClick(Sender);
    VK_SUBTRACT, VK_LCL_MINUS: MinusButtonClick(Sender);
    VK_DIVIDE: DivButtonClick(Sender);
    VK_MULTIPLY: TimesButtonClick(Sender);
    VK_C, VK_CLEAR, VK_BACK, VK_DELETE: CButtonClick(Sender);
    VK_DOWN: RDButtonClick(Sender);
  end;
end;

procedure TMainForm.DotButtonClick(Sender: TObject);
begin
  AppendChar('.');
end;

procedure TMainForm.EnterButtonClick(Sender: TObject);
begin
  Engine.Stack.RollUp;
  EntryMode := PostEnter;
  DisplayRegisters;
end;

procedure TMainForm.InvButtonClick(Sender: TObject);
begin
  Engine.Inv;
  DisplayRegisters;
end;

procedure TMainForm.MinusButtonClick(Sender: TObject);
begin
  Engine.Sub;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.CButtonClick(Sender: TObject);
begin
  Engine.Stack.x := 0;
  EntryMode := PostEnter;
  DisplayRegisters;
end;

procedure TMainForm.ASinButtonClick(Sender: TObject);
begin
  Engine.ArcSinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.ATanButtonClick(Sender: TObject);
begin
  Engine.ArcTangens;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.ACosButtonClick(Sender: TObject);
begin
  Engine.ArcCosinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.CosButtonClick(Sender: TObject);
begin
  Engine.Cosinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.DivButtonClick(Sender: TObject);
begin
  Engine.Divide;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.Nr0ButtonClick(Sender: TObject);
begin
  AppendChar('0');
end;

procedure TMainForm.Nr1ButtonClick(Sender: TObject);
begin
  AppendChar('1');
end;

procedure TMainForm.Nr2ButtonClick(Sender: TObject);
begin
  AppendChar('2');
end;

procedure TMainForm.Nr3ButtonClick(Sender: TObject);
begin
  AppendChar('3');
end;

procedure TMainForm.Nr4ButtonClick(Sender: TObject);
begin
  AppendChar('4');
end;

procedure TMainForm.Nr5ButtonClick(Sender: TObject);
begin
  AppendChar('5');
end;

procedure TMainForm.Nr6ButtonClick(Sender: TObject);
begin
  AppendChar('6');
end;

procedure TMainForm.Nr7ButtonClick(Sender: TObject);
begin
  AppendChar('7');
end;

procedure TMainForm.Nr8ButtonClick(Sender: TObject);
begin
  AppendChar('8');
end;

procedure TMainForm.Nr9ButtonClick(Sender: TObject);
begin
  AppendChar('9');
end;

procedure TMainForm.PlusButtonClick(Sender: TObject);
begin
  Engine.Add;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.PlusMinusButtonClick(Sender: TObject);
begin
  Engine.CHS;
  DisplayRegisters;
end;

procedure TMainForm.PwrButtonClick(Sender: TObject);
begin
  Engine.PWR;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.RDButtonClick(Sender: TObject);
begin
  Engine.Stack.RollDown;
  DisplayRegisters;
end;

procedure TMainForm.SinButtonClick(Sender: TObject);
begin
  Engine.Sinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.SqrtButtonClick(Sender: TObject);
begin
  Engine.sqroot;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.TanButtonClick(Sender: TObject);
begin
  Engine.Tangens;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.TimesButtonClick(Sender: TObject);
begin
  Engine.Times;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TMainForm.DisplayRegisters;
var
  theFormat: TFormatSettings;
begin
  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := '.';
  TRegisterDisplay.Caption := FloatToStr(Engine.Stack.t, theFormat);
  ZRegisterDisplay.Caption := FloatToStr(Engine.Stack.z, theFormat);
  YRegisterDisplay.Caption := FloatToStr(Engine.Stack.y, theFormat);
  XRegisterDisplay.Caption := FloatToStr(Engine.Stack.x, theFormat);
end;

procedure TMainForm.AppendChar(ch: char);
var
  theFormat: TFormatSettings;
begin
  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := '.';
  case EntryMode of
    PostOper:
    begin
      Engine.Stack.RollUp;
      DisplayRegisters;
      XRegisterDisplay.Caption := ch;
      EntryMode := Number;
    end;
    PostEnter:
    begin
      XRegisterDisplay.Caption := ch;
      EntryMode := Number;
    end;
    Number:
    begin
      if XRegisterDisplay.Caption = '0' then
      begin
        if ch = '.' then
          XRegisterDisplay.Caption := XRegisterDisplay.Caption + ch
        else
          XRegisterDisplay.Caption := ch;
      end
      else
      if (ch <> '.') or (pos('.', XRegisterDisplay.Caption) = 0) then
        XRegisterDisplay.Caption := XRegisterDisplay.Caption + ch;
    end;
  end;
  Engine.Stack.x := StrToFloat(XRegisterDisplay.Caption, theFormat);
end;

end.

