unit RPNWidgets;

{ Trurl }

{ A suite of RPN calculators in Object Pascal }

{ GUI Widgets for Basic RPN Engine }

{ Version 2.0 (Bet) }

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
  Classes, SysUtils, Controls, RPNEngine, Math;

type

TEntryMode = (PostOper, PostEnter, Number);

{ TFrame }

TFrame = class
private
public
  Engine: TEngine;
  TRegDisplay, ZRegDisplay, YRegDisplay, XRegDisplay: TControl;
  EntryMode: TEntryMode;
  constructor create;
  destructor destroy; override;
  procedure HandleEnter;
  procedure HandleClear;
  procedure HandleInv;
  procedure HandleAdd;
  procedure HandleSub;
  procedure HandleTimes;
  procedure HandleDiv;
  procedure HandleCHS;
  procedure HandlePWR;
  procedure HandleSin;
  procedure HandleCos;
  procedure HandleTan;
  procedure HandleASin;
  procedure HandleACos;
  procedure HandleATan;
  procedure HandleSqrt;
  procedure HandleRollDown;
  procedure DisplayRegisters;
  procedure AppendChar(ch: char);
  procedure InsertString(theString: String);
  procedure CheckEngine;
  procedure Error(msg: String);
end;

implementation

{ TFrame }

constructor TFrame.create;
begin
  inherited create;
  if RPNEngine_major <> 2 then
    {%H-}Error('RPN Engine version mismatch');
end;

destructor TFrame.destroy;
begin
  inherited destroy;
end;

procedure TFrame.HandleEnter;
begin
  CheckEngine;
  Engine.Stack.RollUp;
  EntryMode := PostEnter;
  DisplayRegisters;
end;

procedure TFrame.HandleClear;
begin
  CheckEngine;
  Engine.Stack.x := 0;
  EntryMode := PostEnter;
  DisplayRegisters;
end;

procedure TFrame.HandleInv;
begin
  CheckEngine;
  Engine.Inv;
  Engine.Stack.RollUp;
  EntryMode := postEnter;
  DisplayRegisters;
end;

procedure TFrame.HandleAdd;
begin
  CheckEngine;
  Engine.Add;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleSub;
begin
  CheckEngine;
  Engine.Sub;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleTimes;
begin
  CheckEngine;
  Engine.Times;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleDiv;
begin
  CheckEngine;
  Engine.Divide;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleCHS;
{ Change sign (+/-) }
begin
  CheckEngine;
  Engine.CHS;
  if EntryMode = PostEnter then
    Engine.Stack.RollUp;
  DisplayRegisters;
end;

procedure TFrame.HandlePWR;
begin
  CheckEngine;
  Engine.PWR;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleSin;
begin
  CheckEngine;
  Engine.Sinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleCos;
begin
  CheckEngine;
  Engine.Cosinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleTan;
begin
  CheckEngine;
  Engine.Tangens;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleASin;
begin
  CheckEngine;
  Engine.ArcSinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleACos;
begin
  CheckEngine;
  Engine.ArcCosinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleATan;
begin
  CheckEngine;
  Engine.ArcTangens;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleSqrt;
begin
  CheckEngine;
  Engine.sqroot;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleRollDown;
begin
  CheckEngine;
  Engine.Stack.RollDown;
  DisplayRegisters;
end;

procedure TFrame.DisplayRegisters;
{ Displays register contents in controls that are provided by the GUI }
var
  theFormat: TFormatSettings;
begin
  CheckEngine;
  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := '.';
  if assigned(TRegDisplay) then
    TRegDisplay.Caption := FloatToStr(Engine.Stack.t, theFormat);
  if assigned(ZRegDisplay) then
    ZRegDisplay.Caption := FloatToStr(Engine.Stack.z, theFormat);
  if assigned(YRegDisplay) then
    YRegDisplay.Caption := FloatToStr(Engine.Stack.y, theFormat);
  if assigned(XRegDisplay) then
    XRegDisplay.Caption := FloatToStr(Engine.Stack.x, theFormat);
end;

procedure TFrame.AppendChar(ch: char);
var
  theFormat: TFormatSettings;
begin
  CheckEngine;
  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := '.';
  case EntryMode of
    PostOper:
    begin
      Engine.Stack.RollUp;
      DisplayRegisters;
      if assigned(XRegDisplay) then
        XRegDisplay.Caption := ch;
      EntryMode := Number;
    end;
    PostEnter:
    begin
      if assigned(XRegDisplay) then
        XRegDisplay.Caption := ch;
      EntryMode := Number;
    end;
    Number:
    begin
      if assigned(XRegDisplay) then
      begin
        if XRegDisplay.Caption = '0' then
        begin
          if ch = '.' then
            XRegDisplay.Caption := XRegDisplay.Caption + ch
          else
            XRegDisplay.Caption := ch;
        end
        else
        if (ch <> '.') or (pos('.', XRegDisplay.Caption) = 0) then
          XRegDisplay.Caption := XRegDisplay.Caption + ch;
      end;
    end;
  end;
  if assigned(XRegDisplay) then
    Engine.Stack.x := StrToFloat(XRegDisplay.Caption, theFormat);
end;

procedure TFrame.InsertString(theString: String);
var
  theFormat: TFormatSettings;
  theNumber: Extended;
begin
  CheckEngine;
  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := '.';
  Engine.Stack.RollUp;
  if TryStrToFloat(theString, theNumber, theFormat) then
    Engine.Stack.x := theNumber
  else
    Engine.Stack.x := NaN;
  EntryMode := PostEnter;
  DisplayRegisters;
end;

procedure TFrame.CheckEngine;
begin
  if not assigned(Engine) then
    Error('Engine not available');
end;

procedure TFrame.Error(msg: String);
begin
  raise exception.create(Msg) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;

end.

