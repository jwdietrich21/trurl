unit RPNWidgets;

{ Trurl }

{ A suite of RPN calculators in Object Pascal }

{ GUI Widgets for Basic RPN Engine }

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
  Classes, SysUtils, Controls, RPNEngine;

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
end;

implementation

{ TFrame }

constructor TFrame.create;
begin
  inherited create;
end;

destructor TFrame.destroy;
begin
  inherited destroy;
end;

procedure TFrame.HandleEnter;
begin
  Engine.Stack.RollUp;
  EntryMode := PostEnter;
  DisplayRegisters;
end;

procedure TFrame.HandleClear;
begin
  Engine.Stack.x := 0;
  EntryMode := PostEnter;
  DisplayRegisters;
end;

procedure TFrame.HandleInv;
begin
  Engine.Inv;
  DisplayRegisters;
end;

procedure TFrame.HandleAdd;
begin
  Engine.Add;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleSub;
begin
  Engine.Sub;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleTimes;
begin
  Engine.Times;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleDiv;
begin
  Engine.Divide;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleCHS;
begin
  Engine.CHS;
  DisplayRegisters;
end;

procedure TFrame.HandlePWR;
begin
  Engine.PWR;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleSin;
begin
  Engine.Sinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleCos;
begin
  Engine.Cosinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleTan;
begin
  Engine.Tangens;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleASin;
begin
  Engine.ArcSinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleACos;
begin
  Engine.ArcCosinus;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleATan;
begin
  Engine.ArcTangens;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleSqrt;
begin
  Engine.sqroot;
  DisplayRegisters;
  EntryMode := PostOper;
end;

procedure TFrame.HandleRollDown;
begin
  Engine.Stack.RollDown;
  DisplayRegisters;
end;

procedure TFrame.DisplayRegisters;
{ Displays register contents in controls that are provided by the GUI }
var
  theFormat: TFormatSettings;
begin
  theFormat := DefaultFormatSettings;
  theFormat.DecimalSeparator := '.';
  TRegDisplay.Caption := FloatToStr(Engine.Stack.t, theFormat);
  ZRegDisplay.Caption := FloatToStr(Engine.Stack.z, theFormat);
  YRegDisplay.Caption := FloatToStr(Engine.Stack.y, theFormat);
  XRegDisplay.Caption := FloatToStr(Engine.Stack.x, theFormat);
end;

procedure TFrame.AppendChar(ch: char);
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
      XRegDisplay.Caption := ch;
      EntryMode := Number;
    end;
    PostEnter:
    begin
      XRegDisplay.Caption := ch;
      EntryMode := Number;
    end;
    Number:
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
  Engine.Stack.x := StrToFloat(XRegDisplay.Caption, theFormat);
end;

end.

