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

{ TFrame }

TFrame = class
private
public
  Engine: TEngine;
  TRegDisplay, ZRegDisplay, YRegDisplay, XRegDisplay: TControl;
  constructor create;
  destructor destroy; override;
  procedure DisplayRegisters;
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

procedure TFrame.DisplayRegisters;
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

end.

