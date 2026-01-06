unit AboutBox;

{ Trurl B }

{ RPN calculator in Object Pascal, inspired by Braun calculators }

{ About Box }

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  RPNEngine;

type

  { TTrurlAboutBox }

  TTrurlAboutBox = class(TForm)
    AboutLabel1: TLabel;
    AboutLabel2: TLabel;
    Image1: TImage;
    EngineVersionLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private

  public

  end;

var
  TrurlAboutBox: TTrurlAboutBox;

implementation

{$R *.lfm}

{ TTrurlAboutBox }

procedure TTrurlAboutBox.FormCreate(Sender: TObject);
begin
  EngineVersionLabel.Caption := 'Based on TRURL RPN Engine ' + IntToStr(RPNEngine_major) + '.'
    + IntToStr(RPNEngine_minor) + '.' + IntToStr(RPNEngine_release) + '.'
    + IntToStr(RPNEngine_patch);
end;

procedure TTrurlAboutBox.Image1Click(Sender: TObject);
begin

end;

end.

