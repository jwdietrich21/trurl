unit AboutBox;

{ Trurl A }

{ Simple RPN calculator in Object Pascal }

{ About Box }

{ Version 1.0 (Leopolis) }

{ (c) Johannes W. Dietrich, 2003 - 2018 }

{ Source code released under the BSD License }

{ See the file "license.txt", included in this distribution, }
{ for details about the copyright. }
{ Current versions and additional information are available from }
{ http://puma-repository.sf.net }

{ This program is distributed in the hope that it will be useful, }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TTrurlAboutBox }

  TTrurlAboutBox = class(TForm)
    AboutLabel1: TLabel;
    AboutLabel2: TLabel;
    Image1: TImage;
  private

  public

  end;

var
  TrurlAboutBox: TTrurlAboutBox;

implementation

{$R *.lfm}


end.

