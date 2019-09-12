unit aboutw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, ShellApi, LazUTF8;

type

  { TFabout }

  TFabout = class(TForm)
    BBclose: TBitBtn;
    BBpotwierdzenia: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    procedure BBpotwierdzeniaClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure Execute;
  end;

var
  Fabout: TFabout;

implementation

uses mainwindow;

{$R *.lfm}

{ TFabout }

procedure TFabout.FormCreate(Sender: TObject);
begin

  Fabout.Position:= poMainFormCenter;
  Fabout.Caption:= cMainCaption + ' v'+ Mwindow.sAppFileVer + ' - dmbsoft.pl';
  BBpotwierdzenia.Caption:= 'Folder potwierdzeń...';
  Label1.Caption:='Program do sprawdzenia kontrahenta' +
    LineEnding + 'na BIAŁEJ LIŚCIE podmiotów VAT.' +
    LineEnding + 'Potwierdzenie weryfikacji (podatnik czynny i ma konto),'+
    ' zapisywane jest w folderze: '+Mwindow.sFolderPotw+
    LineEnding + 'Można także wyszukać w historii weryfikacji, po przełączeniu na'+
    ' "Szukaj w potwierdzeniach".'+
    LineEnding + LineEnding + 'Sprawdź na www.dmbsoft.pl/bialymis' +
    LineEnding + LineEnding + 'dMb soft Damian Brzozowski' + LineEnding +
    LineEnding + 'Ikony icons8.com';
end;

procedure TFabout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

  CloseAction:=caHide;
end;

procedure TFabout.BBpotwierdzeniaClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(UTF8ToWinCP(Mwindow.sFolderPotw)), nil , nil,1);
end;

procedure TFabout.Execute;
begin

  Fabout.ShowModal;
end;

end.

