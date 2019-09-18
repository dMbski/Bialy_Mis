unit mainwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, fphttpclient, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, EditBtn, ComCtrls, Menus, PrintersDlgs, Windows, HtmlView,
  Types, fpjson, Printers, fileinfo, winpeimagereader,
  jsonparser, HTMLUn2, HtmlGlobals, aboutw;

const
  //https://wl-api.mf.gov.pl/api/search/nip/2530153008?date=2019-09-03&_=1567524337973
  cApiUrlBaseS = 'https://wl-api.mf.gov.pl/api/search/';
  cApiUrlBaseC = 'https://wl-api.mf.gov.pl/api/check/';
  cApiSnip = '/nip/';
  cApiSregon = '/regon/';
  cApiSkonto = '/bank-account/';
  cApiSdate = '?date=';
  cColorWarning = $00C8C8FF;
  cColorResError = $008888FF;
  cColorResOK = $00C8FFC8;
  cColorResWarning = $00C8FFFF;
  cMainCaption = 'Biały Miś';
  cTBcaptionOL = 'Szukaj on-line';
  cTBcaptionL = 'Szukaj' + LineEnding + 'w potwierdzeniach';
  cAppPage = 'https://dmbsoft.pl/bialymis';

type

  { TMwindow }

  TMwindow = class(TForm)
    BBclear: TBitBtn;
    BBsearch: TBitBtn;
    DEdate: TDateEdit;
    Ekonto: TEdit;
    Enip: TEdit;
    Eregon: TEdit;
    GBsearchpar: TGroupBox;
    HVresult: THtmlViewer;
    httpClient: TFPHTTPClient;
    ImageList1: TImageList;
    Lkonto: TLabel;
    Lnip: TLabel;
    Lregon: TLabel;
    Lstannadzien: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MIaboutshow: TMenuItem;
    MIclose: TMenuItem;
    MIabout: TMenuItem;
    MIfile: TMenuItem;
    MIcopy: TMenuItem;
    MIcopyall: TMenuItem;
    MIsave: TMenuItem;
    MIprint: TMenuItem;
    MenuItem5: TMenuItem;
    Mresult: TMemo;
    PageControl1: TPageControl;
    PMenu1: TPopupMenu;
    PrintDialog1: TPrintDialog;
    SaveDialog1: TSaveDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TBlocally: TToggleBox;
    procedure BBclearClick(Sender: TObject);
    procedure BBsearchClick(Sender: TObject);
    procedure ChangeExitEdit(Sender: TObject);
    procedure EnipKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure HVresultHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: boolean);
    procedure HVresultKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MIaboutshowClick(Sender: TObject);
    procedure MIcloseClick(Sender: TObject);
    procedure MIcopyallClick(Sender: TObject);
    procedure MIcopyClick(Sender: TObject);
    procedure MIprintClick(Sender: TObject);
    procedure MIsaveClick(Sender: TObject);
    procedure TBlocallyChange(Sender: TObject);
  private

    sHtmlBuf: string;
    procedure clearWindow;
    procedure searchOnline;
    procedure searchLocally;
    function validateFields: boolean;
    function pathInJson(sJson: string; keypath: string): string;
    function findInJson(sJson: string; key: string): string;
    function simpleHtmlCells(cells: array of string): string;
    function makeTable(sJson: string): string;
  public
    sFolderPotw: string;
    sAppFileVer: string;
  end;

var
  Mwindow: TMwindow;

implementation

{$R *.lfm}

{ TMwindow }

procedure TMwindow.FormCreate(Sender: TObject);
var
  fileverinfo: TFileVersionInfo;
begin
  DefaultFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  DefaultFormatSettings.DateSeparator := '-';
  DefaultFormatSettings.DecimalSeparator := ',';
  DefaultFormatSettings.LongTimeFormat := 'hh.nn.ss';

  {reads file version from exe}
  try
    fileverinfo := TFileVersionInfo.Create(nil);
    fileverinfo.FileName:= Application.ExeName;
    fileverinfo.ReadFileInfo;
    //ShowMessage(fileverinfo.VersionStrings.Text);
    //fileversion -autoincrament
    //productversion- manualy
    sAppFileVer := fileverinfo.VersionStrings.Values['FileVersion'];
  finally
    fileverinfo.Free;
  end;

  Enip.MaxLength := 10; //min 10
  Eregon.MaxLength := 14; //min 9
  Ekonto.MaxLength := 26; //min 26

  Enip.NumbersOnly := True;
  Eregon.NumbersOnly := True;
  Ekonto.NumbersOnly := True;

  clearWindow;

  sFolderPotw := ExtractFilePath(Application.ExeName) + 'Potwierdzenia\';

  if not DirectoryExists(sFolderPotw) then
  begin
    if not CreateDir(sFolderPotw) then
    begin
      MessageDlg('Brak folderu', 'Brak folderu do zapisywania potwierdzeń' +
        LineEnding + 'Nie można utworzyć: ' + sFolderPotw +
        LineEnding + 'Potwierdzenia NIE będą zapisywane!',
        mtError, [mbClose], 0);
      sFolderPotw := '';
    end
    else
    begin
      MessageDlg('Potwierdzenia', 'Utworzono folder do zapisywania potwierdzeń.' +
        LineEnding + 'Poprawne będą zapisywane w:' + LineEnding + sFolderPotw,
        mtInformation, [mbClose], 0);
    end;
  end;
  TabSheet1.Caption := 'Wyszukanie: ';
  TabSheet2.Caption := 'Odpowiedź: ';
  Lnip.Caption := 'NIP';
  Lregon.Caption := 'REGON';
  Lkonto.Caption := 'Numer rachunku';
  Lstannadzien.Caption := 'Stan na dzień:';
  BBsearch.Caption := 'Szukaj';
  BBclear.Caption := 'Wyczyść';
  TBlocally.Caption := cTBcaptionOL;
  MIfile.Caption := 'Plik';
  MIabout.Caption := 'Pomoc';
  MIaboutshow.Caption := 'O programie';
  MIclose.Caption := 'Zamknij';
  MIcopy.Caption := 'Kopiuj';
  MIcopyall.Caption := 'Kopiuj wszystko';
  MIsave.Caption := 'Zapisz...';
  MIprint.Caption := 'Drukuj...';

  GBsearchpar.Caption := '';

end;

procedure TMwindow.HVresultHotSpotClick(Sender: TObject; const SRC: ThtString;
  var Handled: boolean);
var
  s: string;
begin
  if SRC = '#' then
  begin
    HVresult.LoadFromString(sHtmlBuf);
    Mresult.Clear;
    Exit;
  end;

  if FileExists(sFolderPotw + SRC) then
  begin
    sHtmlBuf := HVresult.Text;  //copy page to bufor
    Mresult.Lines.LoadFromFile(sFolderPotw + SRC);
    s := '<html><a href="#">...Lista</a><h3>Plik: <em>' + SRC + '</em></h3>';
    s := s + makeTable(Mresult.Lines.Text);
    s := s + '</html>';
    HVresult.Text := s;
  end;

end;

procedure TMwindow.HVresultKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = Ord('C')) then
    HVresult.CopyToClipboard
  else if (ssCtrl in Shift) and (Key = Ord('A')) then
    HVresult.SelectAll;
end;

procedure TMwindow.MIaboutshowClick(Sender: TObject);
begin
  Fabout.Execute;
end;

procedure TMwindow.MIcloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMwindow.MIcopyallClick(Sender: TObject);
begin
  HVresult.SelectAll;
  HVresult.CopyToClipboard;
  HVresult.SelLength := 0;
end;

procedure TMwindow.MIcopyClick(Sender: TObject);
begin
  HVresult.CopyToClipboard;
end;

procedure TMwindow.MIprintClick(Sender: TObject);
begin
  if PrintDialog1.Execute then
  begin
    HVresult.OpenPrint;
    HVresult.Print(1);
    HVresult.ClosePrint;
  end;
end;

procedure TMwindow.MIsaveClick(Sender: TObject);
var
  ssbuf: TStringList;
begin
  SaveDialog1.DefaultExt := '.htm';
  if SaveDialog1.Execute then
  begin
    ssbuf := TStringList.Create;
    ssbuf.Text := HVresult.Text;
    ssbuf.Append('<div>');
    ssbuf.Append('<h4>Pełna odpowiedź z serwera:</h4>');
    ssbuf.Append('<code>' + Mresult.Lines.Text + '</code>');
    ssbuf.Append('</div>');
    ssbuf.Append('<br>--------------------------');
    ssbuf.Append('<br><em>Utworzono w programie <b>' + cMainCaption + '</b> v'+
       sAppFileVer+'</em>');
    ssbuf.Append('<br><b>www.dmbsoft.pl</b>');

    ssbuf.SaveToFile(SaveDialog1.FileName);
    ssbuf.Free;
  end;

end;

procedure TMwindow.TBlocallyChange(Sender: TObject);
begin
  if TBlocally.Checked then
    TBlocally.Caption := cTBcaptionL
  else
    TBlocally.Caption := cTBcaptionOL;
end;

procedure TMwindow.clearWindow;
begin
  PageControl1.ActivePageIndex := 0;
  DEdate.Date := Date;
  Enip.Clear;
  Eregon.Clear;
  Ekonto.Clear;
  Mresult.Clear;
  HVresult.Clear;
  HVresult.Text := 'Brak danych.';
  Mwindow.Caption := cMainCaption;
  TBlocally.Checked := False;
end;

procedure TMwindow.searchOnline;
var
  strbuf, furl: string;
  ssbuf: TStringStream;
  s: string;
  fnpotw: string;
begin
  //validate fields
  if not validateFields then
  begin
    Mwindow.Color := cColorWarning;
    Mwindow.Repaint;
    Beep(120, 150);
    Mwindow.Color := clDefault;
    Exit;
  end;
  //if exist nip&konto or regon&konto mehod check
  //else method search
  Mresult.Clear;
  HVresult.Clear;
  strbuf := '';
  furl := '';

  { TODO : validate nip+konto to switch to CHECK method }
  if Length(Enip.Text) = 10 then
    furl := cApiUrlBaseS + cApiSnip + Enip.Text + cApiSdate + DEdate.Text
  else if (Length(Eregon.Text) = 9) or (Length(Eregon.Text) = 14) then
    furl := cApiUrlBaseS + cApiSregon + Eregon.Text + cApiSdate + DEdate.Text
  else if Length(Ekonto.Text) = 26 then
    furl := cApiUrlBaseS + cApiSkonto + Ekonto.Text + cApiSdate + DEdate.Text
  else
    furl := '';

  if furl = '' then
  begin
    MessageDlg('Popraw dane', mtError, [mbClose], 0);
    Exit;
  end;

  ssbuf := TStringStream.Create('');

  try
    httpClient.HTTPMethod('GET', furl, ssbuf, [200, 400]);
    strbuf := ssbuf.DataString;
  except
    PageControl1.ActivePageIndex := 1;
    Mresult.Lines.Text := 'Unknown Error ' + IntToStr(httpClient.ResponseStatusCode);
    Mresult.Append('---Response headers:');
    Mresult.Append(httpClient.ResponseHeaders.Text);
  end;

  ssbuf.Free;
  Mresult.Append(strbuf);

  s := '';

  if httpClient.ResponseStatusCode = 200 then
  begin
    //ok resposne
    //ShowMessage('test:' + findInJson(strbuf, 'accountNumbers'));

    if (findInJson(strbuf, 'accountNumbers').Length < 26) or
      (findInJson(strbuf, 'statusVat') <> 'Czynny') then
    begin
      Mwindow.Color := cColorResWarning;
      Mwindow.Caption := cMainCaption + ' - Potwierdzenie NIE zapisane';
      TabSheet1.ImageIndex := 11;
    end
    else
    begin
      Mwindow.Color := cColorResOK;
      TabSheet1.ImageIndex := 10;
      //write confirmation
      fnpotw := sFolderPotw + DateToStr(Date) + '_' + TimeToStr(Time) +
        '_na_' + DEdate.Text + '_' + findInJson(strbuf, 'requestId') + '.json';

      Mresult.Lines.SaveToFile(fnpotw);

      if FileExists(fnpotw) then
      begin
        Mwindow.Caption := cMainCaption + ' - Potwierdzenie zapisane ';
      end
      else
      begin
        Mwindow.Caption := cMainCaption + ' - BŁĄD podczas zapisu potwierdzenia!';
      end;
    end;
    PageControl1.ActivePageIndex := 0;
    s := '<html> Stan na dzień: <b>' + DEdate.Text + '</b>';

    s := s + makeTable(strbuf);

  end
  else if httpClient.ResponseStatusCode = 400 then
  begin
    //some error
    Mwindow.Color := cColorResError;
    PageControl1.ActivePageIndex := 0;
    s := '<html>Błąd walidacji: <i>' + pathInJson(strbuf, 'code') + '</i>';
    s := s + '<h3>' + pathInJson(strbuf, 'message') + '</h3>';

  end;
  //show beautiful response
  HVresult.Text := s;

end;

procedure TMwindow.searchLocally;
var
  what: string;
  srec: TSearchRec;
  slbuf: TStringList;
  s: string;
begin
  //validate fields
  if not validateFields then
  begin
    Mwindow.Color := cColorWarning;
    Mwindow.Repaint;
    Beep(120, 150);
    Mwindow.Color := clDefault;
    Exit;
  end;

  Mresult.Clear;
  HVresult.Clear;

  if (sFolderPotw = '') or (not DirectoryExists(sFolderPotw)) then
  begin
    MessageDlg('Brak folderu', 'Brak folderu potwierdzeń', mtError, [mbClose], 0);
  end;

  if Length(Enip.Text) = 10 then
    what := Enip.Text
  else if (Length(Eregon.Text) = 9) or (Length(Eregon.Text) = 14) then
    what := Eregon.Text
  else if Length(Ekonto.Text) = 26 then
    what := Ekonto.Text
  else
    what := '';

  if what = '' then
  begin
    MessageDlg('Popraw dane', mtError, [mbClose], 0);
    Exit;
  end;

  PageControl1.ActivePageIndex := 0;
  s := '';

  slbuf := TStringList.Create;

  if FindFirst(sFolderPotw + '*.json', faAnyFile, srec) = 0 then
  begin
    repeat
      if not ((srec.Attr and faDirectory) = faDirectory) then
      begin
        //ShowMessage('File:' + sFolderPotw + srec.Name);
        slbuf.LoadFromFile(sFolderPotw + srec.Name);
        if (Pos(what, slbuf.Text) > 0) then
        begin
          //ShowMessage('Found in File:' + sFolderPotw + srec.Name);
          s := s + '<tr>' + simpleHtmlCells(['<a href="' + srec.Name +
            '">' + srec.Name + '</a>', findInJson(slbuf.Text, 'name')]) + '</tr>';
        end;

      end;
    until FindNext(srec) <> 0;
    SysUtils.FindClose(srec);
  end;

  slbuf.Free;
  if s = '' then
  begin
    s := '<html>Brak wyników dla: <b>' + what + '</b></html>';
  end
  else
  begin
    s := '<html><h2>Wynik dla: <b>' + what + '</b></h2>' +
      '<table border="1" width="100%" cellspacing="0">' + s + '</table></html>';

  end;

  HVresult.Text := s;
end;

function TMwindow.validateFields: boolean;
begin
  Result := True;
  Mwindow.Caption := cMainCaption;
  TabSheet1.ImageIndex := 2;
  if (Length(Enip.Text) > 0) and (Length(Enip.Text) < 10) then
  begin
    Enip.Color := cColorWarning;
    Result := False;
  end
  else
    Enip.Color := clDefault;

  if ((Length(Eregon.Text) > 0) and (Length(Eregon.Text) < 9)) or
    ((Length(Eregon.Text) > 9) and (Length(Eregon.Text) < 14)) then
  begin
    Eregon.Color := cColorWarning;
    Result := False;
  end
  else
    Eregon.Color := clDefault;

  if (Length(Ekonto.Text) > 0) and (Length(Ekonto.Text) < 26) then
  begin
    Ekonto.Color := cColorWarning;
    Result := False;
  end
  else
    Ekonto.Color := clDefault;

  if (Enip.Text = '') and (Eregon.Text = '') and (Ekonto.Text = '') then
    Result := False;

  if Result then
  begin
    Beep(600, 50);
    Mwindow.Color := clDefault;
  end;
end;

function TMwindow.pathInJson(sJson: string; keypath: string): string;
var
  jData: TJSONData;
  jObject: TJSONData;
  s: string;
  i: integer;
begin
  Result := '';
  if (Length(sJson) < 2) or (keypath = '') then
    Exit;

  jData := GetJSON(sJson);
  if jData = nil then
  begin
    MessageDlg('BŁĄD wewnętrzny', 'Nieprawidłowy format json:' + LineEnding + sJson,
      mtError, [mbClose], 0);
    Exit;
  end;
  try
    s := '';
    jObject := jData.FindPath(keypath);
    if (jObject = nil) or (jObject.IsNull) then
      Exit;
    if (jObject.JSONType = jtArray) or (jObject.JSONType = jtObject) then
    begin

      for i := 0 to jObject.Count - 1 do
      begin
        if s <> '' then
          s := s + LineEnding;
        //ShowMessage('json:' + jObject.Items[i].AsJSON);
        if (jObject.Items[i].JSONType = jtArray) or
          (jObject.Items[i].JSONType = jtObject) then
          s := s + jObject.Items[i].AsJSON
        else
          s := s + jObject.Items[i].AsString;
      end;
    end
    else
    begin
      s := jObject.AsString;
    end;
  except
    s := '';
  end;
  //if jObject <> nil then jObject.Free;
  jData.Free;
  Result := s;
end;

function TMwindow.findInJson(sJson: string; key: string): string;
var
  jData: TJSONData;
  jObject: TJSONData;
  i: integer;
  s: string;
begin
  Result := '';
  if (Length(sJson) < 2) or (key = '') then
    Exit;

  jData := GetJSON(sJson);
  //ShowMessage('findjson, sjson:' + LineEnding + sJson);
  if jData = nil then
  begin
    MessageDlg('BŁĄD wewnętrzny', 'Nieprawidłowy format json:' + LineEnding + sJson,
      mtError, [mbClose], 0);
    Exit;
  end;
  try
    s := pathInJson(sJson, key);
  except
    s := '';
  end;

  if s <> '' then
  begin
    Result := s;
    jData.Free;
    Exit;
  end;

  for i := 0 to jData.Count - 1 do
  begin
    if (jData.Items[i].JSONType = jtArray) or (jData.Items[i].JSONType = jtObject) then
    begin
      s := findInJson(jData.Items[i].AsJSON, key);
      if s <> '' then
        break;
    end
    else
    begin
      try
        jObject := jData.FindPath(key);
        if (jObject = nil) or (jObject.IsNull) then
          s := ''
        else
          s := jObject.GetPath(key).AsString;
      except
        s := '';
      end;
      if s <> '' then
        Break;
    end;

  end;
  Result := s;

  jData.Free;
end;

function TMwindow.simpleHtmlCells(cells: array of string): string;
var
  i: integer;
  s, c: string;

begin
  Result := '';
  if High(cells) < 1 then
    Exit;
  s := '';
  for i := 0 to High(cells) do
  begin
    c := cells[i];
    c := c.Replace(LineEnding, '<br>');
    s := s + '<td>' + c + '</td>';
  end;
  Result := s;
end;

function TMwindow.makeTable(sJson: string): string;
var
  s: string;
begin
  //create simple table with known fields
  Result := '';
  if sJson = '' then
    Exit;

  s := '<table border="1" width="100%" cellspacing="0">';
  s := s + '<tr>' + simpleHtmlCells(['Status podatnika w VAT:',
    findInJson(sJson, 'statusVat')]) + '</tr>';

  s := s + '<tr>' + simpleHtmlCells(['Numer rachunku bankowego',
    '<b>' + findInJson(sJson, 'accountNumbers')]) + '</b></tr>';

  s := s + '<tr>' + simpleHtmlCells(['Nazwa', findInJson(sJson, 'name')]) + '</tr>';
  s := s + '<tr>' + simpleHtmlCells(
    ['Numer NIP', findInJson(sJson, 'nip')]) + '</tr>';
  s := s + '<tr>' + simpleHtmlCells(
    ['Numer REGON', findInJson(sJson, 'regon')]) + '</tr>';
  s := s + '<tr>' + simpleHtmlCells(
    ['Numer KRS', findInJson(sJson, 'krs')]) + '</tr>';

  s := s + '<tr>' + simpleHtmlCells(['Adres siedziby',
    findInJson(sJson, 'residenceAddress')]) + '</tr>';
  s := s + '<tr>' + simpleHtmlCells(['Adres działalności',
    findInJson(sJson, 'workingAddress')]) + '</tr>';
  s := s + '<tr>' + simpleHtmlCells(['Wspólnicy', findInJson(sJson, 'partners')]) +
    '</tr>';
  s := s + '<tr>' + simpleHtmlCells(['Reprezentujący',
    findInJson(sJson, 'representatives')]) + '</tr>';
  s := s + '<tr>' + simpleHtmlCells(['Data rejestracji',
    findInJson(sJson, 'registrationLegalDate')]) + '</tr>';


  s := s + '<tr>' + simpleHtmlCells(['Identyfikator wyszukania',
    findInJson(sJson, 'requestId')]) + '</tr>';
  //table end
  s := s + '</table>';

  Result := s;
end;

procedure TMwindow.BBclearClick(Sender: TObject);
begin
  clearWindow;
end;

procedure TMwindow.BBsearchClick(Sender: TObject);
begin
  if TBlocally.Checked then
    searchLocally
  else
    searchOnline;
end;

procedure TMwindow.ChangeExitEdit(Sender: TObject);
begin
  validateFields;
end;

procedure TMwindow.EnipKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    BBsearchClick(Sender);
end;


end.
