program BialyMis;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FrameViewer09, lazcontrols, printer4lazarus, mainwindow, aboutw
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Biały Miś';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMwindow, Mwindow);
  Application.CreateForm(TFabout, Fabout);
  Application.Run;
end.

