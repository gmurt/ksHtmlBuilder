program HtmlBuilderDemo;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {Form22},
  ksHtmlBuilder in '..\ksHtmlBuilder.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm22, Form22);
  Application.Run;
end.
