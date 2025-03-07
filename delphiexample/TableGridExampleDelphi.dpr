program TableGridExampleDelphi;

uses
  Forms,
  ufrmExample in 'ufrmExample.pas' {Form1},
  uTableGrid in '..\uTableGrid.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
