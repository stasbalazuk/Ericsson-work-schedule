program Ericsson;

uses
  System.StartUpCopy,
  FMX.Forms,
  eric in 'eric.pas' {fEric};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfEric, fEric);
  Application.Run;
end.
