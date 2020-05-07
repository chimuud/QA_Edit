program QA_Edit;

uses
  Vcl.Forms,
  uFrmMain in 'uFrmMain.pas' {FrmMain},
  uFrmDemo in 'uFrmDemo.pas' {FrmDemo},
  uDataModule in 'uDataModule.pas' {DM: TDataModule},
  uConfig in 'uConfig.pas',
  uDetails in 'uDetails.pas',
  uFrmDetails in 'uFrmDetails.pas' {FrmDetails},
  uItemized in 'uItemized.pas',
  uIPage in 'uIPage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmDemo, FrmDemo);
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TFrmDetails, FrmDetails);
  Application.Run;
end.
