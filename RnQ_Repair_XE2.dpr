/////////////////////////////////////////////////////////
// This file is part of RnQ_Pepair.
// &RQ - Copyright (c) Massimo Melina (www.rejetto.com)
// R&Q - Copyright (c) Rapid D., RnQ team (www.rnq.ru)
// RnQ_Repair - Copyright (c) C6 Lab (c6lab.spb.ru)

program RnQ_Repair_XE2;

uses
  Forms,
  Windows,
  SysUtils,
  MainFrm in 'MainFrm.pas' {MainForm},
  HistFile in 'HistFile.pas',
  FileIO in 'FileIO.pas',
  AboutFrm in 'AboutFrm.pas' {AboutBox},
  Decode in 'Decode.pas',
  PwdDlg in 'PwdDlg.pas' {PwdFrm},
  OverbyteIcsMD5 in 'OverbyteIcsMD5.pas';

{$R *.res}

function IsFindRQ: Boolean;
var
  Path: string;
begin
 Result := True;
 Path := ExtractFilePath(Application.ExeName);
  if (not FileExists(Path + '&RQ.exe')) then
    if (not FileExists(Path + 'andrq.exe')) then
      if (not FileExists(Path + 'R&Q.exe')) then
        if (not FileExists(Path + 'RnQ.exe')) then begin
          MessageBox(0, 'Запускайте данную программу из каталога с &RQ/R&Q.', 'Ошибка', MB_ICONERROR or MB_OK);
          Result := False;
        end;
end;

begin
  Application.Initialize;
  if (IsFindRQ) then begin
    Application.MainFormOnTaskbar := True;
    Application.Title := 'R&Q - Проверка и ремонт файлов';
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TAboutBox, AboutBox);
    Application.CreateForm(TPwdFrm, PwdFrm);
    Application.Run;
  end;
end.
