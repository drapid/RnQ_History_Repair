/////////////////////////////////////////////////////////
// This file is part of RnQ_Pepair.
// &RQ - Copyright (c) Massimo Melina (www.rejetto.com)
// R&Q - Copyright (c) Rapid D., RnQ team (www.rnq.ru)
// RnQ_Repair - Copyright (c) C6 Lab (c6lab.spb.ru)

unit AboutFrm;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

const
{$I RnQBuiltTime.inc}

type
  TAboutBox = class(TForm)
    Memo1: TMemo;
    Image1: TImage;
    Label1: TLabel;
    BuiltLbl: TLabel;
    LabelDevel: TLabel;
    ButtonOK: TButton;
    procedure LabelDevelMouseEnter(Sender: TObject);
    procedure LabelDevelMouseLeave(Sender: TObject);
    procedure LabelDevelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.dfm}

uses
  ShellAPI;

procedure TAboutBox.ButtonOKClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  BuiltLbl.Caption := '—борка: ' + DateTimeToStr(BuiltTime);
end;

procedure TAboutBox.LabelDevelClick(Sender: TObject);
begin
  ShellAPI.ShellExecuteW(self.Handle, 'open', 'http://rnq.ru', nil, nil, SW_SHOWNORMAL);
end;

procedure TAboutBox.LabelDevelMouseEnter(Sender: TObject);
begin
  with (sender as Tlabel).font do Style:=Style+[fsUnderline]
end;

procedure TAboutBox.LabelDevelMouseLeave(Sender: TObject);
begin
  with (sender as Tlabel).font do Style:=Style-[fsUnderline]
end;

end.
 
