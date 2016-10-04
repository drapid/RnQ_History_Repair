unit PwdDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TPwdFrm = class(TForm)
    pwdBox: TEdit;
    Label1: TLabel;
    okBtn: TBitBtn;
    procedure okBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    fPassword: string;
  public
    property Password: string read fPassword;
  end;

var
  PwdFrm: TPwdFrm;

implementation

{$R *.DFM}

procedure TPwdFrm.FormShow(Sender: TObject);
begin
  fPassword := '';
  pwdBox.Clear;
  pwdBox.SetFocus;
end;

procedure TPwdFrm.okBtnClick(Sender: TObject);
begin
  fPassword := Trim(pwdBox.Text);
  Close;
end;

end.
