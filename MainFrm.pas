/////////////////////////////////////////////////////////
// This file is part of RnQ_Pepair.
// &RQ - Copyright (c) Massimo Melina (www.rejetto.com)
// R&Q - Copyright (c) Rapid D., RnQ team (www.rnq.ru)
// RnQ_Repair - Copyright (c) C6 Lab (c6lab.spb.ru)

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  StdCtrls, ComCtrls, ExtCtrls, ImgList, Graphics;

const
  STATE_NOTFOUND = 1;
  STATE_NOSELECT = 2;
  STATE_VERIFYONLY = 3;
  STATE_VERIFYANDREPAIR = 4;

type
  TMainForm = class(TForm)
    ComboUserSel: TComboBox;
    Label1: TLabel;
    MemoLog: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    ButtonVerify: TButton;
    GroupBox1: TGroupBox;
    CheckMap: TCheckBox;
    CheckText: TCheckBox;
    CheckBak: TCheckBox;
    ProgressBar: TProgressBar;
    Label4: TLabel;
    ListViewFiles: TListView;
    ImageList1: TImageList;
    ButtonRepair: TButton;
    ImageLogo: TImage;
    Label5: TLabel;
    MemoTip: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ComboUserSelChange(Sender: TObject);
    procedure ListViewFilesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageLogoClick(Sender: TObject);
    procedure ButtonVerifyClick(Sender: TObject);
    procedure ButtonRepairClick(Sender: TObject);
    procedure MemoLogDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListViewFilesDblClick(Sender: TObject);
  private
    { Private declarations }
    RootPath: string;
    IniSet: TStringList;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RunCheckRepair(IsRepair: Boolean);
    function GetLog: string;
    function LoadRnQIni(var LastUser: Cardinal): Boolean;
    procedure LoadIni;
    procedure SaveIni;
    procedure ChangeState(State: Integer);
    procedure RescanState;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  ShellAPI,
  HistFile, FileIO, AboutFrm;

procedure TMainForm.ButtonRepairClick(Sender: TObject);
begin
  RunCheckRepair(True);
end;

procedure TMainForm.ButtonVerifyClick(Sender: TObject);
begin
  RunCheckRepair(False);
end;

procedure TMainForm.ChangeState(State: Integer);
begin
  MemoTip.Lines.Clear;
  case State of
    STATE_NOTFOUND: begin
      MemoTip.Lines.Add('В данном аккаунте файлов истории не найдены.');
      ButtonVerify.Enabled := False;
      ButtonRepair.Enabled := False;
    end;
    STATE_NOSELECT: begin
      MemoTip.Lines.Add('Выберите файлы истории для проверки или ремонта.');
      ButtonVerify.Enabled := False;
      ButtonRepair.Enabled := False;
    end;
    STATE_VERIFYONLY: begin
      MemoTip.Lines.Add('Функция ремонта доступна только после проверки.');
      ButtonVerify.Enabled := True;
      ButtonRepair.Enabled := False;
    end;
    STATE_VERIFYANDREPAIR: begin
      MemoTip.Lines.Add('Сейчас можно отремонтировать поврежденные файлы.');
      ButtonVerify.Enabled := True;
      ButtonRepair.Enabled := True;
    end;
  end;
  MemoTip.Perform(WM_VSCROLL, SB_TOP, 0);
end;

procedure TMainForm.ComboUserSelChange(Sender: TObject);
var
  tsr: TSearchRec;
  Str1: string;
  ListItem: TListItem;
begin
  ListViewFiles.Clear;
  if (FindFirst(ComboUserSel.Text + '\HISTORY\*.*', faNormal, tsr) = 0) then
    repeat
      Str1 := Trim(tsr.Name);
      ListItem := nil;
      try
        if (IntToStr(StrToInt(Str1)) = Str1) then
          ListItem := ListViewFiles.Items.Add;
          ListItem.ImageIndex := 0;
          ListItem.Caption := Str1;
          ListItem.SubItems.Add('Не проверен');
          ListItem.SubItems.Add('');
          ListItem.SubItems.Add('');
      except
      end;
    until (FindNext(tsr) <> 0);
  FindClose(tsr);
  if FileExists(ComboUserSel.Text + '\HISTORY\0spamers') then begin
    ListItem := ListViewFiles.Items.Add;
    ListItem.ImageIndex := 0;
    ListItem.Caption := '0spamers';
    ListItem.SubItems.Add('Не проверен');
    ListItem.SubItems.Add('');
    ListItem.SubItems.Add('');
  end;
  if (ListViewFiles.Items.Count > 0) then begin
    ListViewFiles.SelectAll;
    ChangeState(STATE_VERIFYONLY);
  end
  else
    ChangeState(STATE_NOTFOUND);
  ProgressBar.Position := 0;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IniSet := TStringList.Create;
  IniSet.Add('create-map');
  IniSet.Add('create-txt');
end;

destructor TMainForm.Destroy;
begin
  IniSet.Free;
  inherited Destroy;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
  tsr: TSearchRec;
  Str1: string;
  List1: TStringList;
  LastUser: Cardinal;
begin
  Self.Caption := Application.Title;
  RootPath := ExtractFilePath(Application.ExeName);
  LoadIni;

  List1 := TStringList.Create;
  if (FindFirst(RootPath + '*.*', faDirectory, tsr) = 0) then
    repeat
      Str1 := UpperCase(Trim(tsr.Name));
      if ((Str1 <> '.') and (Str1 <> '..')
          and (Str1 <> 'THEMES') and (Str1 <> 'DOCS') and (Str1 <> 'PLUGINS')) then
        if ((tsr.Attr and faDirectory) <> 0) then
          List1.Add(Str1);
    until (FindNext(tsr) <> 0);
  FindClose(tsr);
  if (List1.Count > 0) then
    for I := 0 to (List1.Count - 1) do begin
      if (FindFirst(RootPath + List1[I] + '\HISTORY', faDirectory, tsr) = 0) then
        ComboUserSel.Items.Add(List1[I]);
      FindClose(tsr);
    end;
  List1.Free;

  if (ComboUserSel.Items.Count > 0) then begin
    LastUser := 0;
    ComboUserSel.ItemIndex := 0;
    if (LoadRnQIni(LastUser)) then begin
      I := ComboUserSel.Items.IndexOf(Trim(IntToStr(LastUser)));
      if (I >= 0) then
        ComboUserSel.ItemIndex := I;
    end;
    ComboUserSelChange(self);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveIni;
  GetLog;
end;

function TMainForm.GetLog: string;
begin
  Result := ChangeFileExt(ExtractFileName(Application.ExeName), '.log');
  MemoLog.Lines.SaveToFile(Result);
end;

procedure TMainForm.ImageLogoClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.ListViewFilesDblClick(Sender: TObject);
begin
  with Sender as TListView do
    RunCheckRepair(ItemFocused.ImageIndex = 2);
end;

procedure TMainForm.ListViewFilesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  RescanState;
  ProgressBar.Position := 0;
end;

procedure TMainForm.LoadIni;
var
  IniName, S, SSet, Val: string;
  Ini: TFileIO;
  P: Integer;
begin
  IniName := ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  if (FileExists(IniName)) then begin
    Ini := TFileIO.Create(0);
    if (Ini.FileOpen(IniName) = 0) then
      repeat
        if (Ini.ReadLn(S) <> 0) then
          break;
        P := Pos('=', S);
        if (P > 0) then begin
          SSet := LowerCase(Trim(Copy(S, 1, P - 1)));
          Val := LowerCase(Trim(Copy(S, P + 1, Length(S) - P)));
          P := IniSet.IndexOf(SSet);
          case (P) of
            0: begin
              // create-map
              if ((Val = 'yes') or (Val = '1')) then
                CheckMap.Checked := True
              else
                CheckMap.Checked := False;
            end;
            1: begin
              // create-txt
              if ((Val = 'yes') or (Val = '1')) then
                CheckText.Checked := True
              else
                CheckText.Checked := False;
            end;
          end;
        end;
      until (False);
    Ini.Free;
  end;
end;

function TMainForm.LoadRnQIni(var LastUser: Cardinal): Boolean;
var
  IniName, S1, S2: string;
  Ini: TFileIO;
  P: Integer;
begin
  Result := False;
  IniName := RootPath + 'common.ini';
  if (FileExists(IniName)) then begin
    Ini := TFileIO.Create(0);
    if (Ini.FileOpen(IniName) = 0) then
      repeat
        if (Ini.ReadLn(S1) <> 0) then
          break;
        P := Pos('last-user=', LowerCase(S1));
        if (P > 0) then begin
          S2 := Copy(S1, 11, Length(S1) - 10);
          LastUser := 0;
          try
            LastUser := StrToInt(S2);
          except
          end;
          Result := (LastUser <> 0);
          break;
        end;
      until (False);
    Ini.Free;
  end;
end;

procedure TMainForm.MemoLogDblClick(Sender: TObject);
begin
  ShellAPI.ShellExecute(self.Handle, 'open', 'notepad.exe', PChar(GetLog), nil, SW_SHOWNORMAL);
end;

procedure TMainForm.RescanState;
var
  ST, I: Integer;
begin
  if (ListViewFiles.Items.Count = 0) then
    ST := STATE_NOTFOUND
  else if (ListViewFiles.SelCount = 0) then
    ST := STATE_NOSELECT
  else begin
    ST := STATE_VERIFYONLY;
    for I := 0 to (ListViewFiles.Items.Count - 1) do begin
      if (ListViewFiles.Items[I].Selected) then begin
        if (ListViewFiles.Items[I].ImageIndex = 2) then begin
          ST := STATE_VERIFYANDREPAIR;
          break;
        end;
      end;
    end;
  end;
  ChangeState(ST);
end;

procedure TMainForm.RunCheckRepair(IsRepair: Boolean);
var
  Path, FileWithPath, HistUIN: string;
  I, J, L: Integer;
  R, Halt, IsCorrupt: Boolean;
  hist, hist2: THistFile;
  text, map: TFileIO;
  IsNewHist: boolean;
  Str1, Str2: string;
  Offs1: Cardinal;
begin
  if (ListViewFiles.SelCount > 0) then begin
    ProgressBar.Position := 0;
    ProgressBar.Min := 0;
    ProgressBar.Max := ListViewFiles.SelCount;
    J := 0;
    MemoLog.Clear;
    if (IsRepair) then
      Str1 := ' Запущен ремонт файлов'
    else
      Str1 := ' Запущена проверка файлов';
    MemoLog.Lines.Add(DateTimeToStr(Now) + Str1);
    MemoLog.Lines.Add('');

    Path := RootPath + Trim(ComboUserSel.Text) + '\HISTORY\';
    hist := THistFile.Create(Path);
    hist.OwnUIN := Trim(ComboUserSel.Text);
    hist2 := nil; text := nil; map := nil; // erase compiler warning
    if (IsRepair) then
      hist2 := THistFile.Create(Path);
    if (CheckText.Checked) then
      text := TFileIO.Create(0);
    if (CheckMap.Checked) then
      map := TFileIO.Create(0);

    for I := 0 to (ListViewFiles.Items.Count - 1) do begin
      if (ListViewFiles.Items[I].Selected) then begin
        HistUIN := Trim(ListViewFiles.Items[I].Caption);
        IsCorrupt := (ListViewFiles.Selected.ImageIndex = 2);
        FileWithPath := Path + HistUIN;
        MemoLog.Lines.Add('Файл истории: ' + FileWithPath);

        IsNewHist := False;
        Halt := False;
        Offs1 := 0;

        while (True) do begin

          if (not hist.HistOpen(HistUIN)) then begin
            Str1 := IntToStr(hist.ErrorKind) + '/' + IntToStr(hist.ErrorCode);
            ListViewFiles.Items[I].SubItems[0] := 'Ошибка I/O';
            ListViewFiles.Items[I].SubItems[1] := Str1;
            ListViewFiles.Items[I].SubItems[2] := 'Ошибка при открытии файла';
            MemoLog.Lines.Add('Ошибка при открытии файла: ' + Str1);
            Halt := True;
            break;
          end;
          if (IsRepair and IsCorrupt) then
            if (not hist2.HistCreate(HistUIN)) then begin
              Str1 := IntToStr(hist2.ErrorKind) + '/' + IntToStr(hist2.ErrorCode);
              ListViewFiles.Items[I].SubItems[0] := 'Ошибка I/O';
              ListViewFiles.Items[I].SubItems[1] := Str1;
              ListViewFiles.Items[I].SubItems[2] := 'Ошибка при создании файла';
              MemoLog.Lines.Add('Ошибка при создании файла Rep: ' + Str1);
              Halt := True;
              break;
            end;
          if (CheckText.Checked) then
            if (text.FileCreate(FileWithPath + '.txt') <> 0) then begin
              Str1 := IntToStr(text.ErrorKind) + '/' + IntToStr(text.ErrorCode);
              MemoLog.Lines.Add('Ошибка при создании файла Txt: ' + Str1);
              Halt := True;
              break;
            end;
          if (CheckMap.Checked) then
            if (map.FileCreate(FileWithPath + '.map') <> 0) then begin
              Str1 := IntToStr(map.ErrorKind) + '/' + IntToStr(map.ErrorCode);
              MemoLog.Lines.Add('Ошибка при создании файла Map: ' + Str1);
              Halt := True;
              break;
            end;

          MemoLog.Lines.Add('Размер: ' + IntToStr(hist.FileSize));

          repeat
            if (not (IsRepair and IsCorrupt)) then
              R := hist.ChunkRead
            else
              R := hist.ChunkReadRepair;

            if (R) then begin
              if (IsRepair and IsCorrupt) then begin
                R := hist2.ChunkWrite(hist);
                if (R) then
                  hist2.FileFlushBuffers;
              end;
              L := hist.ChunkBegin - Offs1;
              if ((CheckMap.Checked) and (L > 0)) then
                map.WriteLn(IntToHex(Offs1, 8) + '-' + IntToHex(hist.ChunkBegin - 1, 8)
                    + ' ' + IntToStr(L) + ' bytes skipped');

              if (hist.What = Cardinal(HI_event)) then begin
                if ((CheckText.Checked) and (hist.EventType = HI_event_msg)) then
                  text.WriteLn(DateTimeToStr(hist.EventTime)
                   + ' ' + IntToSTR(hist.SenderUIN)
                    + ' ' + hist.Body);
                if (CheckMap.Checked) then
                  map.WriteLn(IntToHex(hist.ChunkBegin, 8) + '-' + IntToHex(hist.ChunkEnd, 8)
                    + ' ' + IntToHex(hist.EventType, 2)
                    + ' ' + IntToHex(hist.ExtraInfoLen, 8)
                    + ' ' + IntToHex(hist.BodyLen, 8));
              end
              else
                if (CheckMap.Checked) then begin
                  if (hist.What = Cardinal(HI_hashed)) then
                    Str1 := 'Hashed'
                  else if (hist.What = Cardinal(HI_cryptMode)) then
                    Str1 := 'Crypt-Mode'
                  else
                    Str1 := IntToHex(hist.What, 8);

                  map.WriteLn(IntToHex(hist.ChunkBegin, 8) + '-' + IntToHex(hist.ChunkEnd, 8)
                    + ' ' + Str1);
                end;
              Offs1 := hist.ChunkEnd + 1;
            end
            else if ((hist.ResReason <> HistFile.ERR_IO) and (hist.ResReason <> HistFile.ERR_EOF)) then
              if (CheckMap.Checked) then
                map.WriteLn(IntToHex(hist.ChunkBegin, 8) + '-........ X');
          until (not R);

          if (not (IsRepair and IsCorrupt)) then begin
            if ((hist.ResReason = 0) or (hist.ResReason = HistFile.ERR_EOF)) then begin
              ListViewFiles.Items[I].Selected := False;
              ListViewFiles.Items[I].ImageIndex := 1;
              ListViewFiles.Items[I].SubItems[0] := 'ОК';
              ListViewFiles.Items[I].SubItems[1] := '';
              ListViewFiles.Items[I].SubItems[2] := '';
              MemoLog.Lines.Add('ОК');
            end
            else if (hist.ResReason = HistFile.ERR_IO) then begin
              Str1 := IntToStr(hist.ErrorKind) + '/' + IntToStr(hist.ErrorCode);
              ListViewFiles.Items[I].SubItems[0] := 'Ошибка I/O';
              ListViewFiles.Items[I].SubItems[1] := Str1;
              ListViewFiles.Items[I].SubItems[2] := 'Ошибка при чтении файла';
              MemoLog.Lines.Add('Ошибка при чтении файла: ' + Str1 + ' @ ' + IntToHex(hist.Offset, 8));
            end
            else begin
              Str1 := IntToStr(hist.ResReason);
              Str2 := hist.ResReasonDesc;
              ListViewFiles.Items[I].ImageIndex := 2;
              ListViewFiles.Items[I].SubItems[0] := 'Поврежден';
              ListViewFiles.Items[I].SubItems[1] := Str1;
              ListViewFiles.Items[I].SubItems[2] := Str2;
              MemoLog.Lines.Add('Ошибка: ' + Str1 + ' (' + Str2 + ') @ ' + IntToHex(hist.ErrorOffs, 8));
            end;
          end
          else begin
            if (((hist.ResReason = 0) or (hist.ResReason = HistFile.ERR_EOF)) and (hist2.ResReason = 0)) then begin
              IsNewHist := True;
              ListViewFiles.Items[I].Selected := False;
              ListViewFiles.Items[I].ImageIndex := 1;
              ListViewFiles.Items[I].SubItems[0] := 'ОК';
              ListViewFiles.Items[I].SubItems[1] := '';
              ListViewFiles.Items[I].SubItems[2] := '';
              MemoLog.Lines.Add('ОК');
            end
            else if (hist.ResReason = HistFile.ERR_IO) then begin
              IsNewHist := False;
              Str1 := IntToStr(hist.ErrorKind) + '/' + IntToStr(hist.ErrorCode);
              ListViewFiles.Items[I].SubItems[0] := 'Ошибка I/O';
              ListViewFiles.Items[I].SubItems[1] := Str1;
              ListViewFiles.Items[I].SubItems[2] := 'Ошибка при чтении файла';
              MemoLog.Lines.Add('Ошибка при чтении файла: ' + Str1);
            end
            else if (hist2.ResReason = HistFile.ERR_IO) then begin
              IsNewHist := False;
              Str1 := IntToStr(hist2.ErrorKind) + '/' + IntToStr(hist2.ErrorCode);
              ListViewFiles.Items[I].SubItems[0] := 'Ошибка I/O';
              ListViewFiles.Items[I].SubItems[1] := Str1;
              ListViewFiles.Items[I].SubItems[2] := 'Ошибка при записи в файл';
              MemoLog.Lines.Add('Ошибка при записи в файл: ' + Str1);
            end;
          end;

          break;
        end; // while true

        if (CheckMap.Checked) then begin
          map.WriteLn('');
          map.WriteLn('00000000-' + IntToHex(hist.FileSize - 1, 8) + ' Total');
          map.FileClose;
        end;
        if (CheckText.Checked) then
          text.FileClose;
        if (IsRepair) then
          hist2.HistClose;
        hist.HistClose;

        if (IsNewHist) then begin
          if (CheckBak.Checked) then
            RenameFile(FileWithPath, FileWithPath + '.bak')
          else
            DeleteFile(FileWithPath);
          RenameFile(FileWithPath + '.rep', FileWithPath);
        end
        else if (IsRepair) then begin
          DeleteFile(FileWithPath + '.rep');
        end;

        MemoLog.Lines.Add('');
        Inc(J);
        ProgressBar.Position := J;

        if (Halt) then
          break;
      end;
    end; // for

    if (CheckMap.Checked) then
      map.Free;
    if (CheckText.Checked) then
      text.Free;
    if (IsRepair) then
      hist2.Free;
    hist.Free;

    MemoLog.Lines.Add(DateTimeToStr(Now) + ' Завершено');
    RescanState;
    for I := 0 to (ListViewFiles.Items.Count - 1) do
      if (ListViewFiles.Items[I].Selected) then begin
        ListViewFiles.Items[I].MakeVisible(True);
        break;
      end;
  end;
end;

procedure TMainForm.SaveIni;
var
  IniName, Val: string;
  Ini: TFileIO;
  I: Integer;
begin
  IniName := ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  Ini := TFileIO.Create(0);
  if (Ini.FileCreate(IniName) = 0) then begin
    for I := 0 to IniSet.Count - 1 do begin
      case (I) of
        0: begin
          // create-map
          if (CheckMap.Checked) then
            Val := 'Yes'
          else
            Val := 'No';
        end;
        1: begin
          // create-txt
          if (CheckText.Checked) then
            Val := 'Yes'
          else
            Val := 'No';
        end;
      end;
      if (Ini.WriteLn(IniSet[I] + '=' + Val) <> 0) then
        break;
    end;
    Ini.FileClose;
  end;
  Ini.Free;
end;

end.
