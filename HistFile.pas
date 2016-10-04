/////////////////////////////////////////////////////////
// This file is part of RnQ_Pepair.
// &RQ - Copyright (c) Massimo Melina (www.rejetto.com)
// R&Q - Copyright (c) Rapid D., RnQ team (www.rnq.ru)
// RnQ_Repair - Copyright (c) C6 Lab (c6lab.spb.ru)

unit HistFile;

{ *** FILE FORMAT ***

The history file is a sequence of CHUNKs.
A CHUNK is so made:

int what
  case -1 (event)
    byte      event-type
    int       sender-uin
    datetime  event-time
    int       N
    N byte    extra-info
    string    body (crypted)
  case -2 (hashed)
    string    hashed-password
  case -3 (crypt-mode)
    int       following-data-length
    byte      crypt-mode (0 = simple, 1 = key method #1)
}

interface

uses
  FileIO;

const
  HI_event = -1;
  HI_hashed = -2;
  HI_cryptMode = -3;

  HI_event_msg = 01;
  HI_event_url = 02;
  HI_event_contacts = 03;
  HI_event_file = 04;
  HI_event_authReq = 05;
  HI_event_AddedYou = 06;
  HI_event_oncoming = 07;
  HI_event_offgoing = 08;
  HI_event_auth = 09;
  HI_event_authDenied = 10;
  HI_event_statuschange = 11;
  HI_event_automsgreq = 12;
  HI_event_gcard = 13;
  HI_event_automsg = 14;

  CRYPT_SIMPLE = 0;
  CRYPT_KEY1 = 1;

  ERR_NONE = 0;
  ERR_EOF = 1;
  ERR_IO = 2;
  ERR_NOTLOADED = 3;
  ERR_CHUNKNOTFOUND = 101;
  ERR_CHUNKCORRUPTED = 102;
  //ERR_WHAT = 103;
  //ERR_EVENT = 104;
  //ERR_UIN = 105;
  //ERR_TIME = 106;
  //ERR_LENEXTRA = 107;
  ERR_EXTRA = 108;
  ERR_LENBODY = 109;

  CHUNK_FIXED_LEN = 21; // WHAT+EVENT+UIN+TIME+LEN (4+1+4+8+4)
  CHUNK_HASHED_LEN = 24; // WHAT+LEN+MD5 (4+4+16)
  CHUNK_CRYPT_LEN = 9; // WHAT+LEN+CRYPT (4+4+1)

  SET_WHAT = $0001;
  SET_EVENT = $0002;
  SET_UIN = $0004;
  SET_TIME = $0008;
  SET_LENEXTRA = $0010;
  SET_EXTRA = $0020;
  SET_LENBODY = $0040;
  SET_BODY = $0080;
  SET_HASHED = $0100;
  SET_CRYPT = $0200;

type

  THistFile = class (TFileIO)
  private
    fPath: string;
    fOwnUIN: Cardinal;
    fHistUIN: Cardinal;
    fResReason: Integer;
    fChunkFixBuff: PByte;
    fFields: Word;
    fIsChunkLoaded: Boolean;
    fChunkBegin: Cardinal;
    fChunkEnd: Cardinal;
    fErrorOffs: Cardinal;
    fWhat: Cardinal;
    fEventType: Byte;
    fSenderUIN: Cardinal;
    fEventTime: TDateTime;
    fExtraInfo: PByte;
    fExtraInfoLen: Cardinal;
    fBody: AnsiString;
    fBodyLen: Cardinal;
    fHashed: AnsiString;
    fCryptMode: Byte;
    fCryptPwd: AnsiString;
    fIsPwdCancel: Boolean;
    fCryptKey: Cardinal;
  protected
    function getOwnUIN: string;
    procedure setOwnUIN(UIN: string);
    function getDecriptedBody: string;
    function getResReasonDesc: string;
    function getCryptPassword: Boolean;
  public
    constructor Create(Path: string);
    destructor Destroy; override;
    function HistOpen(UIN: string): Boolean;
    function HistCreate(UIN: string): Boolean;
    procedure HistClose;
    function ChunkDetectFields(IsSave: Boolean): Boolean;
    function CheckExtraInfo: Boolean;
    function ChunkRead: Boolean;
    function ChunkReadRepair: Boolean;
    function ChunkScanRead(var Offs: Cardinal; IsSave: Boolean): Boolean;
    function ChunkScanRepair(var Offs: Cardinal; IsSave: Boolean): Boolean;
    function ChunkWrite(src: THistFile): Boolean;
    property Path: string read fPath;
    property OwnUIN: string read getOwnUIN write setOwnUIN;
    property ResReason: Integer read fResReason;
    property ResReasonDesc: string read getResReasonDesc;
    property IsChunkLoaded: Boolean read fIsChunkLoaded;
    property ChunkBegin: Cardinal read fChunkBegin;
    property ChunkEnd: Cardinal read fChunkEnd;
    property ErrorOffs: Cardinal read fErrorOffs;
    property What: Cardinal read fWhat;
    property EventType: Byte read fEventType;
    property SenderUIN: Cardinal read fSenderUIN;
    property EventTime: TDateTime read fEventTime;
    property ExtraInfoLen: Cardinal read fExtraInfoLen;
    property Body: string read getDecriptedBody;
    property BodyLen: Cardinal read fBodyLen;
  end;

implementation

{ THistFile }

uses
  Windows, SysUtils, Math, Decode, OverbyteIcsMD5, PwdDlg;

{
an extra-info field is a sequence of:
  int     data-id
  int     data-length (N)
  N byte  data-body
}
function THistFile.CheckExtraInfo: Boolean;
var
  PI1, PI2: ^Cardinal;
  P: Cardinal;
begin
  Result := False;
  if (fExtraInfoLen < 8) then
    Exit;
  P := 0;
  repeat
    PI1 := @fExtraInfo[P];
    PI2 := @fExtraInfo[P + 4];
    if ((PI1^ = 1) and (PI2^ <> 4)) then
      break;
    Inc(P, 8 + PI2^);
    if (P = fExtraInfoLen) then
      Result := True;
  until (P >= fExtraInfoLen);
end;

function THistFile.ChunkDetectFields(IsSave: Boolean): Boolean;
var
  W, PI: ^Cardinal;
  PB: ^Byte;
  PD: ^Double;
  Y, Y2, M, D: Word;
begin
  Result := False;
  fFields := 0;
  //---------- what
  W := @fChunkFixBuff[0];
  if ((W^ = Cardinal(HI_event)) or (W^ = Cardinal(HI_hashed)) or (W^ = Cardinal(HI_cryptMode))) then begin
    fFields := fFields or SET_WHAT;
    if (IsSave) then
      fWhat := W^;
  end;
  //---------- event-type
  PB := @fChunkFixBuff[4];
  if ((PB^ >= HI_event_msg) and (PB^ <= HI_event_automsg)) then begin
    fFields := fFields or SET_EVENT;
    if (IsSave) then
      fEventType := PB^;
  end;
  //---------- sender-uin
  PI := @fChunkFixBuff[5];
  if ((PI^ <> 0) and ((fHistUIN = 0) or (fHistUIN = PI^) or (fOwnUIN = PI^))) then begin
    fFields := fFields or SET_UIN;
    if (IsSave) then
      fSenderUIN := PI^;
  end;
  //---------- event-time
  PD := @fChunkFixBuff[9];
  Y := 0;
  try
    DecodeDate(Now, Y2, M, D);
    DecodeDate(PD^, Y, M, D);
  except
  end;
  if ((2001 <= Y) and (Y <= Y2)) then begin
    fFields := fFields or SET_TIME;
    if (IsSave) then
      fEventTime := PD^;
  end;
  //---------- extra-info-len
  PI := @fChunkFixBuff[17];
  if ((PI^ >= 12) and (PI^ <= 32)) then begin
    fFields := fFields or SET_LENEXTRA;
    if (IsSave) then
      fExtraInfoLen := PI^;
  end;
  if (((W^ = Cardinal(HI_event)) and ((fFields and SET_EVENT) <> 0))
    or ((fFields and SET_TIME) <> 0)
      or (((fFields and SET_UIN) <> 0) and (fHistUIN <> 0))) then begin
  //---------- OK
    Result := True;
  end
  else if (W^ = Cardinal(HI_hashed))then begin
  //---------- hashed
    PI := @fChunkFixBuff[4];
    if (PI^ = 16) then begin
      fFields := fFields or SET_HASHED;
      Result := True;
    end;
  end
  else if (W^ = Cardinal(HI_cryptMode)) then begin
  //---------- crypt-mode
    PI := @fChunkFixBuff[4];
    if (PI^ = 1) then begin
      PB := @fChunkFixBuff[8];
      if ((PB^ = CRYPT_SIMPLE) or (PB^ = CRYPT_KEY1)) then begin
        fFields := fFields or SET_CRYPT;
        if (IsSave) then
          fCryptMode := PB^;
        Result := True;
      end;
    end;
  end;
end;

function THistFile.ChunkRead: Boolean;
var
  Offs1, Offs2: Cardinal;
  Len1: Cardinal;
  F: Word;
begin
  Result := False;
  fIsChunkLoaded := False;
  fChunkBegin := self.Offset;
  if (ReadBlock(fChunkFixBuff[0], CHUNK_FIXED_LEN) = 0) then begin
    if (ChunkDetectFields(True)) then begin
      F := SET_WHAT or SET_EVENT or SET_UIN or SET_TIME or SET_LENEXTRA;
      if ((fFields and F) = F) then begin
        if (ReadBlock(fExtraInfo^, fExtraInfoLen) = 0) then begin
          if (CheckExtraInfo) then begin
            if (ReadDWord(fBodyLen) = 0) then begin
              Offs1 := self.Offset;
              if (ChunkScanRead(Offs2, False)) then
                fChunkEnd := Offs2 - 1
              else if (fResReason = ERR_EOF) then begin
                fChunkEnd := GetOffsetEof - 1;
                fResReason := ERR_NONE;
              end
              else
                Exit;
              self.Offset := Offs1;
              Len1 := fChunkEnd + 1 - Offs1;
              if (fBodyLen = Len1) then begin
                // chunk OK :)
                if (fBodyLen > 0) then begin
                  SetLength(fBody, fBodyLen);
                  if (ReadBlock(fBody[1], fBodyLen) = 0) then begin
                    fIsChunkLoaded := True;
                    fResReason := ERR_NONE;
                    Result := True;
                    Exit;
                  end
                  else
                    fResReason := ERR_IO;
                end
                else begin
                  fIsChunkLoaded := True;
                  fResReason := ERR_NONE;
                  Result := True;
                  Exit;
                end;
              end
              else begin
                fResReason := ERR_LENBODY;
                fErrorOffs := self.Offset - 4;
              end;
            end
            else
              fResReason := ERR_IO;
          end
          else begin
            fResReason := ERR_EXTRA;
            fErrorOffs := self.Offset - fExtraInfoLen;
          end;
        end
        else
          fResReason := ERR_IO;
      end
      else begin
        F := SET_WHAT or SET_HASHED;
        if ((fFields and F) = F) then begin
          self.Offset := fChunkBegin + 8; // WHAT+LEN (4+4)
          SetLength(fHashed, 16); // MD5 has fixed len
          if (ReadBlock(fHashed[1], 16) = 0) then begin
            // chunk OK :)
            fChunkEnd := fChunkBegin + CHUNK_HASHED_LEN - 1;
            fIsChunkLoaded := True;
            fResReason := ERR_NONE;
            Result := True;
            Exit;
          end
          else
            fResReason := ERR_IO;
        end
        else begin
          F := SET_WHAT or SET_CRYPT;
          if ((fFields and F) = F) then begin
            // chunk OK :)
            fChunkEnd := fChunkBegin + CHUNK_CRYPT_LEN - 1;
            self.Offset := fChunkEnd + 1;
            fIsChunkLoaded := True;
            fResReason := ERR_NONE;
            Result := True;
            Exit;
          end
          else begin
            fResReason := ERR_CHUNKCORRUPTED;
            fErrorOffs := self.Offset - CHUNK_FIXED_LEN;
          end;
        end;
      end;
    end
    else begin
      fResReason := ERR_CHUNKNOTFOUND;
      fErrorOffs := self.Offset - CHUNK_FIXED_LEN;
    end;
  end
  else
    fResReason := ERR_IO;
  if (fResReason = ERR_IO) then begin
    if (self.ErrorKind = FileIO.ERR_EOF) then
      fResReason := ERR_EOF;
  end;
end;

function THistFile.ChunkReadRepair: Boolean;
var
  Offs1, Offs2: Cardinal;
  Len1, ChunkLen: Cardinal;
  F0, F: Word;
  IsRepeat, IsBodyLenScan: Boolean;
  PI: ^Integer;
function BodyLenScan(var Offs: Cardinal): Boolean;
var
  W: Word;
  PB: ^Byte;
begin
  Offs := Cardinal(-1);
  self.Offset := fChunkBegin + CHUNK_FIXED_LEN;
  if (ReadWord(W) = 0) then begin
    PB := @W;
    Inc(PB); // hi byte
    while (True) do begin
      if (W = 0) then begin
        Offs := self.Offset;
      end;
      if (self.Offset > (fChunkEnd - 1)) then
        break;
      W := W shr 8;
      if (ReadByte(PB^) <> 0) then
        break;
    end;
  end;
  if (self.ErrorKind <> 0) then begin
    if (self.ErrorKind = FileIO.ERR_EOF) then
      fResReason := ERR_NONE
    else
      fResReason := ERR_IO;
  end;
  Result := ((fResReason  = ERR_NONE) and (Offs <> Cardinal(-1)));
end;
begin
  Result := False;
  fIsChunkLoaded := False;
  repeat
    if (ChunkScanRepair(Offs1, True)) then begin
      fChunkBegin := Offs1;
      F0 := fFields;
      if (ChunkScanRepair(Offs1, False)) then
        fChunkEnd := Offs1 - 1
      else if (fResReason = ERR_EOF) then begin
        fChunkEnd := GetOffsetEof - 1;
        fResReason := ERR_NONE;
      end
      else
        break;
      self.Offset := fChunkBegin + CHUNK_FIXED_LEN;
      ChunkLen := fChunkEnd + 1 - fChunkBegin;
      if (ChunkLen < (CHUNK_FIXED_LEN + 16)) then begin
        // too short, corrupted
        continue;
      end;
      fFields := F0;

      F := (SET_WHAT or SET_EVENT);
      if ((((fFields and F) = F) and (fWhat = Cardinal(HI_event)))
        or ((fFields and SET_TIME) <> 0)
          or (((fFields and SET_UIN) <> 0) and (fHistUIN <> 0))) then begin

        if ((fFields and SET_WHAT) = 0) then
          fWhat := Cardinal(HI_event);
        if ((fFields and SET_EVENT) = 0) then
          fEventType := HI_event_msg;
        if ((fFields and SET_UIN) = 0) then
          if (fSenderUIN = 0) then
            fSenderUIN := fHistUIN;
        if ((fFields and SET_TIME) = 0) then
          if (fEventTime = 0) then
            fEventTime := StrToDateTime('01.01.2001');

        Len1 := ChunkLen - CHUNK_FIXED_LEN;
        IsBodyLenScan := False;

        if (((fFields and SET_LENEXTRA) = 0) or ((fExtraInfoLen + 4) > Len1)) then begin
          if (BodyLenScan(Offs1)) then begin
            IsBodyLenScan := True;
            self.Offset := fChunkBegin + CHUNK_FIXED_LEN;
            fExtraInfoLen := Len1 - (fChunkEnd + 1 - Offs1) - 4;
            if ((fExtraInfoLen < 12) and (fExtraInfoLen > 32)) then
              continue;
          end
          else
            continue;
        end;

        IsRepeat := False;
        repeat
          if (ReadBlock(fExtraInfo^, fExtraInfoLen) <> 0) then begin
            fResReason := ERR_IO;
            break;
          end;
          if (ReadDWord(fBodyLen) <> 0) then begin
            fResReason := ERR_IO;
            break;
          end;
          Offs2 := self.Offset;

          if (fBodyLen <> (fChunkEnd + 1 - Offs2)) then begin
            if (not CheckExtraInfo) then begin
              if (fExtraInfoLen <> 12) then begin
                if (not IsBodyLenScan) then begin
                  IsBodyLenScan := True;
                  if BodyLenScan(Offs1) then begin
                    self.Offset := fChunkBegin + CHUNK_FIXED_LEN;
                    fBodyLen := fChunkEnd + 1 - Offs1;
                    fExtraInfoLen := Len1 - fBodyLen - 4;
                    if ((fExtraInfoLen >= 12) and (fExtraInfoLen <= 32)) then begin
                      continue;
                    end;
                  end;
                end;
                IsRepeat := True;
                break;
              end
              else begin
                fBodyLen := Len1 - fExtraInfoLen - 4;
                PI := @fExtraInfo[0];
                PI^ := 1;
                PI := @fExtraInfo[4];
                PI^ := 4;
                PI := @fExtraInfo[8];
                PI^ := 0;
              end;
            end
            else begin
              fBodyLen := Len1 - fExtraInfoLen - 4;
            end;
          end
          else if (not CheckExtraInfo) then begin
            fExtraInfoLen := 12; // ;-)
            PI := @fExtraInfo[0];
            PI^ := 1;
            PI := @fExtraInfo[4];
            PI^ := 4;
            PI := @fExtraInfo[8];
            PI^ := 0;
          end;
          break;
        until (False);

        if (fResReason <> ERR_NONE) then
          break;
        if (IsRepeat) then
          continue;

        if (fBodyLen > 0) then begin
          SetLength(fBody, fBodyLen);
          if (ReadBlock(fBody[1], fBodyLen) <> 0) then begin
            fResReason := ERR_IO;
            break;
          end;
        end;
        fIsChunkLoaded := True;
        fResReason := ERR_NONE;
        Result := True;
        Exit;
      end
      else begin
        F := SET_WHAT or SET_HASHED;
        if ((fFields and F) = F) then begin
          if (ChunkLen <> CHUNK_HASHED_LEN) then
            continue;
          self.Offset := fChunkBegin + 8;
          SetLength(fHashed, 16);
          if (ReadBlock(fHashed[1], 16) = 0) then begin
            fIsChunkLoaded := True;
            fResReason := ERR_NONE;
            Result := True;
            Exit;
          end
          else
            fResReason := ERR_IO;
        end
        else begin
          F := SET_WHAT or SET_CRYPT;
          if ((fFields and F) = F) then begin
            if (ChunkLen <> CHUNK_CRYPT_LEN) then
              continue;
            self.Offset := fChunkEnd + 1;
            fIsChunkLoaded := True;
            fResReason := ERR_NONE;
            Result := True;
            Exit;
          end;
          //else - never, see ChunkDetectFields
        end;
      end;
    end;
    break;
  until (False);
end;

function THistFile.ChunkScanRead(var Offs: Cardinal; IsSave: Boolean): Boolean;
var
  F: Word;
begin
  Result := False;
  if (ReadBlock(fChunkFixBuff[0], CHUNK_FIXED_LEN) = 0) then begin
    F := SET_WHAT or SET_EVENT or SET_UIN or SET_TIME or SET_LENEXTRA;
    repeat
      if (ChunkDetectFields(IsSave)) then begin
        if ((fFields and F) = F) then begin
          Offs := self.Offset - CHUNK_FIXED_LEN;
          fResReason := ERR_NONE;
          Result := True;
          Exit;
        end;
      end;
      Move(fChunkFixBuff[1], fChunkFixBuff[0], CHUNK_FIXED_LEN - 1);
      if (ReadByte(fChunkFixBuff[CHUNK_FIXED_LEN - 1]) <> 0) then
        break;
    until (False);
  end;
  if (self.ErrorKind <> 0) then begin
    if (self.ErrorKind = FileIO.ERR_EOF) then
      fResReason := ERR_EOF
    else
      fResReason := ERR_IO;
  end;
end;

function THistFile.ChunkScanRepair(var Offs: Cardinal; IsSave: Boolean): Boolean;
begin
  Result := False;
  if (ReadBlock(fChunkFixBuff[0], CHUNK_FIXED_LEN) = 0) then begin
    repeat
      if (ChunkDetectFields(IsSave)) then begin
        Offs := self.Offset - CHUNK_FIXED_LEN;
        fResReason := ERR_NONE;
        Result := True;
        Exit;
      end;
      Move(fChunkFixBuff[1], fChunkFixBuff[0], CHUNK_FIXED_LEN - 1);
      if (ReadByte(fChunkFixBuff[CHUNK_FIXED_LEN - 1]) <> 0) then
        break;
    until (False);
  end;
  if (self.ErrorKind <> 0) then begin
    if (self.ErrorKind = FileIO.ERR_EOF) then
      fResReason := ERR_EOF
    else
      fResReason := ERR_IO;
  end;
end;

function THistFile.ChunkWrite(src: THistFile): Boolean;
var
  L: Cardinal;
begin
  Result := False;
  if (not src.fIsChunkLoaded) then begin
    fResReason := ERR_NOTLOADED;
    Exit;
  end;
  if (WriteDWord(src.fWhat) = 0) then begin
    case (src.fWhat) of
      Cardinal(HI_event): begin
        if (WriteByte(src.fEventType) = 0) then
          if (WriteDWord(src.fSenderUIN) = 0) then
            if (WriteDateTime(src.fEventTime) = 0) then
              if (WriteDWord(src.fExtraInfoLen) = 0) then
                if (WriteBlock(src.fExtraInfo^, src.fExtraInfoLen) = 0) then
                  if (WriteDWord(src.fBodyLen) = 0) then
                    if (WriteBlock(src.fBody[1], src.fBodyLen) = 0) then begin
                      fResReason := ERR_NONE;
                      Result := True;
                      Exit;
                    end;
        fResReason := ERR_IO;
      end;
      Cardinal(HI_hashed): begin
        L := 16; // MD5 has fixed length
        if (WriteDWord(L) = 0) then
          if (WriteBlock(src.fHashed[1], L) = 0) then begin
            fResReason := ERR_NONE;
            Result := True;
          end;
        fResReason := ERR_IO;
      end;
      Cardinal(HI_cryptMode): begin
        L := 1; // byte crypt-mode
        if (WriteDWord(L) = 0) then
          if (WriteByte(src.fCryptMode) = 0) then begin
            fResReason := ERR_NONE;
            Result := True;
          end;
        fResReason := ERR_IO;
      end
    end;
  end
  else
    fResReason := ERR_IO;
end;

constructor THistFile.Create(Path: string);
begin
  inherited Create(32768);
  fPath := Path;
  fOwnUIN := 0;
  fHistUIN := 0;
  fResReason := ERR_NONE;
  GetMem(fChunkFixBuff, 32);
  FillChar(fChunkFixBuff^, 32, 0);
  fFields := 0;
  fIsChunkLoaded := False;
  fChunkBegin := 0;
  fChunkEnd := 0;
  fErrorOffs := 0;
  fWhat := 0;
  fEventType := 0;
  fSenderUIN := 0;
  fEventTime := 0;
  GetMem(fExtraInfo, 32);
  FillChar(fExtraInfo^, 32, 0);
  fExtraInfoLen := 0;
  fBody := '';
  fBodyLen := 0;
  fHashed := '';
  fCryptMode := CRYPT_SIMPLE;
  fCryptPwd := '';
  fIsPwdCancel := False;
  fCryptKey := 0;
end;

destructor THistFile.Destroy;
begin
  FreeMem(fExtraInfo);
  FreeMem(fChunkFixBuff);
  inherited Destroy;
end;

function THistFile.getCryptPassword: Boolean;
var
  PasswHash, Hash: AnsiString;
  I: Integer;
begin
  Result := False;
  Hash := '';
  for I := 1 to 16 do
    Hash := Hash + AnsiString(IntToHex(Byte(fHashed[I]), 2));
  repeat
    PwdFrm.ShowModal;
    if (PwdFrm.Password <> '') then begin
      fCryptPwd := AnsiString(PwdFrm.Password);
      PasswHash := StrMD5(fCryptPwd);
      if (PasswHash = Hash) then begin
        if (fCryptMode = CRYPT_KEY1) then
          fCryptKey := Calculate_KEY1(fCryptPwd);
        Result := True;
        break;
      end
      else begin
        MessageBox(0, PChar('Введенный пароль неверен'), 'Ошибка', MB_ICONERROR or MB_OK);
        fCryptPwd := '';
      end;
    end
    else
      break;
  until (False);
end;

function THistFile.getDecriptedBody: string;
var
  S: AnsiString;
  CountUTF8, CountWin, CountUTF, CountUTFBE: Integer;
begin
  if (fBodyLen = 0) then begin
    Result := '';
    Exit;
  end;
  SetLength(S, fBodyLen);
  Move(fBody[1], S[1], fBodyLen);
  case (fCryptMode) of
    CRYPT_SIMPLE: Decritt(S, fSenderUIN);
    CRYPT_KEY1: begin
      if ((not fIsPwdCancel) and (fCryptPwd = '')) then begin
        if (not getCryptPassword) then
          fIsPwdCancel := True;
      end;
      if (fIsPwdCancel) then begin
        Result := 'нерасшифрованная строка';
        Exit;
      end;
      Decritt(S, fCryptKey);
    end;
  end;
  FastDetectCharset(S, CountUTF8, CountWin, CountUTF, CountUTFBE);
  if (CountUTF > 0) then begin
    Result := WideString(StrUTF2Ansi(S));
  end
  else if (CountUTFBE > 0) then begin
    Result := WideString(StrUTFBE2Ansi(S));
  end
  else begin
    if ((CountUTF8 > 0) and (CountUTF8 > CountWin)) then
      Result := UTF8ToWideString(RawByteString(S))
    else
      Result := WideString(S);
  end;
end;

function THistFile.getOwnUIN: string;
begin
  Result := IntToStr(fOwnUIN);
end;

function THistFile.getResReasonDesc: string;
begin
  case fResReason of
    ERR_NONE:
      Result := 'Без ошибок';
    ERR_EOF:
      Result := 'Конец файла';
    ERR_IO:
      Result := 'Ошибка ввода/вывода';
    ERR_NOTLOADED:
      Result := 'Запись не загружена';
    ERR_CHUNKNOTFOUND:
      Result := 'Запись не найдена';
    ERR_CHUNKCORRUPTED:
      Result := 'Запись испорчена';
    //ERR_WHAT:
    //  Result := 'Поле What повреждено';
    //ERR_EVENT:
    //  Result := 'Поле Event-Type повреждено';
    //ERR_UIN:
    //  Result := 'Поле UIN повреждено';
    //ERR_TIME:
    //  Result := 'Поле Event-Time повреждено';
    //ERR_LENEXTRA:
    //  Result := 'Поле длины Extra-Info повреждено';
    ERR_EXTRA:
      Result := 'Поле Extra-Info повреждено';
    ERR_LENBODY:
      Result := 'Поле длины Body повреждено или начало следующей записи испорчено';
  end;
end;

procedure THistFile.HistClose;
begin
  self.FileClose;
end;

function THistFile.HistCreate(UIN: string): Boolean;
begin
  Result := (self.FileCreate(Path + UIN + '.rep') = 0);
  if (Result) then begin
    try
      fHistUIN := StrToInt(UIN);
    except
      fHistUIN := 0;
    end;
  end;
end;

function THistFile.HistOpen(UIN: string): Boolean;
begin
  Result := (self.FileOpen(Path + UIN) = 0);
  if (Result) then begin
    try
      fHistUIN := StrToInt(UIN);
    except
      fHistUIN := 0;
    end;
  end;
end;

procedure THistFile.setOwnUIN(UIN: string);
begin
  try
    fOwnUIN := StrToInt(UIN);
  except
    fOwnUIN := 0;
  end;
end;

end.
