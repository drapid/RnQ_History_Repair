/////////////////////////////////////////////////////////
// This file is part of RQ_Pepair.
// &RQ - Copyright (c) Massimo Melina (www.rejetto.com)
// R&Q - Copyright (c) Rapid D., RnQ team (www.rnq.ru)
// RnQ_Repair - Copyright (c) C6 Lab (c6lab.spb.ru)

unit FileIO;

interface

uses
  Windows;

const
  ERR_CREATE = 01;
  ERR_OPEN = 02;
  ERR_READ = 03;
  ERR_WRITE = 04;
  ERR_EOF = 05;
  ERR_OVERBUFFER = 06;

type

  TFileIO = class (TObject)
  private
    fHandle: Cardinal;
    fBuffer: PByte;
    fBuffOffs: Cardinal;
    fBuffSize: Word;
    fBuffCount: Word;
    fBuffIndex: Word;
    fIsWriteMode: Boolean;
    fIsEof: Boolean;
    fFileOffset: Cardinal;
    fErrorKind: Integer;
    fErrorCode: Cardinal;
  protected
    function _ReadBuffered(var Buffer; Count: Word): Integer;
    function _WriteBuffered(var Buffer; Count: Word): Integer;
    function _WriteFlush: Integer;
    procedure _FileSeekAbs(Offset: Cardinal);
    //function _IsEof: Boolean;
    function getFileSize: Cardinal;
  public
    constructor Create(uBufferSize: Word);
    destructor Destroy; override;
    function FileOpen(FileName: string): Integer;
    function FileCreate(FileName: string): Integer;
    procedure FileClose;
    //function FileSeekRel(Offset: Integer): Cardinal;
    function ReadBlock(var Buffer; Count: Word): Integer;
    function ReadByte(var B: Byte): Integer;
    function ReadWord(var W: Word): Integer;
    function ReadDWord(var DW: Cardinal): Integer;
    function ReadLn(var Str: string): Integer;
    function WriteBlock(var Buffer; Count: Word): Integer;
    function WriteByte(var B: Byte): Integer;
    function WriteDWord(var DW: Cardinal): Integer;
    function WriteDateTime(var DT: TDateTime): Integer;
    function WriteLn(Str: string): Integer;
    procedure FileFlushBuffers;
    function GetOffsetEof: Cardinal;
    property Offset: Cardinal read fFileOffset write _FileSeekAbs;
    //property Eof: boolean read _IsEof;
    property FileSize: Cardinal read getFileSize;
    property ErrorKind: Integer read fErrorKind;
    property ErrorCode: Cardinal read fErrorCode;
  end;

implementation

uses
  SysUtils;

{ TFileIO }

constructor TFileIO.Create(uBufferSize: Word);
begin
  inherited Create;
  if (uBufferSize > 0) then begin
    fBuffSize := uBufferSize;
  end
  else
    // default
    fBuffSize := 8192;
  GetMem(fBuffer, fBuffSize);
  FillChar(fBuffer^, fBuffSize, 0);
  fBuffOffs := 0;
  fBuffCount := 0;
  fBuffIndex := 0;
  fHandle := INVALID_HANDLE_VALUE;
  fFileOffset := 0;
  fIsWriteMode := False;
  fIsEof := False;
  fErrorKind := 0;
  fErrorCode := 0;
end;

destructor TFileIO.Destroy;
begin
  FileClose;
  FreeMem(fBuffer);
  inherited Destroy;
end;

procedure TFileIO.FileClose;
begin
  if (fHandle <> INVALID_HANDLE_VALUE) then begin
    if (fIsWriteMode) then
      _WriteFlush;
    Windows.CloseHandle(THandle(fHandle));
    fHandle := INVALID_HANDLE_VALUE;
    fBuffOffs := 0;
    fBuffCount := 0;
    fBuffIndex := 0;
    fFileOffset := 0;
    fIsWriteMode := False;
    fIsEof := False;
    fErrorKind := 0;
    fErrorCode := 0;
  end;
end;

function TFileIO.FileCreate(FileName: string): Integer;
begin
  Result := 0;
  FileClose;
  fHandle := Windows.CreateFile(PChar(FileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if (fHandle = INVALID_HANDLE_VALUE) then begin
    fErrorKind := ERR_CREATE;
    fErrorCode := Windows.GetLastError;
    Result := fErrorKind;
  end
  else
    fIsWriteMode := True;
    fErrorKind := 0;
    fErrorCode := 0;
end;

procedure TFileIO.FileFlushBuffers;
begin
  if (fIsWriteMode) then
    _WriteFlush;
end;

function TFileIO.FileOpen(FileName: string): Integer;
begin
  Result := 0;
  FileClose;
  fHandle := Windows.CreateFile(PChar(FileName), GENERIC_READ, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (fHandle = INVALID_HANDLE_VALUE) then begin
    fErrorKind := ERR_OPEN;
    fErrorCode := Windows.GetLastError;
    Result := fErrorKind;
  end
  else begin
    fErrorKind := 0;
    fErrorCode := 0;
  end;
end;

function TFileIO.getFileSize: Cardinal;
begin
  Result := Windows.GetFileSize(fHandle, nil);
end;

//function TFileIO.FileSeekRel(Offset: Integer): Cardinal;
//begin
//  fFileOffset := Windows.SetFilePointer(fHandle, Integer(fFileOffset) + Offset, nil, FILE_BEGIN);
//  if (fFileOffset < fBuffOffs) or (fFileOffset >= (fBuffOffs + fBuffCount)) then begin
//    fBuffOffs := fFileOffset;
//    fBuffCount := 0;
//    fBuffIndex := 0;
//  end
//  else begin
//    fBuffIndex := fFileOffset - fBuffOffs;
//  end;
//  Result := fFileOffset;
//end;

function TFileIO.GetOffsetEof: Cardinal;
var
  P1: Cardinal;
begin
  P1 := Windows.SetFilePointer(fHandle, 0, nil, FILE_CURRENT);
  Result := Windows.SetFilePointer(fHandle, 0, nil, FILE_END);
  Windows.SetFilePointer(fHandle, P1, nil, FILE_BEGIN);
end;

procedure TFileIO._FileSeekAbs(Offset: Cardinal);
begin
  fFileOffset := Windows.SetFilePointer(fHandle, Offset, nil, FILE_BEGIN);
  if (fFileOffset < fBuffOffs) or (fFileOffset >= (fBuffOffs + fBuffCount)) then begin
    fBuffOffs := fFileOffset;
    fBuffCount := 0;
    fBuffIndex := 0;
  end
  else begin
    fBuffIndex := fFileOffset - fBuffOffs;
  end;
  fIsEof := False;
  fErrorKind := 0;
  fErrorCode := 0;
end;

//function TFileIO._IsEof: Boolean;
//begin
//  Result := ((fBuffCount = 0) and fIsEof);
//end;

function TFileIO.ReadBlock(var Buffer; Count: Word): Integer;
begin
  if (Count = 0) then begin
    Result := 0;
    Exit;
  end;
  Result := _ReadBuffered(Buffer, Count);
end;

function TFileIO.ReadByte(var B: Byte): Integer;
begin
  Result := _ReadBuffered(B, 1);
end;

function TFileIO.ReadWord(var W: Word): Integer;
begin
  Result := _ReadBuffered(W, 2);
end;

function TFileIO.ReadDWord(var DW: Cardinal): Integer;
begin
  Result := _ReadBuffered(DW, 4);
end;

function TFileIO.ReadLn(var Str: string): Integer;
var
  S: AnsiString;
  B: Byte;
  C: Integer;
begin
  C := 0;
  SetLength(S, 255);
  repeat
    Result := _ReadBuffered(B, 1);
    if (Result <> 0) then
      break;
    if (B = $0D) then begin
      if (Result = 0) then
        Result := _ReadBuffered(B, 1);
      break;
    end;
    Inc(C);
    S[C] := AnsiChar(B);
  until (C = 254);
  S[C + 1] := #0;
  SetLength(S, C + 1);
  Str := WideString(S);
end;

function TFileIO.WriteBlock(var Buffer; Count: Word): Integer;
begin
  if (Count = 0) then begin
    Result := 0;
    Exit;
  end;
  Result := _WriteBuffered(Buffer, Count);
end;

function TFileIO.WriteByte(var B: Byte): Integer;
begin
  Result := _WriteBuffered(B, 1);
end;

function TFileIO.WriteDateTime(var DT: TDateTime): Integer;
begin
  Result := _WriteBuffered(DT, 8);
end;

function TFileIO.WriteDWord(var DW: Cardinal): Integer;
begin
  Result := _WriteBuffered(DW, 4);
end;

function TFileIO.WriteLn(Str: string): Integer;
var
  Str2: AnsiString;
begin
  Str2 := AnsiString(Str) + #$0D#$0A;
  Result := _WriteBuffered(Str2[1], Length(Str2));
end;

function TFileIO._ReadBuffered(var Buffer; Count: Word): Integer;
var
  lBytesRead: LongWord;
  C1, C2: Word;
begin
  if (fBuffSize < Count) then begin
    fErrorKind := ERR_OVERBUFFER;
    fErrorCode := 0;
    Result := fErrorKind;
    Exit;
  end;
  Result := 0;
  fErrorKind := 0;
  fErrorCode := 0;
  C1 := fBuffCount - fBuffIndex;
  if (C1 >= Count) then begin
    Move(fBuffer[fBuffIndex], Buffer, Count);
    Inc(fBuffIndex, Count);
    Inc(fFileOffset, Count);
  end
  else begin
    if (fIsEof) then begin
      fErrorKind := ERR_EOF;
      Result := fErrorKind;
      Exit;
    end;
    C2 := fBuffSize - C1;
    if (C1 > 0) then
      Move(fBuffer[fBuffIndex], fBuffer[0], C1);
    Inc(fBuffOffs, fBuffIndex);
    fBuffCount := C1;
    fBuffIndex := 0;
    fFileOffset := fBuffOffs + fBuffIndex;
    Windows.SetFilePointer(fHandle, fBuffOffs + C1, nil, FILE_BEGIN);
    while (fBuffCount < fBuffSize) do begin
      if (ReadFile(THandle(fHandle), fBuffer[C1], C2, lBytesRead, nil)) then begin
        if (lBytesRead > 0) then begin
          Inc(C1, lBytesRead);
          Dec(C2, lBytesRead);
          Inc(fBuffCount, lBytesRead);
        end
        else begin
          fIsEof := True;
          break;
        end;
      end
      else begin
        fErrorKind := ERR_READ;
        fErrorCode := Windows.GetLastError;
        Result := fErrorKind;
        break;
      end;
    end;
    if (fBuffCount >= Count) then begin
      Move(fBuffer[fBuffIndex], Buffer, Count);
      Inc(fBuffIndex, Count);
      Inc(fFileOffset, Count);
    end
    else begin
      if (fIsEof) then begin
        fErrorKind := ERR_EOF;
        Result := fErrorKind;
      end
      else begin
      end;
    end;
  end;
end;

function TFileIO._WriteBuffered(var Buffer; Count: Word): Integer;
var
  lBytesWritten: LongWord;
  C1: Word;
begin
  if (fBuffSize < Count) then begin
    fErrorKind := ERR_OVERBUFFER;
    fErrorCode := 0;
    Result := fErrorKind;
    Exit;
  end;
  Result := 0;
  fErrorKind := 0;
  fErrorCode := 0;
  C1 := fBuffSize - fBuffCount;
  if (Count <= C1) then begin
    Move(Buffer, fBuffer[fBuffCount], Count);
    Inc(fBuffCount, Count);
    Inc(fFileOffset, Count);
  end
  else begin
    C1 := 0;
    while (fBuffCount > 0) do begin
      if (Windows.WriteFile(THandle(fHandle), fBuffer[C1], fBuffCount, lBytesWritten, nil)) then begin
        Inc(C1, lBytesWritten);
        Dec(fBuffCount, lBytesWritten);
      end
      else begin
        fErrorKind := ERR_WRITE;
        fErrorCode := Windows.GetLastError;
        Result := fErrorKind;
        Exit;
      end;
    end;
    // Assert (fBuffCount = 0)
    fBuffOffs := fFileOffset;
    Move(Buffer, fBuffer[0], Count);
    fBuffCount := Count;
    Inc(fFileOffset, Count);
  end;
end;

function TFileIO._WriteFlush: Integer;
var
  lBytesWritten: LongWord;
  C1: Word;
begin
  Result := 0;
  fErrorKind := 0;
  fErrorCode := 0;
  C1 := 0;
  while (fBuffCount > 0) do begin
    if (Windows.WriteFile(THandle(fHandle), fBuffer[C1], fBuffCount, lBytesWritten, nil)) then begin
      Windows.FlushFileBuffers(fHandle);
      Inc(C1, lBytesWritten);
      Dec(fBuffCount, lBytesWritten);
    end
    else begin
      fErrorKind := ERR_WRITE;
      fErrorCode := Windows.GetLastError;
      Result := fErrorKind;
      Exit;
    end;
  end;
  // Assert (fBuffCount = 0)
  fBuffOffs := fFileOffset;
end;

end.
