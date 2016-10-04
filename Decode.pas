/////////////////////////////////////////////////////////
// This file is part of RnQ_Pepair.
// &RQ - Copyright (c) Massimo Melina (www.rejetto.com)
// R&Q - Copyright (c) Rapid D., RnQ team (www.rnq.ru)
// RnQ_Repair - Copyright (c) C6 Lab (c6lab.spb.ru)

unit Decode;

interface

procedure Decritt(var S: AnsiString; Key: Integer);
function Calculate_KEY1(Pwd: AnsiString): Integer;
function StrUTF2Ansi(S: AnsiString): AnsiString;
function StrUTFBE2Ansi(S: AnsiString): AnsiString;
procedure FastDetectCharset(S: AnsiString; var CountUTF8, CountWin, CountUTF, CountUTFBE: Integer);

implementation

procedure Decritt(var S: AnsiString; Key: Integer);
begin
  asm
    MOV ECX, Key
    MOV DL, CL
    SHR ECX, 20
    MOV DH, CL

    MOV ESI, S
    MOV ESI, [ESI]
    OR  ESI, ESI
    JZ  @OUT

    MOV AH, 10111000b

    MOV ECX, [ESI - 4]
    OR  ECX, ECX
    JZ  @OUT
@IN:
    MOV AL, [ESI]
    XOR AL, AH
    ROL AL, 3
    XOR AL, DH
    SUB AL, DL

    MOV [ESI], AL
    INC ESI
    ROR AH, 3
    DEC ECX
    JNZ @IN
@OUT:
  end;
end;

function Calculate_KEY1(Pwd: AnsiString): Integer;
var
  I, L: Integer;
  P: ^Integer;
begin
  L := Length(Pwd);
  Result := L shl 16;
  P := nil;
  if (Pwd > '')
    then P := @Pwd[1];
  I := 0;
  while (I + 4 < L) do begin
    Inc(Result, P^);
    Inc(P);
    Inc(I, 4);
  end;
  while (I < L) do begin
    Inc(Result, Ord(Pwd[I]));
    Inc(I);
  end;
end;

function StrUTF2Ansi(S: AnsiString): AnsiString;
begin
  SetLength(Result, Length(S) div 2);
  asm
    PUSH  ESI
    PUSH  EDI
    PUSH  EDX
    PUSH  ECX
    PUSH  EAX

    MOV   ESI, S
    OR    ESI, ESI
    JZ    @OUT
    MOV   EAX, [ESI-4]
    OR    EAX, EAX
    JZ    @OUT
    MOV   ECX, 2
    XOR   EDX, EDX
    DIV   ECX
    MOV   ECX, EAX
    MOV   EDI, Result
    MOV   EDI, [EDI]
@IN:
    LODSW
    CMP   AH, 04h
    JNZ   @PUT
    CMP   AL, 10h
    JB    @PUT
    CMP   AL, 4Fh
    JA    @PUT
    ADD   AL, 0B0h
@PUT:
    STOSB
    LOOP  @IN
    XOR   EAX, EAX
    STOSB
@OUT:
    POP   EAX
    POP   ECX
    POP   EDX
    POP   EDI
    POP   ESI
  end;
end;

function StrUTFBE2Ansi(S: AnsiString): AnsiString;
begin
  SetLength(Result, Length(S) div 2);
  asm
    PUSH  ESI
    PUSH  EDI
    PUSH  EDX
    PUSH  ECX
    PUSH  EAX

    MOV   ESI, S
    OR    ESI, ESI
    JZ    @OUT
    MOV   EAX, [ESI-4]
    OR    EAX, EAX
    JZ    @OUT
    MOV   ECX, 2
    XOR   EDX, EDX
    DIV   ECX
    MOV   ECX, EAX
    MOV   EDI, Result
    MOV   EDI, [EDI]
@IN:
    LODSW
    CMP   AL, 04h
    JNZ   @PUT
    CMP   AH, 10h
    JB    @PUT
    CMP   AH, 4Fh
    JA    @PUT
    ADD   AH, 0B0h
@PUT:
    MOV   AL, AH
    STOSB
    LOOP  @IN
    XOR   EAX, EAX
    STOSB
@OUT:
    POP   EAX
    POP   ECX
    POP   EDX
    POP   EDI
    POP   ESI
  end;
end;

procedure FastDetectCharset(S: AnsiString; var CountUTF8, CountWin, CountUTF, CountUTFBE: Integer);
begin
  asm
    PUSH  ESI
    PUSH  EBX
    PUSH  ECX
    PUSH  EDX
    PUSH  EAX

    MOV   ESI, S
    OR    ESI, ESI
    JZ    @OUT
    MOV   ECX, [ESI-4]
    OR    ECX, ECX
    JZ    @OUT
    XOR   EBX, EBX
    XOR   EDX, EDX
@IN:
    XOR   EAX, EAX
    LODSB

    CMP   AL, 0D0h
    JNZ   @J1
    INC   BL
    JMP   @LO
@J1:
    CMP   AL, 0D1h
    JNZ   @J2
    INC   BL
    JMP   @LO
@J2:
    CMP   AL, 0E0h
    JNZ   @J3
    INC   BH
    JMP   @LO
@J3:
    CMP   AL, 0EEh
    JNZ   @J4
    INC   BH
    JMP   @LO
@J4:
    CMP   AL, 0CEh
    JNZ   @J5
    INC   BH
    JMP   @LO
@J5:
    MOV   AH, CL
    AND   AH, 01h
    JZ    @J6
    CMP   AL, 04h
    JNZ   @LO
    INC   DL
    JMP   @LO
@J6:
    CMP   AL, 04h
    JNZ   @LO
    INC   DH
@LO:
    LOOP  @IN
@OUT:
    XOR   EAX, EAX
    MOV   AL, BL
    MOV   ESI, [CountUTF8]
    MOV   [ESI], EAX
    MOV   AL, BH
    MOV   ESI, [CountWin]
    MOV   [ESI], EAX
    MOV   AL, DL
    MOV   ESI, [CountUTF]
    MOV   [ESI], EAX
    MOV   AL, DH
    MOV   ESI, [CountUTFBE]
    MOV   [ESI], EAX

    POP   EAX
    POP   EDX
    POP   ECX
    POP   EBX
    POP   ESI
  end;
end;

end.
