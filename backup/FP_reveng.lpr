program FP_reveng;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  StrUtils,
  TypInfo,
  crt;

type

  Tinstruction = (_unknown, _nop, _MOV, _XOR, _TEST, _PUSH, _SUB, _CDQ, _IDIV,
    _ADD, _CMP, _JG, _POP, _retn, _LEA, _CALL, _JNZ, _JMP, _JLE,
    _MOVZX, _OR, _JNS, _MOVSD, _XORPS, _MOVDQA, _INC, _IMUL, _JS, _SUBSB, _SUBSD,
    _NEG, _SBB, _AND, _MULSD, _JZ, _MOVUPS, _MOVAPS, _SHL, _SETNZ, _SETB,
    _UNPCKLPD, _DIVSD, _MOVSS, _CVTPS2PD, _ADDSD, _UCOMISD, _JA, _MOVSXD,
    _SETZ, _CMOVNZ, _MOVDQU, _JGE, _NOT, _DIV, _LOCK, _CMOVS, _XCHG, _ANDPS,
    _SQRTPD, _CMOVZ, _JL, _JB, _JNB, _JBE, _DEC, _INT, _BT, _BTS, _CMOVG, _CMOVA,
    _MOVD, _CVTDQ2PD, _CVTTSD2SI, _CMOVL, _SHR, _CMOVB, _DIVPD, _SETNB, _CQO,
    _SAR, _CDQE, _CMOVNS, _MUL, _CMOVO, _CMOVBE, _CMOVLE, _CMOVGE,
    _CVTDQ2PS, _MULSS, _CVTTSS2SI, _ADDPD, _ADDPS, _ADDSS, _ANDNPS,
    _BSR, _BTC, _BTR, _CMOVNB, _CMPLTPD, _COMISD, _COMISS, _CPUID,
    _CVTPD2PS, _CVTSD2SS, _CVTSI2SD, _CVTSI2SS, _CVTSS2SD, _CVTTPD2DQ,
    _CWDE, _DD, _DIVSS, _DQ, _JNP, _JP, _MAXPD, _MAXSD, _MAXSS, _MINPD,
    _MINSD, _MINSS, _MOVHPD, _MOVLHPS, _MOVLPD, _MOVQ, _MOVSX, _MOVUPD,
    _MULPD, _MULPS, _ORPS, _PADDD, _PAND, _PANDN, _PCMPEQB, _PCMPEQD,
    _PCMPEQQ, _PCMPGTD, _PMAXSD, _PMAXUB, _PMINSD, _PMINUB, _POR, _PSHUFD,
    _PSRLDQ, _PSUBD, _PUNPCKLBW, _PUNPCKLDQ, _PUNPCKLQDQ, _PXOR, _REP,
    _ROL, _ROR, _ROUNDPD, _SETBE, _SETL, _SETLE, _SETNBE, _SETNL, _SETNLE,
    _SETNS, _SETS, _SHUFPS, _SQRTSD, _SQRTSS, _SUBPD, _SUBSS, _UCOMISS,
    _UNPCKHPD, _UNPCKLPS, _VMOVDQU, _VPERMQ, _VZEROUPPER, _XGETBV);

  TRegister = (R_unknown, R_rax, R_al, R_rbx, R_ecx, R_rdx, R_eax, R_rdi, R_r8, R_r9d, R_rsi, R_r8b, R_sil, R_r15d, R_r12d, R_r13d, R_r14, R_avd,
    R_rcx, R_ax, R_xmm0, R_xmm1, R_xmm2, R_xmm3, R_esi, R_dl, R_rsp, R_edx, R_r8d, R_ebx, R_r9, R_edi, R_ebp, R_r10d, R_r10, R_rbp, R_bpl,
    R_cx, R_r13, R_r12, R_r15, R_r14d, R_dil, R_bl, R_cl, R_r11, R_r11d, R_r9w, R_r9b, R_r8w, R_r10b, R_dx, R_di, R_xmm12, R_xmm14,
    R_si, R_bx, R_r14w, R_r12b, R_r14b, R_r13b, R_r15b, R_xmm5, R_xmm4, R_r11w, R_r11b, R_xmm13, R_xmm11, R_cmpxchg, R_xmm15,
    R_bp, R_xmm6, R_xmm7, R_xmm9, R_xmm8, R_r15w, R_r12w, R_r13w, R_xmm10, R_rva, R_r10w, R_ymm4, R_ymm1, R_ymm2, R_ymm0);

  TOperatorKind = (O_pointer, O_register, O_ex, O_csPointer, O_loc, O_expression, O_procAddress, O_dwordPointer, O_decimal,
    O_qwordPointer, O_ss_rva, O_def, O_questionMark, O_API, O_CAPI, O_opkw, O_ProcName);

  TOperatorKeyWord = (OPKW_unknown, OPKW_ConsoleEventHandler, OPKW_ConvertCAPItoCOM, OPKW_ConvertCOMtoCAPI, OPKW_ME_DATA_EPS,
    OPKW_short, OPKW_inc, OPKW_xadd, OPKW_ymmword, OPKW_byte, OPKW_qword, OPKW_word, OPKW_dword, OPKW_and, OPKW_xmmword, OPKW_offset,
    OPKW_ptr, OPKW_dec);


  TOperator = record
    p: int64;
    asWritten: string;
    Ftype: TOperatorKind;
    Register: TRegister;
    keyword: TOperatorKeyWord;
  end;

  TProcedureName = class
  private
    Name: string;
    constructor Create(TheName: string);

  end;

  TRegisterList = class
  private
    l: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function get(RegisterString: string): TRegister;
    function isRegister(RegisterString: string): boolean;
  end;

  TinstructionList = class
  private
    l: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function get(InstructionString: string): Tinstruction;
  end;

  TOperatorKeyWordList = class
  private
    l: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function get(RegisterString: string): TOperatorKeyWord;
    function isRegister(RegisterString: string): boolean;
  end;


  TProcedureNameList = class
  private
    l: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure add(procName: string);
    function get(procName: string): TProcedureName;
    function isProcedure(procName: string): boolean;
  end;


  TLineType = (LT_none, LT_comment, LT_instruction, LT_procBegin, LT_procEnd, LT_error, LT_label, LT_parDeclaration, LT_memorySpace,
    LT_GlobalVarByte, LT_publicProcName, LT_align);

  TreadingState = (RS_other, RS_procedure, RS_dataStructure);

  // an asm instruction has the form <instruction>  a, b where a, b are pointers or registers

  { TASM_line }

  TASM_line = class
  public
    line: string;
    tokens: TStringArray;
    lineNum: integer;
    comment: string;
    instruction: Tinstruction;
    operators: array of TOperator;
    a, b, c, d: TOperator;
    Name: string;
    lineType: TLineType;
    parValue: string;
    address: string;
    constructor Create(Aline: string);
    destructor Destroy; override;
    function decodeStartProcedure: boolean;
    function decodeStartDataBlock: boolean;
    function decodeEndProcedure: boolean;
    function decodeInstruction: boolean;
    function decodeLabel: boolean;
    function decodeParDeclaration: boolean;
    function decodeMemorySpace: boolean;
    function decodeGlobalVarByte: boolean;
    function decodePublicProcName: boolean;
    function decodeAlign: boolean;
    //procedure fromStringToOperator(s: string; var op: TOperator);
  end;

  { TASMfile }

  TASMfile = class
  public
    Lines: array of TASM_line;
    constructor Create;
    destructor Destroy; override;
    procedure setFile(asmFileName: string);
    procedure decodeLines(fileName: string);
  end;

  { TFP_reveng }

  TFP_reveng = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    FasmFile: TASMfile;
    FasmFileName: string;
    currLineNum, colNum: integer;
    currLine: string;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure HandleException(Sender: TObject); override;
  end;

  TCRTprogressBar = class
  public
    maxReal: integer;
    currReal, delta, Next: integer;
    y, x: integer;
    procedure start(len: integer);
    procedure update(i: integer);
  end;


var
  Application: TFP_reveng;
  progressBar: TCRTprogressBar;
  mancanti: TStringList;
  instructionNamesList: TinstructionList;
  RegisterList: TRegisterList;
  OPkeywordList: TOperatorKeyWordList;
  ProcedureList: TProcedureNameList;


  function tokenize(s: string): TStringArray;
  var
    i, j, n, nt, roundBraketLevel, squareBraketLevel: integer;
    s1: string;
  begin
    nt := 0;
    s1 := '';
    roundBraketLevel := 0;
    squareBraketLevel := 0;
    n := length(s);
    SetLength(Result, nt);
    for i := 1 to n do
    begin
      case s[i] of
        ' ':
        begin
          //the spaces in the beginning of the token are ignored
          if s1 = '' then continue;
          //everything inside the brakets is part of the token
          if (roundBraketLevel > 0) or (squareBraketLevel > 0) then
          begin
            s1 := s1 + ' ';
            continue;
          end;
          //it's the end of the token
          Inc(nt);
          SetLength(Result, nt);
          Result[nt - 1] := trim(s1);
          s1 := '';
          roundBraketLevel := 0;
          squareBraketLevel := 0;
        end;
        '[':
        begin
          Inc(squareBraketLevel);
          s1 := s1 + '[';
        end;
        '(':
        begin
          Inc(roundBraketLevel);
          s1 := s1 + '(';
        end;
        ']':
        begin
          Dec(squareBraketLevel);
          s1 := s1 + ']';
        end;
        ')':
        begin
          Dec(roundBraketLevel);
          s1 := s1 + ')';
        end;
        else
          s1 := s1 + s[i];
      end; //case
      if i >= n then
      begin
        if s1 <> '' then
        begin
          Inc(nt);
          SetLength(Result, nt);
          Result[nt - 1] := trim(s1);
        end;
        exit;
      end;
    end;
  end;

  function MayBeIsEx(s: string): boolean;
  var
    i: integer;
    c: char;
  begin
    if s[length(s)] <> 'h' then
    begin
      Result := False;
      exit;
    end;
    s := copy(s, 1, length(s) - 1);
    Result := True;
    for i := 1 to Length(s) do
    begin
      c := s[i];
      if not (c in ['0'..'9', 'A'..'F', 'a'..'f']) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;


  function MayBeIsDecimal(s: string): boolean;
  var
    i: integer;
    c: char;
  begin
    Result := True;
    for i := 1 to Length(s) do
    begin
      c := s[i];
      if not (c in ['0'..'9']) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

  function OnlyLetters(const s: string): boolean;
  var
    i: integer;
    c: char;
  begin
    Result := True;
    for i := 1 to Length(s) do
    begin
      c := s[i];
      if not (c in ['A'..'Z', 'a'..'z']) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

  function startWith(const s: string; start: string): boolean;
  begin
    Result := pos(start, s) = 1;
  end;

  function insideSquareBrakets(s: string): boolean;
  begin
    Result := (pos('[', s) = 1) and (pos(']', s) = length(s));
  end;

  procedure raiseError(errorNum, lineNum, ColomnNum: integer; line: string); overload;
  var
    tokens: TStringArray;
    desc: string;
    i: integer;
  begin
    case errorNum of
      1:
        desc := format('Line %d is not valid. Expected the start of procedure or the start of a data block.', [lineNum]);
      2:
      begin
        desc := format('ERROR in line %d,  _unknown instruction. Expected the end of procedure or a valid instruction.', [lineNum]);
        tokens := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
        //     mancanti.add(',_' + UpperCase(tokens[0]));
        //   mancanti.add(' else if s = ' + tokens[0] + ' then instruction := _' + UpperCase(tokens[0]));
      end;
      3:
        desc := format('ERROR in line %d,  _unknown register. Expected valid register or pointer.', [lineNum]);
    end;
    //writeln('ERROR during elaboration : errNo:', errorNum, ' : ', desc);
    //writeln('   Line number ', lineNum);
    //writeln('   Position in the line ', ColomnNum);
    //writeln(line);
    //for i := 1 to ColomnNum do Write('_');
    //writeln('|__');
    //writeln('press a key to terminate');
    //    readln;
    //halt(errorNum);
  end;

  procedure raiseError(errorNum, lineNum, ColomnNum: integer; line: string; tokens: TStringArray; tokenNum: integer); overload;
  var
    i, k: integer;
    desc: string;
  begin
    if ColomnNum = 0 then
    begin
      if tokenNum >= length(tokens) then k := length(tokens) - 1
      else
        k := tokenNum;
      for i := 0 to k do
        ColomnNum := ColomnNum + length(tokens[i]);
    end;
    case errorNum of
      1, 2:
        raiseError(errorNum, lineNum, ColomnNum, line);
      3:
        desc := format('ERROR in line %d,  _unknown register. Expected valid register or pointer.', [lineNum]);
      //   mancanti.add('token = ' + tokens[tokenNum]);

    end;
    //     mancanti.add(' else if s = ' + tokens[0] + ' then instruction := _' + UpperCase(tokens[0]));

    mancanti.add(tokens[tokenNum]);
    //writeln('ERROR during elaboration : errNo:', errorNum, ' : ', desc);
    //writeln('   Line number ', lineNum);
    //writeln('   Position in the line ', ColomnNum);
    //if tokenNum <> 0 then  writeln('error in token ', tokenNum, ' : ', tokens[tokenNum]);
    //writeln;
    //writeln(line);
    //for i := 1 to ColomnNum do Write('_');
    //writeln('|__');
    //   writeln('press a key to terminate');
    //  readln;
    //halt(errorNum);
  end;

  constructor TProcedureNameList.Create;
  var
    s: string;
  begin
    l := TStringList.Create;
    l.sorted := True;
    l.Duplicates := dupIgnore;
  end;

  destructor TProcedureNameList.Destroy;
  begin
    l.Free;
    inherited Destroy;
  end;

  procedure TProcedureNameList.add(procName: string);
  var
    prc: TProcedureName;
  begin
    if isProcedure(procName) then exit;
    prc := TProcedureName.Create(procName);
    l.AddObject(procName, prc);
  end;

  function TProcedureNameList.get(procName: string): TProcedureName;
  var
    index: integer;
  begin
    Index := l.IndexOf(procName);
    if Index <> -1 then
      Result := TProcedureName(l.Objects[Index])
    else
      Result := nil;
  end;

  function TProcedureNameList.isProcedure(procName: string): boolean;
  begin
    Result := l.IndexOf(procName) >= 0;
  end;

  constructor TProcedureName.Create(TheName: string);
  begin
    Name := TheName;
  end;

  constructor TOperatorKeyWordList.Create;
  var
    istr: TOperatorKeyWord;
    s: string;
  begin
    l := TStringList.Create;
    l.sorted := True;
    l.Duplicates := dupIgnore;
    for istr := Low(TOperatorKeyWord) to High(TOperatorKeyWord) do
    begin
      s := GetEnumName(TypeInfo(TOperatorKeyWord), Ord(istr));
      s := Copy(s, 6, MaxInt);
      s := LowerCase(s);
      l.add(s);
    end;

  end;

  destructor TOperatorKeyWordList.Destroy;
  begin
    l.Free;
    inherited Destroy;
  end;

  function TOperatorKeyWordList.get(RegisterString: string): TOperatorKeyWord;
  var
    enumValue: integer;
  begin
    if l.IndexOf(RegisterString) >= 0 then
    begin
      //    Result := Tinstruction(l.Objects[i]);
      EnumValue := GetEnumValue(TypeInfo(TOperatorKeyWord), 'OPKW_' + RegisterString);
      if EnumValue = -1 then
        raise Exception.CreateFmt('Invalid TOperatorKeyWord value: %s', [RegisterString]);
      Result := TOperatorKeyWord(EnumValue);
    end
    else
      Result := OPKW_unknown;

  end;

  function TOperatorKeyWordList.isRegister(RegisterString: string): boolean;
  begin
    Result := l.IndexOf(RegisterString) >= 0;
  end;

  constructor TRegisterList.Create;
  var
    istr: TRegister;
    s: string;
  begin
    l := TStringList.Create;
    l.sorted := True;
    l.Duplicates := dupIgnore;
    for istr := Low(TRegister) to High(TRegister) do
    begin
      s := GetEnumName(TypeInfo(TRegister), Ord(istr));
      s := Copy(s, 3, MaxInt);
      s := LowerCase(s);
      l.add(s);
    end;
  end;

  destructor TRegisterList.Destroy;
  begin
    l.Free;
    inherited Destroy;
  end;

  function TRegisterList.get(RegisterString: string): TRegister;
  var
    enumValue: integer;
  begin
    if l.IndexOf(RegisterString) >= 0 then
    begin
      //    Result := Tinstruction(l.Objects[i]);
      EnumValue := GetEnumValue(TypeInfo(TRegister), 'R_' + RegisterString);
      if EnumValue = -1 then
        raise Exception.CreateFmt('Invalid enum value: %s', [RegisterString]);
      Result := TRegister(EnumValue);
    end
    else
      Result := R_unknown;
  end;

  function TRegisterList.isRegister(RegisterString: string): boolean;
  begin
    Result := l.IndexOf(RegisterString) >= 0;
  end;

  constructor TinstructionList.Create;
  var
    istr: Tinstruction;
    s: string;
  begin
    l := TStringList.Create;
    l.sorted := True;
    l.Duplicates := dupIgnore;
    for istr := Low(Tinstruction) to High(Tinstruction) do
    begin
      s := GetEnumName(TypeInfo(Tinstruction), Ord(istr));
      s := Copy(s, 2, MaxInt);
      s := LowerCase(s);
      l.add(s);
    end;
  end;

  destructor TinstructionList.Destroy;
  begin
    l.Free;
    inherited Destroy;
  end;

  function TinstructionList.get(InstructionString: string): Tinstruction;
  var
    enumValue: integer;
  begin
    if l.IndexOf(InstructionString) >= 0 then
    begin
      //    Result := Tinstruction(l.Objects[i]);
      EnumValue := GetEnumValue(TypeInfo(Tinstruction), '_' + InstructionString);
      if EnumValue = -1 then
        raise Exception.CreateFmt('Invalid enum value: %s', [InstructionString]);
      Result := Tinstruction(EnumValue);
    end
    else
      Result := _unknown;
  end;

  procedure TCRTprogressBar.start(len: integer);
  begin
    MaxReal := len;
    currReal := 0;
    y := crt.WhereY + 1;
    x := 1;
    crt.gotoXY(x, y);
    writeln(';');
    delta := len div 100;
    Next := delta;
  end;

  procedure TCRTprogressBar.update(i: integer);
  begin
    if i < Next then exit;
    Inc(x);
    crt.gotoXY(x, y);
    writeln('-');
    Next := Next + delta;
    if i >= maxReal then
    begin
      crt.gotoXY(100, y);
      writeln(';  OK');
    end;
  end;

  { TFP_reveng }


  constructor TASM_line.Create(Aline: string);
  var
    i: integer;
    s: string;
  begin
    lineType := LT_none;
    line := trim(Aline);
    i := pos(';', line);
    if i > 0 then
    begin
      comment := copy(line, i, 9999999);
      line := trim(copy(line, 1, i - 1));
    end
    else
      comment := '';
    if line = '' then lineType := LT_comment;
    parValue := '';
    line := ReplaceStr(line, ' - ', '-');
    //  tokens := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
    tokens := tokenize(line);
    if length(tokens) = 0 then
      lineType := LT_comment;
    for i := 0 to length(tokens) - 1 do
    begin
      s := tokens[i];
      if s[length(s)] = ',' then tokens[i] := copy(s, 1, length(s) - 1);
    end;
  end;

  destructor TASM_line.Destroy;
  begin
    inherited Destroy;
  end;

  function TASM_line.decodeStartProcedure: boolean;
  begin
    Result := False;
    if pos('proc near', line) <= 0 then exit;
    lineType := LT_procBegin;
    Name := tokens[0];
    Result := True;
  end;

  function TASM_line.decodeStartDataBlock: boolean;
  begin
    Result := False;
  end;

  function TASM_line.decodeEndProcedure: boolean;
  begin
    Result := False;
    if pos(' endp', line) <= 0 then exit;
    lineType := LT_procEnd;
    Name := tokens[0];
    Result := True;
  end;


  function TASM_line.decodeLabel: boolean;
  var
    s: string;
  begin
    s := LowerCase(trim(line));
    //   if (pos('loc', s) = 1) and (s[length(s)] = ':') then
    if s[length(s)] = ':' then
    begin
      Result := True;
      lineType := LT_label;
      Name := Copy(s, 1, length(s) - 2);
    end
    else
      Result := False;
  end;

  function TASM_line.decodeParDeclaration: boolean;
  var
    i: integer;
  begin
    // arg_0           = qword ptr  8
    //arg_8           = qword ptr  10h
    //arg_10          = qword ptr  18h
    Result := False;
    if pos('=', line) <= 0 then exit;
    lineType := LT_parDeclaration;
    Name := tokens[0];
    for i := 2 to length(tokens) - 1 do
      parValue := parValue + tokens[i];
    //the asm line wil be: asmLine := name + '=' + parValue;
    Result := True;
  end;

  function TASM_line.decodeMemorySpace: boolean;
    // db 0CCh    reserve a memory space
    //  In Pascal could be represented as a try except block
  var
    i: integer;
  begin
    Result := False;
    if pos('db ', line) <= 0 then exit;
    lineType := LT_memorySpace;
    Name := tokens[0];  //db
    for i := 1 to length(tokens) - 1 do
      parValue := parValue + tokens[i];
    //the asm line wil be: asmLine := db +' '+ parValue;
    Result := True;
  end;

  function TASM_line.decodeGlobalVarByte: boolean;
    //byte_140043C3F  db 11h dup(0CCh)
    // byte_140043C3F: This is a label that identifies the memory location starting at address 140043C3F.
    //db: This stands for "define byte," and it's used to declare a byte or a series of bytes in memory.
    //11h dup(0CCh): This means "repeat the value 0CCh 11 times."
    //So, the statement byte_140043C3F db 11h dup(0CCh) is essentially defining 17 consecutive bytes (since 11h is hexadecimal for 17) with the value 0CCh.
  var
    i: integer;
  begin
    Result := False;
    if pos('byte_', line) <= 0 then exit;
    lineType := LT_GlobalVarByte;
    address := tokens[0];
    Name := tokens[1];  //db
    for i := 2 to length(tokens) - 1 do
      parValue := parValue + tokens[i];
    //the asm line wil be: asmLine := address + ' ' + db +' '+ parValue;
    Result := True;
  end;

  function TASM_line.decodePublicProcName: boolean;
    //public SDEntryPoint
  begin
    Result := False;
    if pos('public', line) <= 0 then exit;
    lineType := LT_publicProcName;
    Name := tokens[1];  //SDEntryPoint
    //the asm line wil be: asmLine := 'public ' + name;
    Result := True;
  end;

  function TASM_line.decodeAlign: boolean;
  begin
    Result := False;
    if pos('align', line) <= 0 then exit;
    lineType := LT_align;
    address := tokens[1];
    //the asm line wil be: asmLine := 'align ' + address;
    Result := True;
  end;

  function TASM_line.decodeInstruction: boolean;
  var
    i: integer;
    s: string;
  begin
    Result := False;
    instruction := instructionNamesList.get(tokens[0]);
    if instruction = _unknown then
      //it isn't a know instruction. It's an error
    begin
      raiseError(2, lineNum, 1, line);
      exit;
    end;
    lineType := LT_instruction;
    if length(tokens) = 1 then
      // instruction without operators
    begin
      SetLength(operators, 0);
      Result := True;
      exit;
    end;
    SetLength(operators, length(tokens) - 1);
    for i := 1 to length(operators) - 1 do
    begin
      s := tokens[i];
      operators[i].Register := RegisterList.get(s);
      if operators[i].Register <> R_unknown then
      begin
        operators[i].p := 0;
        operators[i].Ftype := O_register;
      end else
      if OPkeywordList.isRegister(s) then
      begin
        operators[i].asWritten := s;
        operators[i].Ftype := O_opkw;
        operators[i].keyword := OPkeywordList.get(s);
      end else
      if startWith(s, 'cs:') then
      begin
        operators[i].Ftype := O_csPointer;
        operators[i].asWritten := s;
      end else
      if insideSquareBrakets(s) then
      begin
        operators[i].Ftype := O_expression;
        operators[i].asWritten := s;
      end else

      if s = 'ss:rva' then
      begin
        operators[i].Ftype := O_ss_rva;
        operators[i].asWritten := s;
      end else

      if startWith(s, 'loc_') then
      begin
        operators[i].Ftype := O_loc;
        operators[i].asWritten := s;
      end else
      if startWith(s, 'def_') then
      begin
        operators[i].Ftype := O_def;
        operators[i].asWritten := s;
      end else
      if startWith(s, 'sub_') then
      begin
        operators[i].Ftype := O_procAddress;
        operators[i].asWritten := s;
      end else
      if startWith(s, 'dword') then
      begin
        operators[i].Ftype := O_dwordPointer;
        operators[i].asWritten := s;
      end else
      if startWith(s, 'qword_') then
      begin
        operators[i].Ftype := O_qwordPointer;
        operators[i].asWritten := s;
      end else
      if startWith(s, 'unk_') then
      begin
        operators[i].Ftype := O_procAddress;
        operators[i].asWritten := s;
      end else
      if startWith(s, 'API_') then
      begin
        operators[i].Ftype := O_API;
        operators[i].asWritten := s;
      end else
      if startWith(s, 'CAPI_') then
      begin
        operators[i].Ftype := O_CAPI;
        operators[i].asWritten := s;
      end else
      if startWith(s, '?') then
      begin
        operators[i].Ftype := O_questionMark;
        operators[i].asWritten := s;
      end else
      if MayBeIsEx(s) then
      begin
        operators[i].Ftype := O_ex;
        operators[i].asWritten := s;
      end else
      if MayBeIsDecimal(s) then
      begin
        operators[i].Ftype := O_decimal;
        operators[i].asWritten := s;
      end else begin
        ProcedureList.add(s);
        operators[i].Ftype := O_ProcName;
      end;
      //if OnlyLetters(s) then
      //  raiseError(3, lineNum, 0, line, tokens, i) //not correct!!!!!!
      //else
      //  raiseError(3, lineNum, 0, line, tokens, i);
    end;
  end;


  { TASMfile }
  constructor TASMfile.Create;
  begin
    setLength(Lines, 0);
  end;

  destructor TASMfile.Destroy;
  begin
    setLength(Lines, 0);
  end;

  procedure TASMfile.setFile(asmFileName: string);
  var
    FileLines: TStringList;
    i: integer;
  begin
    FileLines := TStringList.Create;
    try
      FileLines.LoadFromFile(asmFileName);
      setLength(Lines, FileLines.Count);
      for i := 0 to FileLines.Count - 1 do
      begin
        Lines[i] := TASM_line.Create(FileLines[i]);
        Lines[i].lineNum := i;
      end;
    finally
      FileLines.Free;
    end;
  end;

  procedure TASMfile.decodeLines(FileName: string);
  var
    n, rowNum, idx: integer;
    state: TreadingState;
    startingIDA: boolean;
    MyFile: TextFile;
    Line: TASM_line;
    s: string;
  begin
    state := RS_other;
    startingIDA := True;
    //n is an arbitrary integer value greater than the nuber of lines of the file
    AssignFile(MyFile, FileName);
    Reset(MyFile);
    //toggle comment to next line if you want to auto detect the number of lines of the file
    //rowNum := -1;
    rowNum := 36227999;
    if rowNum < 0 then
    begin
      writeln('detecting the number of lines in the input file');
      idx := -1;
      while not EOF(MyFile) do
      begin
        readln(MyFile, s);
        Inc(idx);
      end;
      rowNum := idx; //36227996
      writeln(rowNum, ' rows detected in the input file');
    end;
    idx := -1;
    progressBar.start(rowNum);
    setLength(Lines, rowNum + 1);
    Reset(MyFile);
    try
      while not EOF(MyFile) do
      begin
        readln(MyFile, s);
        Inc(idx);
        progressBar.update(idx);
        if idx >= rowNum then
        begin
          writeln('maximum lne number reached. Please increase n ');
          writeln('press a key to stop');
          readln;
          halt;
        end;
        line := TASM_line.Create(s);
        Lines[idx] := line;
        Line.lineNum := idx;
        if line.lineType = LT_comment then continue;
        case state of
          RS_other:
            //prima dell'inizio delle procedure e tra le procedure
            if line.decodeStartProcedure then
            begin
              state := RS_procedure;
              startingIDA := False;
            end
            else if line.decodeStartDataBlock then  state := RS_dataStructure
            else if line.decodeLabel then
            begin
              //it is an alignament instruction that for me it is equivalent to a comment
              line.lineType := LT_comment;
              line.comment := line.line + line.comment;
              line.line := '';
            end
            else if line.decodeAlign then  line.lineType := LT_align
            else if startingIDA then
            begin
              line.lineType := LT_comment;
              line.comment := line.line;
              line.line := '';
            end
            //eventualy an instruction, for ex. a jmp is located out of a procedure
            //idx've to undrstand how to render in pascal
            else if line.decodeInstruction then line.lineType := LT_instruction
            else if line.decodeGlobalVarByte then
              line.lineType := LT_GlobalVarByte
            else if line.decodeMemorySpace then
              line.lineType := LT_memorySpace
            else if line.decodePublicProcName then
              line.lineType := LT_publicProcName
            else
              raiseError(1, idx, 1, line.line);
          RS_procedure:
            if line.decodeEndProcedure then  state := RS_other
            else if line.decodeLabel then line.lineType := LT_label
            else if line.decodeParDeclaration then
              line.lineType := LT_parDeclaration
            else if line.decodeMemorySpace then
              line.lineType := LT_memorySpace
            else if line.decodeAlign then  line.lineType := LT_align
            else
              // decodeInstruction rise exception if it is not an instruction
              line.decodeInstruction;
        end;
      end;
    finally
      CloseFile(MyFile);
    end;
    n := 0;
    for idx := 0 to length(Lines) - 1 do
      if line.lineType = LT_procBegin then Inc(n);
    writeln(n, ' procedures find;  Lines with lineType = LT_procBegin');
  end;

  procedure TFP_reveng.DoRun;
  var
    ErrorMsg: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }

    if ParamCount < 1 then
      FasmFileName :=
        'C:\Users\cmgau\documentiCMG\hacker e dintorni\Soliddesigner decompilato - sorgenti asm e c\SD_ida_p0_ansi.asm'
    else
      FasmFileName := ParamStr(1);

    if not FileExists(FasmFileName) then
    begin
      writeln('FATAL ERROR : file ', FasmFileName, ' don''t exist');
      Terminate;
      exit;
    end;
    currLineNum := 0;
    colNum := 0;
    currLine := '';
    Writeln('parsing : ', FasmFileName);
    //  FasmFile.setFile(FasmFileName);

    //   Writeln('readed ', length(FasmFile.Lines), ' lines');
    writeln('start decoding lines');

    FasmFile.decodeLines(FasmFileName);

    writeln(length(FasmFile.Lines), ' lines decoded without errors');

    writeln('mancanti stampati. Press enterto close');
    writeln(ProcedureList.l.Count, ' tokens considered procedure names');
    mancanti.SaveToFile('C:\Users\cmgau\documentiCMG\hacker e dintorni\mancanti.txt');
    readln;
    halt;
    // stop program loop
    exit;
  end;

  constructor TFP_reveng.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
    FasmFile := TASMfile.Create;
  end;

  destructor TFP_reveng.Destroy;
  begin
    inherited Destroy;
    FasmFile.Free;
  end;

  procedure TFP_reveng.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

  procedure TFP_reveng.HandleException(Sender: TObject);
  begin
    inherited HandleException(Sender);
  end;


begin
  Application := TFP_reveng.Create(nil);
  Application.Title := 'FP_reveng';
  progressBar := TCRTprogressBar.Create;
  mancanti := TStringList.Create;
  instructionNamesList := TinstructionList.Create;
  RegisterList := TRegisterList.Create;
  OPkeywordList := TOperatorKeyWordList.Create;
  ProcedureList := TProcedureNameList.Create;
  mancanti.sorted := True;
  mancanti.Duplicates := dupIgnore;
  Application.Run;


  progressBar.Free;
  mancanti.Free;
  instructionNamesList.Free;
  RegisterList.Free;
  OPkeywordList.Free;
  ProcedureList.Free;
  Application.Free;
end.
//procedure TASM_line.fromStringToOperator(s: string; var op: TOperator);//  procedure setRegister(r: TRegister);//  begin//    op.Ftype := O_register;//    op.rt := r;//  end;//begin//  op.rt := R_unknown;//  s := LowerCase(s);//  if s = '' then//    exit;//  op.p := 0;//  case s[1] of//    'a': if s = 'ax' then  setRegister(R_ax)//      else if s = 'al' then  setRegister(R_al);//    'd': if s = 'dl' then  setRegister(R_dl);//    'e': if s = 'eax' then  setRegister(R_eax)//      else if s = 'ecx' then  setRegister(R_ecx)//      else//      if s = 'esi' then  setRegister(R_esi);//    'r': if s = 'rax' then  setRegister(R_rax)//      else//      if s = 'rbx' then  setRegister(R_rbx)//      else//      if s = 'rdx' then  setRegister(R_rdx)//      else//      if s = 'rsp' then  setRegister(R_rsp)//      else//      if s = 'rcx' then  setRegister(R_rcx);//    'x': if s = 'xmm0' then  setRegister(R_xmm0)//      else if s = 'xmm1' then  setRegister(R_xmm1)//      else if s = 'xmm2' then  setRegister(R_xmm2);//  end;//end;//case s[1] of//  'a': if s = 'ax' then  setRegister(R_ax)//    else if s = 'al' then  setRegister(R_al);//  'd': if s = 'dl' then  setRegister(R_dl);//  'e': if s = 'eax' then  setRegister(R_eax)//    else if s = 'ecx' then  setRegister(R_ecx)//    else//    if s = 'esi' then  setRegister(R_esi);//  'r': if s = 'rax' then  setRegister(R_rax)//    else//    if s = 'rbx' then  setRegister(R_rbx)//    else//    if s = 'rdx' then  setRegister(R_rdx)//    else//    if s = 'rsp' then  setRegister(R_rsp)//    else//    if s = 'rcx' then  setRegister(R_rcx);//  'x': if s = 'xmm0' then  setRegister(R_xmm0)//    else if s = 'xmm1' then  setRegister(R_xmm1)//    else if s = 'xmm2' then  setRegister(R_xmm2);//end;//  instruction := _unknown;//s := LowerCase(tokens[0]);//case s[1] of//  'a': if s = 'add' then instruction := _ADD//    else if s = 'addsd' then instruction := _ADDSD//    else if s = 'and' then instruction := _AND//    else if s = 'addpd' then instruction := _ADDPD//    else if s = 'addps' then instruction := _ADDPS//    else if s = 'addss' then instruction := _ADDSS//    else if s = 'andnps' then instruction := _ANDNPS//    else if s = 'andps' then instruction := _ANDPS;//  'b': if s = 'bt' then instruction := _BT//    else if s = 'bsr' then instruction := _BSR//    else if s = 'btc' then instruction := _BTC//    else if s = 'btr' then instruction := _BTR//    else if s = 'bts' then instruction := _BTS;//  'c': if s = 'cmp' then instruction := _CMP//    else if s = 'cvtps2pd' then instruction := _CVTPS2PD//    else if s = 'cvtdq2ps' then instruction := _CVTDQ2PS//    else if s = 'cvtdq2pd' then instruction := _CVTDQ2PD//    else if s = 'cvttsd2si' then instruction := _CVTTSD2SI//    else if s = 'cvttss2si' then instruction := _CVTTSS2SI//    else if s = 'cmovz' then instruction := _CMOVZ//    else if s = 'cmova' then instruction := _CMOVA//    else if s = 'cmovo' then instruction := _CMOVO//    else if s = 'cmovbe' then instruction := _CMOVBE//    else if s = 'cmovle' then instruction := _CMOVLE//    else if s = 'cmovge' then instruction := _CMOVGE//    else if s = 'cmovns' then instruction := _CMOVNS//    else if s = 'cdqe' then instruction := _CDQE//    else if s = 'cqo' then instruction := _CQO//    else if s = 'cdq' then instruction := _CDQ//    else if s = 'cmovb' then instruction := _CMOVB//    else if s = 'cmovg' then instruction := _CMOVG//    else if s = 'cmovl' then instruction := _CMOVL//    else if s = 'cmovs' then instruction := _CMOVS//    else if s = 'cmovnz' then instruction := _CMOVNZ//    else if s = 'cmovnb' then instruction := _CMOVNB//    else if s = 'cmpltpd' then instruction := _CMPLTPD//    else if s = 'comisd' then instruction := _COMISD//    else if s = 'comiss' then instruction := _COMISS//    else if s = 'cpuid' then instruction := _CPUID//    else if s = 'cvtpd2ps' then instruction := _CVTPD2PS//    else if s = 'cvtsd2ss' then instruction := _CVTSD2SS//    else if s = 'cvtsi2sd' then instruction := _CVTSI2SD//    else if s = 'cvtsi2ss' then instruction := _CVTSI2SS//    else if s = 'cvtss2sd' then instruction := _CVTSS2SD//    else if s = 'cvttpd2dq' then instruction := _CVTTPD2DQ//    else if s = 'cwde' then instruction := _CWDE//    else if s = 'call' then instruction := _CALL;//  'd': if s = 'divsd' then instruction := _DIVSD//    else if s = 'div' then instruction := _DIV//    else if s = 'divpd' then instruction := _DIVPD//    else if s = 'dd' then instruction := _DD//    else if s = 'divss' then instruction := _DIVSS//    else if s = 'dq' then instruction := _DQ//    else if s = 'dec' then instruction := _DEC;//  'i': if s = 'inc' then instruction := _INC//    else if s = 'imul' then instruction := _IMUL//    else if s = 'idiv' then instruction := _IDIV//    else if s = 'int' then instruction := _INT;//  'l': if s = 'lea' then instruction := _LEA//    else if s = 'lock' then instruction := _LOCK;//  'm': if s = 'mov' then instruction := _MOV//    else if s = 'movzx' then instruction := _MOVZX//    else if s = 'movsd' then instruction := _MOVSD//    else if s = 'movd' then instruction := _MOVD//    else if s = 'mul' then instruction := _MUL//    else if s = 'mulss' then instruction := _MULSS//    else if s = 'movsxd' then instruction := _MOVSXD//    else if s = 'movdqa' then instruction := _MOVDQA//    else if s = 'movdqu' then instruction := _MOVDQU//    else if s = 'movups' then instruction := _MOVUPS//    else if s = 'movss' then instruction := _MOVSS//    else if s = 'movaps' then instruction := _MOVAPS//    else if s = 'maxpd' then instruction := _MAXPD//    else if s = 'maxsd' then instruction := _MAXSD//    else if s = 'maxss' then instruction := _MAXSS//    else if s = 'minpd' then instruction := _MINPD//    else if s = 'minsd' then instruction := _MINSD//    else if s = 'minss' then instruction := _MINSS//    else if s = 'movhpd' then instruction := _MOVHPD//    else if s = 'movlhps' then instruction := _MOVLHPS//    else if s = 'movlpd' then instruction := _MOVLPD//    else if s = 'movq' then instruction := _MOVQ//    else if s = 'movsx' then instruction := _MOVSX//    else if s = 'movupd' then instruction := _MOVUPD//    else if s = 'mulpd' then instruction := _MULPD//    else if s = 'mulps' then instruction := _MULPS//    else if s = 'mulsd' then instruction := _MULSD;//  'n': if s = 'nop' then instruction := _nop//    else if s = 'not' then instruction := _NOT//    else if s = 'neg' then instruction := _NEG;//  'j': if s = 'jg' then  instruction := _JG//    else if s = 'jnz' then instruction := _JNZ//    else if s = 'jmp' then instruction := _JMP//    else if s = 'jle' then instruction := _JLE//    else if s = 'js' then instruction := _JS//    else if s = 'jz' then instruction := _JZ//    else if s = 'ja' then instruction := _JA//    else if s = 'jl' then instruction := _JL//    else if s = 'jb' then instruction := _JB//    else if s = 'jbe' then instruction := _JBE//    else if s = 'jge' then instruction := _JGE//    else if s = 'jnb' then instruction := _JNB//    else if s = 'jnp' then instruction := _JNP//    else if s = 'jp' then instruction := _JP//    else if s = 'jns' then instruction := _JNS;//  'o': if s = 'or' then instruction := _OR//    else if s = 'orps' then instruction := _ORPS;//  'p': if s = 'push' then instruction := _PUSH//    else if s = 'paddd' then instruction := _PADDD//    else if s = 'pand' then instruction := _PAND//    else if s = 'pandn' then instruction := _PANDN//    else if s = 'pcmpeqb' then instruction := _PCMPEQB//    else if s = 'pcmpeqd' then instruction := _PCMPEQD//    else if s = 'pcmpeqq' then instruction := _PCMPEQQ//    else if s = 'pcmpgtd' then instruction := _PCMPGTD//    else if s = 'pmaxsd' then instruction := _PMAXSD//    else if s = 'pmaxub' then instruction := _PMAXUB//    else if s = 'pminsd' then instruction := _PMINSD//    else if s = 'pminub' then instruction := _PMINUB//    else if s = 'por' then instruction := _POR//    else if s = 'pshufd' then instruction := _PSHUFD//    else if s = 'psrldq' then instruction := _PSRLDQ//    else if s = 'psubd' then instruction := _PSUBD//    else if s = 'punpcklbw' then instruction := _PUNPCKLBW//    else if s = 'punpckldq' then instruction := _PUNPCKLDQ//    else if s = 'punpcklqdq' then instruction := _PUNPCKLQDQ//    else if s = 'pxor' then instruction := _PXOR//    else if s = 'pop' then instruction := _POP;//  'r': if s = 'retn' then instruction := _retn//    else if s = 'rep' then instruction := _REP//    else if s = 'rol' then instruction := _ROL//    else if s = 'ror' then instruction := _ROR//    else if s = 'roundpd' then instruction := _ROUNDPD;//  's': if s = 'sub' then instruction := _SUB//    else if s = 'subsb' then instruction := _SUBSB//    else if s = 'subsd' then instruction := _SUBSD//    else if s = 'shl' then instruction := _SHL//    else if s = 'shr' then instruction := _SHR//    else if s = 'sar' then instruction := _SAR//    else if s = 'setnz' then instruction := _SETNZ//    else if s = 'setnb' then instruction := _SETNB//    else if s = 'setz' then instruction := _SETZ//    else if s = 'setb' then instruction := _SETB//    else if s = 'sqrtpd' then instruction := _SQRTPD//    else if s = 'setbe' then instruction := _SETBE//    else if s = 'setl' then instruction := _SETL//    else if s = 'setle' then instruction := _SETLE//    else if s = 'setnbe' then instruction := _SETNBE//    else if s = 'setnl' then instruction := _SETNL//    else if s = 'setnle' then instruction := _SETNLE//    else if s = 'setns' then instruction := _SETNS//    else if s = 'sets' then instruction := _SETS//    else if s = 'shufps' then instruction := _SHUFPS//    else if s = 'sqrtsd' then instruction := _SQRTSD//    else if s = 'sqrtss' then instruction := _SQRTSS//    else if s = 'subpd' then instruction := _SUBPD//    else if s = 'subss' then instruction := _SUBSS//    else if s = 'sbb' then instruction := _SBB;//  't': if s = 'test' then instruction := _TEST;//  'u': if s = 'unpcklpd' then instruction := _UNPCKLPD//    else if s = 'ucomiss' then instruction := _UCOMISS//    else if s = 'unpckhpd' then instruction := _UNPCKHPD//    else if s = 'unpcklps' then instruction := _UNPCKLPS//    else if s = 'ucomisd' then instruction := _UCOMISD;//  'v': if s = 'vmovdqu' then instruction := _VMOVDQU//    else if s = 'vpermq' then instruction := _VPERMQ//    else if s = 'vzeroupper' then instruction := _VZEROUPPER;//  'x': if s = 'xor' then instruction := _XOR//    else if s = 'xorps' then instruction := _XORPS//    else if s = 'xgetbv' then instruction := _XGETBV//    else if s = 'xchg' then instruction := _XCHG;//end;
