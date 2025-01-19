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
  crt;

type

  Tinstruction = (_unknown, _nop, _MOV, _XOR, _TEST, _PUSH, _SUB, _CDQ, _IDIV,
    _ADD, _CMP, _JG, _POP, _retn, _LEA, _CALL, _JNZ, _JMP, _JLE,
    _MOVZX, _OR, _JNS, _MOVSD, _XORPS, _MOVDQA, _INC, _IMUL, _JS,
    _NEG, _SBB, _AND, _MULSD, _JZ, _MOVUPS, _MOVAPS, _SHL, _SETNZ,
    _UNPCKLPD, _DIVSD, _MOVSS, _CVTPS2PD, _ADDSD, _UCOMISD, _JA, _MOVSXD,
    _SETZ, _CMOVNZ, _MOVDQU, _JGE, _NOT, _DIV, _LOCK, _CMOVS, _XCHG,
    _SQRTPD, _CMOVZ, _JL, _JB, _JNB, _JBE, _DEC, _INT, _BT, _BTS, _CMOVG,
    _MOVD, _CVTDQ2PD, _CVTTSD2SI, _CMOVL, _SHR, _CMOVB, _DIVPD, _SETNB, _CQO,
    _SAR, _CDQE, _CMOVNS, _MUL, _CMOVO, _CMOVBE, _CMOVLE, _CMOVGE);

  TRegister = (R_unknown, R_rax, R_al, R_rbx, R_ecx, R_rdx, R_eax,
    R_rcx, R_ax, R_xmm0, R_xmm1, R_xmm2, R_esi, R_dl);
  TOperatorKind = (O_pointer, O_register);

  TOperator = record
    p: int64;
    Ftype: TOperatorKind;
    rt: TRegister;
  end;


  TLineType = (LT_none, LT_comment, LT_instruction, LT_procBegin,
    LT_procEnd, LT_error, LT_label, LT_parDeclaration, LT_memorySpace,
    LT_GlobalVarByte, LT_publicProcName, LT_align);

  TreadingState = (RS_other, RS_procedure, RS_dataStructure);

  // an asm instruction has the form <instruction>  a, b where a, b are pointers or registers

  { TASM_line }

  TASM_line = class
  private

  public
    line: string;
    comment: string;
    //  tokens: TTokens;
    instruction: Tinstruction;
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
    procedure fromStringToInstruction(s: string);
    procedure fromStringToOperator(s: string; var op: TOperator);
  end;

  { TASMfile }

  TASMfile = class
  public
    Lines: array of TASM_line;
    constructor Create;
    destructor Destroy; override;
    procedure setFile(asmFileName: string);
    procedure decodeLines;
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

  procedure raiseError(errorNum, lineNum, ColomnNum: integer; line: string);
  var
    desc: string;
    i: integer;
  begin
    case errorNum of
      1: desc := format(
          'Line %d is not valid. Expected the start of procedure or the start of a data block.',

          [lineNum]);
      2: desc := format(
          'ERROR in line %d,  _unknown instruction. Expected the end of procedure or a valid instruction.',
          [lineNum]);
    end;
    writeln('ERROR during elaboration : errNo:', errorNum, ' : ', desc);
    writeln('   Line number ', lineNum);
    writeln('   Position in the line ', ColomnNum);
    writeln(line);
    for i := 1 to ColomnNum do Write('_');
    writeln('|__');
    writeln('press a key to terminate');
    readln;
    halt(errorNum);
    // application.terminate(errorNum);
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
  end;

  destructor TASM_line.Destroy;
  begin
    inherited Destroy;
  end;

  function TASM_line.decodeStartProcedure: boolean;
  var
    tokens: TStringArray;
  begin
    Result := False;
    if pos(' proc near', line) <= 0 then exit;
    tokens := SplitString(Line, ' ');
    lineType := LT_procBegin;
    Name := tokens[0];
    Result := True;
  end;

  function TASM_line.decodeStartDataBlock: boolean;
  begin
    Result := False;
  end;

  function TASM_line.decodeEndProcedure: boolean;
  var
    tokens: TStringArray;
  begin
    Result := False;
    if pos(' endp', line) <= 0 then exit;
    tokens := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
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
    tokens: TStringArray;
    i: integer;
  begin
    // arg_0           = qword ptr  8
    //arg_8           = qword ptr  10h
    //arg_10          = qword ptr  18h
    Result := False;
    if pos('=', line) <= 0 then exit;
    tokens := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
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
    tokens: TStringArray;
    i: integer;
  begin
    Result := False;
    if pos('db ', line) <= 0 then exit;
    tokens := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
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
    tokens: TStringArray;
    i: integer;
  begin
    Result := False;
    if pos('byte_', line) <= 0 then exit;
    tokens := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
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
  var
    tokens: TStringArray;
    i: integer;
  begin
    Result := False;
    if pos('public', line) <= 0 then exit;
    tokens := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
    lineType := LT_publicProcName;
    Name := tokens[1];  //SDEntryPoint
    //the asm line wil be: asmLine := 'public ' + name;
    Result := True;
  end;

  function TASM_line.decodeAlign: boolean;
  var
    tokens: TStringArray;
    i: integer;
  begin
    Result := False;
    if pos('align', line) <= 0 then exit;
    tokens := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
    lineType := LT_align;
    address := tokens[1];
    //the asm line wil be: asmLine := 'align ' + address;
    Result := True;
  end;

  function TASM_line.decodeInstruction: boolean;
  var
    tokens: TStringArray;
  begin
    Result := False;
    tokens := line.Split(' ', TStringSplitOptions.ExcludeEmpty);
    fromStringToInstruction(tokens[0]);
    case instruction of
      _nop, _retn, _CQO, _CDQ: ;
      _PUSH, _POP, _CALL, _JNZ, _JMP, _JLE, _JNS, _INC, _JS, _NEG,
      _JZ, _JA, _SETNZ, _JL, _JB, _JNB, _SETZ, _JGE, _NOT, _JBE, _DIV,
      _INT, _SETNB, _MUL, _IDIV:
        fromStringToOperator(tokens[1], a);
      _SUB, _MOV, _ADD, _LEA, _XOR, _TEST, _MOVZX, _OR, _MOVSD, _XORPS,
      _MOVDQA, _SBB, _AND, _MULSD, _MOVUPS, _MOVAPS, _SHL, _UNPCKLPD, _CMOVS,
      _DIVSD, _MOVSS, _CVTPS2PD, _ADDSD, _UCOMISD, _SQRTPD, _CMOVZ,
      _MOVSXD, _SHR, _DIVPD, _CMOVO, _CMOVBE, _CMOVLE,
      _CMOVNZ, _MOVDQU, _XCHG, _DEC, _BT, _BTS, _MOVD, _CVTTSD2SI,
      _CMOVG, _CMOVL, _CMOVB, _SAR, _CDQE, _CMOVNS, _CMOVGE: begin
        if (length(tokens) > 0) and (tokens[1] <> '') then
          fromStringToOperator(tokens[1], a);
        if (length(tokens) > 0) and (tokens[2] <> '') then
          fromStringToOperator(tokens[2], b);
      end;
      _IMUL:
      begin
        //a volte da errore perchè non ci sono tutti i parametri
        fromStringToOperator(tokens[1], a);
        if (length(tokens) > 0) and (tokens[2] <> '') then
          fromStringToOperator(tokens[2], b);
        if (length(tokens) > 1) and (tokens[3] <> '') then
          fromStringToOperator(tokens[3], c);
      end;
      _LOCK: begin
        fromStringToOperator(tokens[1], a);
        fromStringToOperator(tokens[2], b);
        fromStringToOperator(tokens[3], c);
        fromStringToOperator(tokens[4], d);
      end;
    end;
    if instruction <> _unknown then Result := True;
  end;

  procedure TASM_line.fromStringToInstruction(s: string);
  begin
    s := LowerCase(s);
    instruction := _unknown;
    case s[1] of
      'a': if s = 'add' then instruction := _ADD
        else if s = 'addsd' then instruction := _ADDSD
        else if s = 'and' then instruction := _AND;
      'b': if s = 'bt' then instruction := _BT
        else if s = 'bts' then instruction := _BTS;
      'c': if s = 'cmp' then instruction := _CMP
        else if s = 'cvtps2pd' then instruction := _CVTPS2PD
        else if s = 'cvtdq2pd' then instruction := _CVTDQ2PD
        else if s = 'cvttsd2si' then instruction := _CVTTSD2SI
        else if s = 'cmovz' then instruction := _CMOVZ
        else if s = 'cmovo' then instruction := _CMOVO
        else if s = 'cmovbe' then instruction := _CMOVBE
        else if s = 'cmovle' then instruction := _CMOVLE
        else if s = 'cmovge' then instruction := _CMOVGE
        else if s = 'cmovns' then instruction := _CMOVNS
        else if s = 'cdqe' then instruction := _CDQE
        else if s = 'cqo' then instruction := _CQO
        else if s = 'cdq' then instruction := _CDQ
        else if s = 'cmovb' then instruction := _CMOVB
        else if s = 'cmovg' then instruction := _CMOVG
        else if s = 'cmovl' then instruction := _CMOVL
        else if s = 'cmovs' then instruction := _CMOVS
        else if s = 'cmovnz' then instruction := _CMOVNZ
        else if s = 'call' then instruction := _CALL;
      'd': if s = 'divsd' then instruction := _DIVSD
        else if s = 'div' then instruction := _DIV
        else if s = 'divpd' then instruction := _DIVPD
        else if s = 'dec' then instruction := _DEC;
      'i': if s = 'inc' then instruction := _INC
        else if s = 'imul' then instruction := _IMUL
        else if s = 'idiv' then instruction := _IDIV
        else if s = 'int' then instruction := _INT;
      'l': if s = 'lea' then instruction := _LEA
        else if s = 'lock' then instruction := _LOCK;
      'm': if s = 'mov' then instruction := _MOV
        else if s = 'movzx' then instruction := _MOVZX
        else if s = 'movsd' then instruction := _MOVSD
        else if s = 'movd' then instruction := _MOVD
        else if s = 'mul' then instruction := _MUL
        else if s = 'movsxd' then instruction := _MOVSXD
        else if s = 'movdqa' then instruction := _MOVDQA
        else if s = 'movdqu' then instruction := _MOVDQU
        else if s = 'movups' then instruction := _MOVUPS
        else if s = 'movss' then instruction := _MOVSS
        else if s = 'movaps' then instruction := _MOVAPS
        else if s = 'mulsd' then instruction := _MULSD;
      'n': if s = 'nop' then instruction := _nop
        else if s = 'not' then instruction := _NOT
        else if s = 'neg' then instruction := _NEG;
      'j': if s = 'jg' then  instruction := _JG
        else if s = 'jnz' then instruction := _JNZ
        else if s = 'jmp' then instruction := _JMP
        else if s = 'jle' then instruction := _JLE
        else if s = 'js' then instruction := _JS
        else if s = 'jz' then instruction := _JZ
        else if s = 'ja' then instruction := _JA
        else if s = 'jl' then instruction := _JL
        else if s = 'jb' then instruction := _JB
        else if s = 'jbe' then instruction := _JBE
        else if s = 'jge' then instruction := _JGE
        else if s = 'jnb' then instruction := _JNB
        else if s = 'jns' then instruction := _JNS;
      'o': if s = 'or' then instruction := _OR;
      'p': if s = 'push' then instruction := _PUSH
        else if s = 'pop' then instruction := _POP;
      'r': if s = 'retn' then instruction := _retn;
      's': if s = 'sub' then instruction := _SUB
        else if s = 'shl' then instruction := _SHL
        else if s = 'shr' then instruction := _SHR
        else if s = 'sar' then instruction := _SAR
        else if s = 'setnz' then instruction := _SETNZ
        else if s = 'setnb' then instruction := _SETNB
        else if s = 'setz' then instruction := _SETZ
        else if s = 'sqrtpd' then instruction := _SQRTPD
        else if s = 'sbb' then instruction := _SBB;
      't': if s = 'test' then instruction := _TEST;
      'u': if s = 'unpcklpd' then instruction := _UNPCKLPD
        else if s = 'ucomisd' then instruction := _UCOMISD;
      'x': if s = 'xor' then instruction := _XOR
        else if s = 'xorps' then instruction := _XORPS
        else if s = 'xchg' then instruction := _XCHG;
    end;
    lineType := LT_instruction;
  end;

  procedure TASM_line.fromStringToOperator(s: string; var op: TOperator);

    procedure setRegister(r: TRegister);
    begin
      op.Ftype := O_register;
      op.rt := r;
    end;

  begin
    s := LowerCase(s);
    if s = '' then
      beep;
    op.rt := R_unknown;
    op.p := 0;
    case s[1] of
      'a': if s = 'ax' then  setRegister(R_ax);
      'd': if s = 'dl' then  setRegister(R_dl);
      'e': if s = 'eax' then  setRegister(R_eax)
        else if s = 'ecx' then  setRegister(R_ecx)
        else
        if s = 'esi' then  setRegister(R_esi);
      'r': if s = 'rax' then  setRegister(R_rax)
        else
        if s = 'rbx' then  setRegister(R_rbx)
        else
        if s = 'rdx' then  setRegister(R_rdx)
        else
        if s = 'rcx' then  setRegister(R_rcx);
      'x': if s = 'xmm0' then  setRegister(R_xmm0)
        else if s = 'xmm1' then  setRegister(R_xmm1);
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
      end;
    finally
      FileLines.Free;
    end;
  end;

  procedure TASMfile.decodeLines;
  var
    i, n: integer;
    state: TreadingState;
    startingIDA: boolean;
  begin
    state := RS_other;
    startingIDA := True;
    progressBar.start(length(Lines) - 1);
    for i := 0 to length(Lines) - 1 do
    begin
      progressBar.update(i);
      if Lines[i].lineType = LT_comment then continue;
      case state of
        RS_other: begin
          if Lines[i].decodeStartProcedure then
          begin
            state := RS_procedure;
            startingIDA := False;
          end
          else if Lines[i].decodeStartDataBlock then  state := RS_dataStructure
          else if Lines[i].decodeLabel then
          begin
            //it is an alignament instruction that for me it is equivalent to a comment
            Lines[i].lineType := LT_comment;
            Lines[i].comment := Lines[i].line + Lines[i].comment;
            Lines[i].line := '';
          end
          else if Lines[i].decodeAlign then  Lines[i].lineType := LT_align
          else if startingIDA then
          begin
            Lines[i].lineType := LT_comment;
            Lines[i].comment := Lines[i].line;
            Lines[i].line := '';
          end
          //eventualy an instruction, for ex. a jmp is located out of a procedure
          //I've to undrstand how to render in pascal
          else if Lines[i].decodeInstruction then Lines[i].lineType := LT_instruction
          else if Lines[i].decodeGlobalVarByte then
            Lines[i].lineType := LT_GlobalVarByte
          else if Lines[i].decodeMemorySpace then
            Lines[i].lineType := LT_memorySpace
          else if Lines[i].decodePublicProcName then
            Lines[i].lineType := LT_publicProcName
          else
          begin
            raiseError(1, i, 1, Lines[i].line);
          end;
        end;
        RS_procedure: begin
          if Lines[i].decodeEndProcedure then  state := RS_other
          else if Lines[i].decodeLabel then Lines[i].lineType := LT_label
          else if Lines[i].decodeParDeclaration then
            Lines[i].lineType := LT_parDeclaration
          else if Lines[i].decodeMemorySpace then
            Lines[i].lineType := LT_memorySpace
          else if Lines[i].decodeAlign then  Lines[i].lineType := LT_align
          else
          begin
            Lines[i].decodeInstruction;
            if Lines[i].instruction = _unknown then
              raiseError(2, i, 1, Lines[i].line);
          end;
        end;
      end;
    end;
    n := 0;
    for i := 0 to length(Lines) - 1 do
    begin
      if Lines[i].lineType = LT_procBegin then Inc(n);
    end;
    writeln(n, ' procedures find');
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
        'C:\Users\cmgau\documentiCMG\hacker e dintorni\Soliddesigner decompilato - sorgenti asm e c\SD_ida_p0.asm'
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
    FasmFile.setFile(FasmFileName);

    Writeln('readed ', length(FasmFile.Lines), ' lines');
    writeln('start decoding lines');

    FasmFile.decodeLines;

    writeln(length(FasmFile.Lines), ' lines decoded without errors');

    readln;
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
  Application.Run;
  progressBar.Free;
  Application.Free;
end.
