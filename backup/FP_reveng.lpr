program FP_reveng;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  StrUtils;

type

  TTokenType = (ttUnknown, ttMnemonic, ttRegister, ttImmediate, ttLabel, ttDirective);

  TToken = record
    TokenType: TTokenType;
    Value: string;
  end;

  TTokens = array of TToken;

  Tinstruction = (_nop, _MOV, _XOR, _TEST);

  TRegister = (R_rax, R_al);
  TOperatorKind = (O_pointer, O_register);

  TOperator = record
    a: int64;
    Ftype: TOperatorKind;
  end;

  // an asm instruction has the form <instruction>  a, b where a, b are pointers or registers

  { TASM_line }

  TASM_line = class
    line: string;
    comment: string;
    instruction: Tinstruction;
    a, b: TOperator;
    constructor create(Aline:string);
    destructor destroy;override;
     function TokenizeLine: TTokens;
  end;

  { TASMfile }

  TASMfile = class
  public
    Lines: array of TASM_line;
    constructor Create;
    destructor Destroy; override;
    procedure setFile(asmFileName: string);
  end;

  { TFP_reveng }

  TFP_reveng = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    FasmFile: TASMfile;
    FasmFileName: string;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TFP_reveng }


  constructor TASM_line.create(Aline: string);
  begin
    line:=Aline;
  end;

  destructor TASM_line.destroy;
begin
  inherited destroy;
end;

    function TASM_line.TokenizeLine: TTokens;
  var
    Words: TStringArray;
    i: integer;
  begin
    Words := SplitString(Line, ' ');
    SetLength(Result, Length(Words));
    for i := 0 to High(Words) do
    begin
      if AnsiStartsStr(';', Words[i]) then Break; // Commenti
      if Words[i] = '' then Continue;
      if Words[i] = 'MOV' then Result[i].TokenType := ttMnemonic
      else if Words[i] = 'AX' then Result[i].TokenType := ttRegister
      else if AnsiStartsStr('$', Words[i]) then Result[i].TokenType := ttImmediate
      else
        Result[i].TokenType := ttUnknown;
      Result[i].Value := Words[i];
    end;
  end;

  procedure ParseFile(const FileName: string);
  var
    FileLines: TStringList;
    i, j: integer;
    Tokens: TTokens;
  begin
    FileLines := TStringList.Create;
    try
      FileLines.LoadFromFile(FileName);
      for i := 0 to FileLines.Count - 1 do
      begin
     //   Tokens := TokenizeLine(FileLines[i]);
        for j := 0 to High(Tokens) do
          Writeln('Token: ', Tokens[j].Value, ' Type: ', integer(Tokens[j].TokenType));
      end;
    finally
      FileLines.Free;
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
        Lines[i].line := FileLines[i];
    finally
      FileLines.Free;
    end;
  end;

  procedure TFP_reveng.DoRun;
  var
    ErrorMsg: string;
    i: integer;
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
    Writeln('parsing : ', FasmFileName);
    FasmFile.setFile(FasmFileName);
    //parse

    Writeln('readed ', length(FasmFile.Lines), ' lines');


    readln;
    // stop program loop
    Terminate;
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

var
  Application: TFP_reveng;
begin
  Application := TFP_reveng.Create(nil);
  Application.Title := 'FP_reveng';
  Application.Run;
  Application.Free;
end.
