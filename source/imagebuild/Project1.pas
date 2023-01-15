program Project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }, Math, LazUTF8;

const
  EepromStart = $1000;

type

  { TPrimoROMBuilder }

  TPrimoROMBuilder = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure Log(msg: String);
    Procedure countValidLines(FileName:String; var NumOfEntries : Word);
    Procedure CountSizeOfFileEntries(FileName:String; NumOfEntries : Word; var SumSizeOfFileEntries:Word);
    procedure ProcessFile(fileName : String);
    procedure ProcessPriFile(filePath, fileRef, nameRef : String);
    procedure ProcessPtpFile(filePath, fileRef, nameRef : String);
    procedure ProcessROMFile(filePath, fileRef, nameRef : String);
  end;

eepromArrayType = array[EepromStart .. $7FFFF] of Byte;

var
  eeprom : eepromArrayType; // EepromStart - $7FFFF 512kB EEPROM
  currentPointerPos : Word;
  currentFileEntryPos : Word;
  currentContentPos : LongInt;
  lastROMContentPos : LongInt;

{ TPrimoROMBuilder }



procedure copyPrimoFileName(var eeprom: eepromArrayType; pos: word; name: String);
var i,j: word;
  c : char;
  currP : PChar;
  cLength : Integer;
  specChars : Array of String = ('á', 'é', 'í', 'ó', 'ö', 'ő', 'ú', 'ü', 'ű', 'Á', 'É', 'Í', 'Ó', 'Ö', 'Ő', 'Ú', 'Ü', 'Ű', '[', ']');
  primoVals : Array of Byte =   ($7d, $60, $1e, $5b, $7c, $7b, $5f, $7e, $7f, $5d, $40, $1e, $5b, $5c, $7b, $5f, $5e, $7f, $81, $82);
  len : Integer;
  charToCheck : String;
Begin
  len := length(name);
  i:=1;
  currP := PChar(name);
  while i<=len Do
  Begin
    cLength:=UTF8CodepointSize(currP);
    c:='-';
    charToCheck:=copy(name, i, cLength);
    for j:=0 to 19 Do begin
      if charToCheck = specChars[j] then begin
        c:=char(primoVals[j]);
        break;
      end;
    end;
    if(cLength = 1) and (c='-') Then begin
      c := name[i];
    end;

    eeprom[pos+i-1] := byte(c);
    inc(i, cLength);
    inc(currP, cLength);
    dec(pos, cLength - 1);
  end;
  eeprom[pos+i-1]:=0;
end;

procedure TPrimoROMBuilder.Log(msg : String);
Begin
  WriteLn(msg);
end;

Procedure TPrimoROMBuilder.CountValidLines(FileName:String; var NumOfEntries : Word);
var f : TextFile;
    line : String;
Begin
  NumOfEntries:=0;
  try
    AssignFile(f, fileName);
    Reset(f);
    while(Not EOF(f)) Do
    Begin
      Readln(f, line);
      line := Trim(line);
      if(line = '') Or (line[1]='#') Then
         Continue;

      Inc(NumOfEntries);
    end;
  finally
    CloseFile(f);
  end;
end;

Procedure TPrimoROMBuilder.CountSizeOfFileEntries(FileName:String; NumOfEntries : Word; var SumSizeOfFileEntries:Word);
var f : TextFile;
    p,offset,n : word;
    line : String;
    prgname : String;
Begin
  offset:=EepromStart + NumOfEntries*2 + 2;
  SumSizeOfFileEntries:=0;
  try
    AssignFile(f, fileName);
    Reset(f);
    while(Not EOF(f)) Do
    Begin
      Readln(f, line);
      line := Trim(line);
      if(line = '') Or (line[1]='#') Then
         Continue;

      p:=pos(',', line);
      if p = 0 Then
      Begin
        n:=Pos('.', line);
        if n=0 then
          continue;
        prgname:=Copy(line, 1, n-1);
      end
      else
      Begin
        prgname:=copy(line, p+1, Length(line)-p);
      end;

      if ((offset + SumSizeOfFileEntries + UTF8Length(prgname) - p + 6) div $4000 <>
          (offset + SumSizeOfFileEntries - 1) div $4000) Then
      Begin
        SumSizeOfFileEntries:=((offset + SumSizeOfFileEntries) div $4000 + 1) * $4000;
      end
      else
      Begin
        Inc(SumSizeOfFileEntries, UTF8Length(prgname) + 7);
      end;
    end;
  finally
    CloseFile(f);
  end;
end;

procedure TPrimoROMBuilder.ProcessPriFile(filePath, fileRef, nameRef : String);
Var
    binFile : File of Byte;
    binfSize : Word;
Begin
  Log('.pri entry parsing...' + fileRef);
  try
    if(not fileExists(filePath + fileRef)) Then
    Begin
      log('File not found: ' + fileRef);
      exit;
    end;
    AssignFile(binFile, filePath + fileRef);
    Reset(binFile);
    binfSize:=FileSize(binFile);
    if ((binfSize + currentContentPos) >= lastROMContentPos) then
    Begin
      Log('Payload is full.');
      currentContentPos:=$80000;
      exit;
    end;

    BlockRead(binFile, eeprom[currentContentPos], binfSize);
    eeprom[currentFileEntryPos] := 1;  // type: pri
    Log('.pri file exists');
    eeprom[currentFileEntryPos+1] := Byte(binfSize mod 256); // size
    eeprom[currentFileEntryPos+2] := Byte(binfSize div 256);

    eeprom[currentFileEntryPos+3] := Byte(currentContentPos div 16384); // bank
    eeprom[currentFileEntryPos+4] := Byte((currentContentPos mod 16384) mod 256); // address in bank
    eeprom[currentFileEntryPos+5] := Byte((currentContentPos mod 16384) div 256);
    copyPrimoFileName(eeprom, currentFileEntryPos+6, NameRef);

    eeprom[currentPointerPos] := Byte(currentFileEntryPos mod 256);
    eeprom[currentPointerPos+1] := Byte(currentFileEntryPos div 256);

    WriteLn('pri file is processed as: ', nameRef, ', starting in ROM at $',
                 IntToHex(currentContentPos, 6), ' (- $', IntToHex(currentContentPos + binfSize-1,6), ')');

    if ((currentFileEntryPos + UTF8Length(NameRef) + 6) div $4000 <>
        (currentFileEntryPos) div $4000) Then
    Begin
      currentFileEntryPos:=((currentFileEntryPos + UTF8Length(NameRef) + 6) div $4000) * $4000;
    end
    else
    Begin
      Inc(currentFileEntryPos, UTF8Length(NameRef) + 7);
    end;

    inc(currentPointerPos,2);
    Inc(currentContentPos, binfSize);
  finally
    try
      CloseFile(binFile);
    finally
    end;

  end;
end;

procedure TPrimoROMBuilder.ProcessPtpFile(filePath, fileRef, nameRef : String);
type ptpHeaderR = packed record
       startByte : Byte;
       fileLength : word;
     end;
     ptpBlockHdrR = packed Record
       startByte : Byte;
       blockLength : Word;
       blockType : Byte;
       blockNum  : Byte;
       blockAddr : Word;
       blockDataLen  : Byte;
     end;

Var
    binFile : File of Byte;
    binfSize : Word;
    ptpHeader : ptpHeaderR;
    ptpBlockHdr : ptpBlockHdrR;
    properPTPPrimoFileFound : Boolean;
    i,p,ptpFile, ptpFileIdx : Integer;
    primoFilePosInPTP : LongInt;
    blockPosInPTP : LongInt;
Begin
  Log('.ptp entry parsing...' + fileRef);
  try
    p := pos(':', fileRef);
    if p<>0 then begin
      ptpFileIdx := StrToInt(copy(fileRef, p+1, length(fileRef) - p));
      fileRef := copy(fileRef, 1, p-1);
    end
    else
      ptpFileIdx := 0;

    if (not fileExists(filePath + fileRef)) Then
    Begin
      WriteLn('File not found: ', fileRef);
      exit;
    end;
    AssignFile(binFile, filePath + fileRef);
    Reset(binFile);
    i:=1;
    primoFilePosInPTP:=0;
    Write('Primo file lengths in PTP file: ');
    repeat
      BlockRead(binFile, ptpHeader, sizeof(ptpHeader));
      if(ptpHeader.startByte<>$ff) then begin
        write(' !!Invalid primo file header at ', IntToStr(i), ' !!');
        break;
      end;
      Write('#',IntToStr(i),':',IntToHex(ptpHeader.fileLength, 4), ' ');
      Inc(i);
      Inc(primoFilePosInPTP, ptpHeader.fileLength);
      Seek(binFile, primoFilePosInPTP);
    until Eof(binFile);
    writeLn;
    Seek(binFile, 0);
    binfSize:=FileSize(binFile);
    properPTPPrimoFileFound := false;
    ptpFile := 0;
    repeat
      Inc(ptpFile);
      primoFilePosInPTP := filePos(binFile);
      BlockRead(binFile, ptpHeader, 3); {blockRead works because of packed record}
      if(ptpHeader.startByte <> 255) Then begin
        WriteLn('PTP Primo file doesn''t start with $FF at idx #', IntToStr(ptpFile), ', failed to process PTP file');
        exit;
      end;
      Write('Processing #', ptpFile, ' primo file in ptp file... ');
      if(ptpFileIdx<>0) and (ptpFile<>ptpFileIdx) Then Begin
        Seek(binFile, primoFilePosInPTP + ptpHeader.fileLength);
        Writeln('Skipping as current file# doesn''t match the given file#');
        continue;
      end;

      blockPosInPTP:=primoFilePosInPTP+3;
      Repeat
        BlockRead(binFile, ptpBlockHdr, 5);  {block start, blockLength, blockType, blockNum}
        if( ((ptpBlockHdr.startByte = $55) or (ptpBlockHdr.blockType = $B9)) and
             ((ptpBlockHdr.blockType<>$83) and (ptpBlockHdr.blockType<>$87))) Then
        Begin
          BlockRead(binFile, ptpBlockHdr.blockAddr, 2);
          if ptpBlockHdr.blockType<>$b9 Then
          Begin
            BlockRead(binFile, ptpBlockHdr.blockDataLen, 1);
            Write(IntToHex(ptpBlockHdr.startByte, 2), '-', IntToHex(ptpBlockHdr.blockType,2),'-@',
                  IntToHex(ptpBlockHdr.blockAddr, 4), '-', IntToHex(ptpBlockHdr.blockDataLen,2), ' ');
          End
          else
          Begin
            Write(IntToHex(ptpBlockHdr.startByte, 2), '-', IntToHex(ptpBlockHdr.blockType,2),'-@',
                  IntToHex(ptpBlockHdr.blockAddr, 4), ' ');
          End;
        end
        else
        Begin
          Write(IntToHex(ptpBlockHdr.startByte, 2), '-', IntToHex(ptpBlockHdr.blockType,2), ' ');
        end;
        if (ptpBlockHdr.startByte = $AA) and  {closing block}
          ((ptpBlockHdr.blockType = $B1) or (ptpBlockHdr.blockType = $B9)) Then begin
            properPTPPrimoFileFound:=True;
            seek(binFile, primoFilePosInPTP);
            Writeln(' $B1 or $B9 type found in the last block, primo file selected');
            break;
        end
        else if (ptpBlockHdr.startByte = $55) or (ptpBlockHdr.startByte = $AA) then begin
          Inc(blockPosInPTP, ptpBlockHdr.blockLength + 3);
          seek(binFile, blockPosInPTP);
          if(ptpBlockHdr.startByte = $AA) then begin
            Writeln(' end of blocks reached, not a valid primo file, checking the next one...');
          end;
        end
        else
        begin
          WriteLn('\nInvalid tape block in primo file #',IntToStr(ptpFile),' at file pos $', IntToHex(blockPosInPTP, 6));
          exit;
        end;
      Until ptpBlockHdr.startByte=$AA;
    Until properPTPPrimoFileFound and (not Eof(binFile));

    if not properPTPPrimoFileFound then begin
      Writeln('No valid Primo file found in ', fileRef, ', skipping..');
      exit;
    end;

    if ((ptpHeader.fileLength + currentContentPos) >= lastROMContentPos) then
    Begin
      Log('Payload is full.');
      currentContentPos:=$80000;
      exit;
    end;
    binFSize := ptpHeader.fileLength;
    BlockRead(binFile, eeprom[currentContentPos], ptpHeader.fileLength);
    eeprom[currentFileEntryPos] := 2;  // type: ptp
    eeprom[currentFileEntryPos+1] := Byte(binfSize mod 256); // size
    eeprom[currentFileEntryPos+2] := Byte(binfSize div 256);

    eeprom[currentFileEntryPos+3] := Byte(currentContentPos div 16384); // bank
    eeprom[currentFileEntryPos+4] := Byte((currentContentPos mod 16384) mod 256); // address in bank
    eeprom[currentFileEntryPos+5] := Byte((currentContentPos mod 16384) div 256);
    copyPrimoFileName(eeprom, currentFileEntryPos+6, NameRef);

    eeprom[currentPointerPos] := Byte(currentFileEntryPos mod 256);
    eeprom[currentPointerPos+1] := Byte(currentFileEntryPos div 256);

    if ((currentFileEntryPos + UTF8Length(NameRef) + 6) div $4000 <>
        (currentFileEntryPos) div $4000) Then
    Begin
      currentFileEntryPos:=((currentFileEntryPos + UTF8Length(NameRef) + 6) div $4000) * $4000;
    end
    else
    Begin
      Inc(currentFileEntryPos, UTF8Length(NameRef) + 7);
    end;

    WriteLn('ptp file is processed as: ' + nameRef + ', starting in ROM at $',IntToHex(currentContentPos, 6));
    inc(currentPointerPos,2);
    Inc(currentContentPos, binfSize);
  finally
    try
      CloseFile(binFile);
    finally
    end;

  end;
end;

procedure TPrimoROMBuilder.ProcessROMFile(filePath, fileRef, nameRef : String);
Var
    binFile : File of Byte;
    romNames : String;
    romName : String;
    romOffset : Word;
    p: Integer;
    currentROMContentPos : LongInt;
Begin
  Log('rom image entry is being processed...');
  currentROMContentPos:=lastROMContentPos - $4000;
  romNames:=fileRef;
  WriteLn('ROM payload starts at $', IntToHex(currentROMContentPos, 6));
  if currentContentPos>=currentROMContentPos then
  Begin
    Log('Payload is full, rom content cannot fit');
    currentContentPos:=$80000;
    exit;
  end;
  if ((7+Length(NameRef) + currentFileEntryPos) > $3FFF) then
  begin
    Log('File entries are full.');
    currentFileEntryPos:=$4000;
    exit;
  end;

  while romNames<>'' Do
  Begin
    p:=pos(';', romNames);
    if p=0 Then
    Begin
      romName:=romNames;
      romNames:='';
    end
    else
    Begin
      romName:=copy(romNames, 1,p-1);
      romNames:=copy(romNames, p+1, Length(romNames));
    end;
    p:=pos(':', romName);
    if p<1 then Continue;
    romOffset:=StrToInt(copy(romName, 1, p-1));
    romName:=copy(romName, p+1, Length(romName));
    Log('Entry in rom inputs: [offset is ' + IntToStr(romOffset) + '], [file name: ' + romName + ']');
    try
      AssignFile(binFile, filePath + romName);
      Reset(binFile);
      BlockRead(binFile, eeprom[currentROMContentPos+romOffset], min(FileSize(binFile), $4000-romOffset));
    finally
      CloseFile(binFile);
    end;
  end;
  eeprom[currentFileEntryPos] := 3; // type: rom
  eeprom[currentFileEntryPos+1] := 0; // size
  eeprom[currentFileEntryPos+2] := 64;

  eeprom[currentFileEntryPos+3] := Byte(currentROMContentPos div 16384); // bank
  eeprom[currentFileEntryPos+4] := Byte((currentROMContentPos mod 16384) mod 256); // address in bank
  eeprom[currentFileEntryPos+5] := Byte((currentROMContentPos mod 16384) div 256);
  copyPrimoFileName(eeprom, currentFileEntryPos+6, NameRef);

  eeprom[currentPointerPos] := Byte(currentFileEntryPos mod 256);
  eeprom[currentPointerPos+1] := Byte(currentFileEntryPos div 256);

  if ((currentFileEntryPos + UTF8Length(NameRef) + 6) div $4000 <>
      (currentFileEntryPos) div $4000) Then
  Begin
    currentFileEntryPos:=((currentFileEntryPos + UTF8Length(NameRef) + 6) div $4000) * $4000;
  end
  else
  Begin
    Inc(currentFileEntryPos, UTF8Length(NameRef) + 7);
  end;
  inc(currentPointerPos,2);
  dec(lastROMContentPos, $4000);
end;


procedure TPrimoROMBuilder.ProcessFile(fileName : String);
Var f : TextFile;
    binFile : File of Byte;
    line: String;
    fileRef : String;
    nameRef : String;
    numOfEntries : Word;
    SumSizeOfFileEntries : Word;

    p,n : SizeInt;
begin
  for p:=EepromStart To $07FFFF Do
    eeprom[p] := 0;
  numOfEntries:=0;
  SumSizeOfFileEntries:=0;

  if not FileExists(FileName) then begin
    WriteLn('Input file (',fileName,') not found, exiting...');
    exit;
  end;


  CountValidLines(FileName, NumOfEntries);
  CountSizeOfFileEntries(FileName, NumOfEntries, SumSizeOfFileEntries);

  currentPointerPos:= EepromStart;
  currentFileEntryPos:=currentPointerPos + NumOfEntries * 2 + 2;  {2 trailing zeroes for closing the pointer list}
  currentContentPos := currentFileEntryPos + SumSizeOfFileEntries;
  lastROMContentPos := $80000;


  assignFile(f, FileName);
  try
    Reset(f);
    Log('Primo list file opened: ' + FileName);
    while (not Eof(f)) and (currentContentPos<$80000) do
    begin
      readLn(f, line);
      line := Trim(line);
      p := Pos(',', line);
      if (length(line) = 0) or (line[1] = '#') Then
        Continue;

      if p = 0 Then
      Begin
        n:=Pos('.', line);
        if n=0 then
          continue;
        NameRef:=Copy(line, 1, n-1);
        FileRef:=line;
      end
      else
      Begin
        NameRef:=trim(copy(line, p+1, Length(line)-p));
        FileRef := copy(line, 1, p-1);
      end;

      p:=pos(':', fileRef);
      if pos('.pri', lowercase(fileRef)) <> 0 Then
      Begin
  // pri file
        processPriFile(ExtractFilePath(FileName), fileRef, nameRef);
      end
      else if pos('.ptp', lowercase(fileRef)) <> 0 Then
      Begin
  // ptp file
        processPtpFile(ExtractFilePath(FileName), fileRef, nameRef);
      end
      else
      Begin
        // rom images
        processROMFile(ExtractFilePath(FileName), fileRef, nameRef);
      end;
    end;
  finally
    closeFile(f);
  end;

  WriteLn('Input file is processed, remaining free space (out of ', IntToStr(512*1024 - eepromStart), ' bytes): ', IntToStr(lastROMContentPos - currentContentPos));

  try
    AssignFile(binFile, ExtractFilePath(FileName) + ExtractFileName(FileName) + '.mcrom');
    ReWrite(binFile);
    BlockWrite(binFile, eeprom, $80000 - EepromStart);
  finally
    CloseFile(binFile);
  end;
end;


procedure TPrimoROMBuilder.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if (paramCount < 1) or (ParamCount>1) Then Begin
    WriteHelp;
    WriteLn('INVALID ARGUMENTS!');
    Terminate;
    Exit;
  end;

  { add your program here }
  if(ExtractFileDir(ParamStr(1)) = '') Then
  Begin
    processFile(ExtractFileDir(ParamStr(0))+DirectorySeparator+ParamStr(1));
  end
  else
  begin
    processFile(ParamStr(1));
  end;
  // stop program loop
  Terminate;
end;

constructor TPrimoROMBuilder.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TPrimoROMBuilder.Destroy;
begin
  inherited Destroy;
end;

procedure TPrimoROMBuilder.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExtractFileName(ExeName));
  Writeln('  -h                 This help');
  writeln('  <.plst file name>  The .plst file name as input file');
end;

var
  Application: TPrimoROMBuilder;
begin
  Application:=TPrimoROMBuilder.Create(nil);
  Application.Title:='Primo 512kB MultiCart ROM Builder';
  Application.Run;
  Application.Free;
end.

