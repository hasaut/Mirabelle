unit TcProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

Type
  TTcOpA = (toaAdd, toaSub, toaAnd, toaOr, toaXor, toaMul, toaUDiv, toaSDiv, toaURem, toaSRem, toaShl, toaShr, toaRol, toaAsr);
  TTcOpF = (tofAdd, tofSub, tofMul, tofDiv);

  TTcProc   = class(TObject)
  private
    FPath       : string;
    FProcNames  : string;
    FCode       : TStringList;

    FRegIdxA,
    FRegIdxB,
    FRegIdxC,
    FRegIdxD    : Integer;

    Procedure StdHeaderAsm ( Const ATestName : string );
    Procedure SaveOpti ( Const ATestName : string );

    Function GenRegName ( ATableIdx, ARegIdx : char ) : string;
    Procedure AppendCmd ( Const ACmdS : string );
    Procedure AppendLabel ( Const AName : string );
    Procedure AppendProcName ( Const AName : string );
    Procedure EnterProcA ( Const ATestName : string; ADataLen : Cardinal );
    Procedure LeaveProcA ( Const ATestName : string; ADataLen : Cardinal );
    Procedure DecRegLenUS ( ARegLen : char; ASignExt : boolean; Var AData : Cardinal; Out AHexLen : Integer; Out AMask : Cardinal );
    Procedure DecRegLenR ( ARegLen : char; Var AData : Cardinal; Out AHexLen : Integer; Out AMask : Cardinal );
    Procedure DecOpA ( AOpcode : TTcOpA; ADataA, ADataB : Cardinal; AShiftResLen : char; Out AResult : Cardinal; Out ACmdS : string );
    Procedure DecOpF ( AOpcode : TTcOpF; ADataA, ADataB : Single; Out AResult : Single; Out ACmdS : string );
    Procedure GenTestAluA_RI ( ARegLen : char; ADataA, ADataB : Cardinal; Const AErrLabel : string );
    Procedure GenTestAluA_RR ( ARegLen : char; ADataA, ADataB : Cardinal; Const AErrLabel : string );
    Procedure GenTestBasic ( AData : Cardinal; Const AErrLabel : string );
    Procedure GenTestF ( ADataA, ADataB : Single; Const AErrLabel : string );
    Procedure GenTestBTM_RB ( ADataA, ADataB : byte; Const AErrLabel : string );
    Procedure GenTestBTR_RW ( AData : Cardinal; ABitIdx : byte; Const AErrLabel : string );
    Procedure GenTestBTR_RD ( AData : Cardinal; ABitIdx : byte; Const AErrLabel : string );
    Procedure GenTestArus ( Const ARegLen : string; ADataA, ADataB : Cardinal; AExt : char; Const AErrLabel : string );
    Procedure GenTestArui ( Const ARegLen : string; ADataA, ADataB : Cardinal; AExt : char; Const AErrLabel : string );
    Procedure GenTestAluA_RI_File ( ARegLen : char; Const ATestName : string; Const ADataSrc : array of Cardinal );
    Procedure GenTestAluA_RI_All ( Const ATestName : string );
    Procedure GenTestAluA_RR_File ( ARegLen : char; Const ATestName : string; Const ADataSrc : array of Cardinal );
    Procedure GenTestAluA_RR_All ( Const ATestName : string );
    Procedure GenTestArus_File ( Const ARegLen : string; AExt : char; Const ATestName : string );
    Procedure GenTestArui_File ( Const ARegLen : string; AExt : char; Const ATestName : string );
    Procedure GenTestBasic_All ( Const ATestName : string; Const ADataSrc : array of Cardinal );
    Procedure GenTestArus_All ( Const ATestName : string );
    Procedure GenTestArui_All ( Const ATestName : string );
    Procedure GenTestF_All ( Const ATestName : string; Const ADataSrc : array of Single );
    Procedure GenTestBTM_All ( Const ATestName : string );
    Procedure GenTestBTR_All ( Const ATestName : string );
    //Procedure GenTestAluA_RUS_X ( ARegLen : char; AHexLen : Integer; AMask : Cardinal; Const ATestName : string; Const ADataSrc : array of Cardinal );
    Procedure AddProcNames;
  public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Build;
  end;

Const
  CAllRegs  = 'awx|bwx|cwx|dwx|ewx|fwx|gwx|ar|br|cr|dr|er|fr|gr';

  CSrcListB : array [0..8] of Cardinal = ($00, $01, $07, $08, $09, $0F, $7F, $80, $FF);
  CSrcListW : array [0..8] of Cardinal = ($0000, $0001, $0008, $0009, $007F, $0080, $7FFF, $8000, $FFFF);
  CSrcListD : array [0..8] of Cardinal = ($00000000, $00000001, $00000008, $00000009, $00007FFF, $00008000, $7FFFFFFF, $80000000, $FFFFFFFF);
  CSrcListF : array [0..8] of Single = (0.0, 1.0, 0.5, 2.5, 3.0, 0.3, -10.0, -100.0, 1000.0);

  CSrcListBasic : array [0..10] of Cardinal = ($00000000, $0000007F, $00000080, $00007F12, $000080CD, $0000FFFF, $007F80FF, $0080ABCD, $7F123456, $80789ABC, $FFFFFFFF);

implementation

Uses
  ConComL;

Constructor TTcProc.Create;
Begin
 Inherited;
 FCode:=TStringList.Create;
End;

Destructor TTcProc.Destroy;
Begin
 FCode.Free;
 Inherited;
End;

Function DecRegLen ( ARegLen : char ) : byte;
Begin
 Result:=0;
 case ARegLen of
   'b': Result:=1;
   'w': Result:=2;
   'd': Result:=4;
 end;
End;

Function StrReplace ( Var AStr : string; Const AStrA, AStrB : string ) : boolean;
Var
  BPos  : Integer;
Begin
 Result:=FALSE;
 repeat
 BPos:=Pos(AStrA,AStr);
 if BPos=0 then break;
 Delete(AStr,BPos,Length(AStrA));
 Insert(AStrB,AStr,BPos);
 Result:=TRUE;
 until TRUE;
End;

{Const
  CRegNamesB : array [0..13] of string = ('al', 'ah', 'bl', 'bh', 'cl', 'ch', 'dl', 'dh', 'el', 'eh', 'fl', 'fh', 'gl', 'gh');
  CRegNamesW : array [0..13] of string = ('ax', 'aw', 'bx', 'bw', 'cx', 'cw', 'dx', 'dw', 'ex', 'ew', 'fx', 'fw', 'gx', 'gw');
  CRegNamesD : array [0..13] of string = ('awx', 'ar', 'bwx', 'br', 'cwx', 'cr', 'dwx', 'dr', 'ewx', 'er', 'fwx', 'fr', 'gwx', 'gr');
  CRegNamesQ : array  [0..6] of string = ('aq', 'bq', 'cq', 'dq', 'eq', 'fq', 'gq');

  CRegNamesL : array  [0..6] of string = ('al', 'bl', 'cl', 'dl', 'el', 'fl', 'gl');
  CRegNamesH : array  [0..6] of string = ('ah', 'bh', 'ch', 'dh', 'eh', 'fh', 'gh');
  CRegNamesX : array  [0..6] of string = ('ax', 'bx', 'cx', 'dx', 'ex', 'fx', 'gx');
  CRegNamesY : array  [0..6] of string = ('aw', 'bw', 'cw', 'dw', 'ew', 'fw', 'gw');
  CRegNamesE : array  [0..6] of string = ('awx', 'bwx', 'cwx', 'dwx', 'ewx', 'fwx', 'gwx');
  CRegNamesR : array  [0..6] of string = ('ar', 'br', 'cr', 'dr', 'er', 'fr', 'gr');
}
Function TTcProc.GenRegName ( ATableIdx, ARegIdx : char ) : string;
Var
  BRegIdx       : Integer;
  BRow          : char;
  BRegName      : string;
Begin
 Result:='';
 repeat
 BRegIdx:=0;
 case ARegIdx of
   'a': BRegIdx:=FRegIdxA;
   'b': BRegIdx:=FRegIdxB;
   'c': BRegIdx:=FRegIdxC;
   'd': BRegIdx:=FRegIdxD;
 end;

 BRow:=Char((BRegIdx mod 7)+Integer('a'));

 case ATableIdx of
   'b': if BRegIdx<7 then Result:=BRow+'l' else Result:=BRow+'h';
   'w': if BRegIdx<7 then Result:=BRow+'x' else Result:=BRow+'w';
   'd': if BRegIdx<7 then Result:=BRow+'wx' else Result:=BRow+'r';
   'q': Result:=BRow+'q';
   'l': Result:=BRow+'l';
   'h': Result:=BRow+'h';
   'x': Result:=BRow+'x';
   'y': Result:=BRow+'w';
   'e': Result:=BRow+'wx';
   'r': Result:=BRow+'r';
 end;

 until TRUE;
End;

Procedure TTcProc.AppendCmd ( Const ACmdS : string );
Var
  BCmdS,
  BCmdA     : string;
  BTableIdx,
  BRegIdx   : char;
  BCmpName,
  BRegName  : string;
Begin
 BCmdS:=ACmdS;

 for BTableIdx in ['b','w','d','q','l','h','x','y','e','r'] do
  begin
  for BRegIdx in ['a', 'b', 'c', 'd'] do
   begin
   BCmpName:='%r'+BTableIdx+BRegIdx;
   BRegName:=GenRegName(BTableIdx,BRegIdx);
   repeat
   if StrReplace(BCmdS,BCmpName,BRegName)=FALSE then break;
   until FALSE;
   end;
  end;

 BCmdA:='         '+ReadParamStr(BCmdS)+' '; DelFirstLastSpace(BCmdS);
 AddSpacesVarR(BCmdA,17);

 FCode.Append(BCmdA+BCmdS);
End;


Procedure TTcProc.AppendLabel ( Const AName : string );
Var
  BCmdS     : string;
Begin
 BCmdS:=AName+':';
 AddSpacesVarL(BCmdS,9);
 FCode.Append(BCmdS);
End;

Procedure TTcProc.AppendProcName ( Const AName : string );
Var
  BCmdS     : string;
Begin
 BCmdS:=AName+':';
 FCode.Append(BCmdS);
End;

Procedure TTcProc.EnterProcA ( Const ATestName : string; ADataLen : Cardinal );
Begin
 FProcNames:=FProcNames+ATestName+' ';
 AppendProcName(ATestName);
 AppendCmd('enter '+CAllRegs+','+IntToStr(ADataLen));
End;

Procedure TTcProc.LeaveProcA ( Const ATestName : string; ADataLen : Cardinal );
Begin
 AppendCmd('bra '+ATestName+'Ret');
 AppendLabel(ATestName+'Err');
 AppendCmd('nop');
 AppendCmd('trap');
 AppendCmd('nop');
 AppendLabel(ATestName+'Ret');
 AppendCmd('leave '+CAllRegs+','+IntToStr(ADataLen)+',0');
End;

Procedure TTcProc.DecOpA ( AOpcode : TTcOpA; ADataA, ADataB : Cardinal; AShiftResLen : char; Out AResult : Cardinal; Out ACmdS : string );
Var
  BShiftResLen  : byte;
Begin
 ACmdS:=''; AResult:=0;
 case AOpcode of
   toaAdd:  begin ACmdS:='add'; AResult:=ADataA+ADataB; end;
   toaSub:  begin ACmdS:='sub'; AResult:=ADataA-ADataB; end;
   toaAnd:  begin ACmdS:='and'; AResult:=ADataA and ADataB; end;
   toaOr:   begin ACmdS:='or';  AResult:=ADataA or ADataB; end;
   toaXor:  begin ACmdS:='xor'; AResult:=ADataA xor ADataB; end;
   toaMul:  begin ACmdS:='mul'; AResult:=ADataA*ADataB; end;
   toaUDiv: begin
            ACmdS:='udiv';
            if ADataB=0 then AResult:=$FFFFFFFF
            else AResult:=ADataA div ADataB;
            end;
   toaSDiv: begin
            ACmdS:='sdiv';
            if ADataB=0 then AResult:=$FFFFFFFF
            else AResult:=Cardinal(Integer(ADataA) div Integer(ADataB));
            end;
   toaURem: begin
            ACmdS:='urem';
            if ADataB=0 then AResult:=$00000000
            else AResult:=ADataA mod ADataB;
            end;
   toaSRem: begin
            ACmdS:='srem';
            if ADataB=0 then AResult:=$00000000
            else AResult:=Cardinal(Integer(ADataA) mod Integer(ADataB));
            end;
   toaShl:  begin
            ACmdS:='shl';
            AResult:=ADataA shl ADataB;
            end;
   toaShr:  begin ACmdS:='shr'; AResult:=ADataA shr ADataB; end;
   toaRol:  begin
            ACmdS:='rol';
            BShiftResLen:=DecRegLen(AShiftResLen);
            AResult:=(ADataA shl ADataB) or (ADataA shr (8*BShiftResLen-ADataB));
            end;
   toaAsr:  begin
            ACmdS:='asr';
            BShiftResLen:=DecRegLen(AShiftResLen);
            AResult:=ADataA shr ADataB;
            if (ADataA and (1 shl (8*BShiftResLen-1)))<>0 then
             begin
             AResult:=AResult or (($FFFFFFFF shr ADataB) xor $FFFFFFFF);
             end;
            end;
   else Writeln('Invalid Opcode');
 end;
End;

Procedure TTcProc.DecOpF ( AOpcode : TTcOpF; ADataA, ADataB : Single; Out AResult : Single; Out ACmdS : string );
Begin
 ACmdS:=''; AResult:=0;
 case AOpcode of
   tofAdd: begin ACmdS:='fadd'; AResult:=ADataA+ADataB; end;
   tofSub: begin ACmdS:='fsub'; AResult:=ADataA-ADataB; end;
   tofMul: begin
           ACmdS:='fmul';
           AResult:=ADataA*ADataB;
           {if (ADataA=0.0) and (ADataB=-10.0) then
            begin
            Sleep(0);
            end;}
           end;
   tofDiv: begin
           ACmdS:='fdiv';
           if IsZero(ADataB) then
            begin
            if Sign(ADataA)=Sign(ADataB) then AResult:=Infinity
            else AResult:=NegInfinity;
            end
           else AResult:=ADataA/ADataB;
           end;
   else Writeln('Invalid Opcode');
 end;
End;

Function GetMask ( ARegLen : char ) : Cardinal;
Begin
 Result:=0;
 case ARegLen of
   'b': Result:=$000000FF;
   'w': Result:=$0000FFFF;
   'd': Result:=$FFFFFFFF;
 end;
End;

Procedure TTcProc.DecRegLenUS ( ARegLen : char; ASignExt : boolean; Var AData : Cardinal; Out AHexLen : Integer; Out AMask : Cardinal );
Begin
 AHexLen:=1; AMask:=$0;
 case ARegLen of
   'b': begin
        AMask:=$000000FF;
        AData:=AData and AMask;
        if ASignExt and ((AData and $80)<>0) then AData:=AData or $FFFFFF00;
        AHexLen:=2;
        end;
   'w': begin
        AMask:=$0000FFFF;
        AData:=AData and AMask;
        if ASignExt and ((AData and $8000)<>0) then AData:=AData or $FFFF0000;
        AHexLen:=4;
        end;
   'd': begin
        AMask:=$FFFFFFFF;
        AHexLen:=8;
        end;
 end;
End;

Procedure TTcProc.DecRegLenR ( ARegLen : char; Var AData : Cardinal; Out AHexLen : Integer; Out AMask : Cardinal );
Begin
 AHexLen:=1;
 case ARegLen of
   'b': begin
        AMask:=$000000FF;
        AData:=AData and AMask;
        AHexLen:=2;
        end;
   'w': begin
        AMask:=$0000FFFF;
        AData:=AData and AMask;
        AHexLen:=4;
        end;
   'd': begin
        AMask:=$FFFFFFFF;
        AHexLen:=8;
        end;
 end;
End;

Procedure MaskShiftArg ( ARegLen : char; Var AData : Cardinal );
Begin
 case ARegLen of
   'b': AData:=AData and $7;
   'w': AData:=AData and $F;
   'd': AData:=AData and $1F;
 end;
End;

Procedure TTcProc.GenTestAluA_RI ( ARegLen : char; ADataA, ADataB : Cardinal; Const AErrLabel : string );
Var
  BOpcode   : TTcOpA;
  BDataA,
  BDataB    : Cardinal;
  BHexLenA,
  BHexLenB,
  BHexLenR  : Integer;
  BMaskA,
  BMaskB,
  BMaskR    : Cardinal;
  BCmdS     : string;
  BResult   : Cardinal;
  BExt      : boolean;
Begin
 for BOpcode in TTcOpA do
  begin
  repeat
  if BOpcode in [toaURem, toaSRem] then break;
  BDataA:=ADataA; BDataB:=ADataB; BExt:=BOpcode in [toaAdd, toaSub, toaSdiv];
  DecRegLenUS(ARegLen,BExt,BDataA,BHexLenA,BMaskA);
  DecRegLenUS(ARegLen,BExt,BDataB,BHexLenB,BMaskB);
  if BOpcode in [toaShl, toaShr, toaRol, toaAsr] then MaskShiftArg(ARegLen,BDataB);
  if BOpcode in [toaShl, toaShr, toaRol] then BDataA:=BDataA and GetMask(ARegLen);
  DecOpA(BOpcode,BDataA,BDataB,ARegLen,BResult,BCmdS);
  DecRegLenR(ARegLen,BResult,BHexLenR,BMaskR);
  AppendCmd('mov %r'+ARegLen+'a,0x'+IntToHex(BDataA and BMaskA,BHexLenA));
  AppendCmd(BCmdS+' %r'+ARegLen+'a,0x'+IntToHex(BDataB and BMaskB,BHexLenB));
  AppendCmd('cmp %r'+ARegLen+'a,0x'+IntToHex(BResult and BMaskR,BHexLenR));
  AppendCmd('bne '+AErrLabel);
  FRegIdxA:=(FRegIdxA+1) mod 14;
  until TRUE;
  end;
End;

Procedure TTcProc.GenTestAluA_RR ( ARegLen : char; ADataA, ADataB : Cardinal; Const AErrLabel : string );
Var
  BOpcode   : TTcOpA;
  BDataA,
  BDataB    : Cardinal;
  BHexLenA,
  BHexLenB,
  BHexLenR  : Integer;
  BMaskA,
  BMaskB,
  BMaskR    : Cardinal;
  BCmdS     : string;
  BResult   : Cardinal;
  BExt      : boolean;
Begin
 for BOpcode in TTcOpA do
  begin
  BDataA:=ADataA; BDataB:=ADataB; BExt:=BOpcode in [toaAdd, toaSub, toaSdiv];
  DecRegLenUS(ARegLen,BExt,BDataA,BHexLenA,BMaskA);
  DecRegLenUS(ARegLen,BExt,BDataB,BHexLenB,BMaskB);
  if BOpcode in [toaShl, toaShr, toaRol, toaAsr] then MaskShiftArg(ARegLen,BDataB);
  if BOpcode in [toaShl, toaShr, toaRol] then BDataA:=BDataA and GetMask(ARegLen);
  DecOpA(BOpcode,BDataA,BDataB,ARegLen,BResult,BCmdS);
  DecRegLenR(ARegLen,BResult,BHexLenR,BMaskR);
  AppendCmd('mov %r'+ARegLen+'a,0x'+IntToHex(BDataA and BMaskA,BHexLenA));
  AppendCmd('mov %r'+ARegLen+'b,0x'+IntToHex(BDataB and BMaskB,BHexLenB));
  AppendCmd(BCmdS+' %r'+ARegLen+'a,%r'+ARegLen+'b');
  AppendCmd('cmp %r'+ARegLen+'a,0x'+IntToHex(BResult and BMaskR,BHexLenR));
  AppendCmd('bne '+AErrLabel);
  FRegIdxA:=(FRegIdxA+1) mod 14;
  FRegIdxB:=(FRegIdxB+1) mod 14;
  end;
End;

Procedure TTcProc.GenTestArus ( Const ARegLen : string; ADataA, ADataB : Cardinal; AExt : char; Const AErrLabel : string );
Var
  BOpcode   : TTcOpA;
  BDataA,
  BDataB    : Cardinal;
  BHexLenA,
  BHexLenB,
  BHexLenR  : Integer;
  BMaskA,
  BMaskB,
  BMaskR    : Cardinal;
  BCmdS     : string;
  BResult   : Cardinal;
  BExt      : boolean;
Begin
 for BOpcode in TTcOpA do
  begin
  repeat
  if (BOpcode in [toaSdiv, toaSrem]) and (AExt='z') then break;
  if (BOpcode in [toaUdiv, toaUrem, toaShl, toaShr, toaAsr, toaRol]) and (AExt='s') then break;
  BDataA:=ADataA; BDataB:=ADataB;
  //BExt:=BOpcode in [toaAdd, toaSub, toaSdiv];
  BExt:=AExt='s';
  DecRegLenUS(ARegLen[2],BExt,BDataA,BHexLenA,BMaskA);
  DecRegLenUS(ARegLen[3],BExt,BDataB,BHexLenB,BMaskB);
  if BOpcode in [toaShl, toaShr, toaRol, toaAsr] then MaskShiftArg(ARegLen[1],BDataB);
  if BOpcode in [toaShl, toaShr, toaRol] then BDataA:=BDataA and GetMask(ARegLen[1]);
  DecOpA(BOpcode,BDataA,BDataB,ARegLen[1],BResult,BCmdS);
  BCmdS:=BCmdS+AExt+'x';
  DecRegLenR(ARegLen[1],BResult,BHexLenR,BMaskR);
  AppendCmd('mov %r'+ARegLen[2]+'a,0x'+IntToHex(BDataA and BMaskA,BHexLenA));
  AppendCmd('mov %r'+ARegLen[3]+'b,0x'+IntToHex(BDataB and BMaskB,BHexLenB));
  AppendCmd(BCmdS+' %r'+ARegLen[1]+'c,%r'+ARegLen[2]+'a,%r'+ARegLen[3]+'b');
  AppendCmd('cmp %r'+ARegLen[1]+'c,0x'+IntToHex(BResult and BMaskR,BHexLenR));
  AppendCmd('bne '+AErrLabel);
  FRegIdxA:=(FRegIdxA+1) mod 14;
  FRegIdxB:=(FRegIdxB+1) mod 14;
  FRegIdxC:=(FRegIdxC+1) mod 14;
  until TRUE;
  end;
End;

Procedure TTcProc.GenTestArui ( Const ARegLen : string; ADataA, ADataB : Cardinal; AExt : char; Const AErrLabel : string );
Var
  BOpcode   : TTcOpA;
  BDataA,
  BDataB    : Cardinal;
  BHexLenA,
  BHexLenB,
  BHexLenR  : Integer;
  BMaskA,
  BMaskB,
  BMaskR    : Cardinal;
  BCmdS     : string;
  BResult   : Cardinal;
  BExt      : boolean;
Begin
 for BOpcode in TTcOpA do
  begin
  repeat
  if (BOpcode in [toaSdiv, toaSrem]) and (AExt='z') then break;
  if (BOpcode in [toaUdiv, toaUrem, toaShl, toaShr, toaAsr, toaRol]) and (AExt='s') then break;
  BDataA:=ADataA; BDataB:=ADataB;
  BExt:=AExt='s';
  DecRegLenUS(ARegLen[2],BExt,BDataA,BHexLenA,BMaskA);
  DecRegLenUS('b',BExt,BDataB,BHexLenB,BMaskB);
  if BOpcode in [toaShl, toaShr, toaRol, toaAsr] then MaskShiftArg(ARegLen[1],BDataB);
  if BOpcode in [toaShl, toaShr, toaRol] then BDataA:=BDataA and GetMask(ARegLen[1]);
  DecOpA(BOpcode,BDataA,BDataB,ARegLen[1],BResult,BCmdS);
  BCmdS:=BCmdS+AExt+'x';
  DecRegLenR(ARegLen[1],BResult,BHexLenR,BMaskR);
  AppendCmd('mov %r'+ARegLen[2]+'a,0x'+IntToHex(BDataA and BMaskA,BHexLenA));
  AppendCmd(BCmdS+' %r'+ARegLen[1]+'c,%r'+ARegLen[2]+'a,0x'+IntToHex(BDataB and BMaskB,BHexLenB));
  AppendCmd('cmp %r'+ARegLen[1]+'c,0x'+IntToHex(BResult and BMaskR,BHexLenR));
  AppendCmd('bne '+AErrLabel);
  FRegIdxA:=(FRegIdxA+1) mod 14;
  FRegIdxB:=(FRegIdxB+1) mod 14;
  FRegIdxC:=(FRegIdxC+1) mod 14;
  until TRUE;
  end;
End;

Procedure TTcProc.GenTestBasic ( AData : Cardinal; Const AErrLabel : string );
Var
  BDataA,
  BDataB    : Cardinal;
Begin
 if (FRegIdxD and $1)<>0 then FRegIdxD:=(FRegIdxD+1) mod 14;
 // Bytes
 AppendCmd(';// Bytes');
 BDataA:=AData and $FF; BDataB:=BDataA; if (BDataB and $80)<>0 then BDataB:=BDataB or $FFFFFF00;
 AppendCmd('mov %rba,0x'+IntToHex(BDataA,2));
 AppendCmd('pushzx %rba');
 AppendCmd('pop %rdb');
 AppendCmd('cmp %rdb,0x'+IntToHex(BDataA,8));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('pushsx %rba');
 AppendCmd('pop %rdb');
 AppendCmd('cmp %rdb,0x'+IntToHex(BDataB,8));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('push 0x'+IntToHex(BDataA,2));
 AppendCmd('pop %rdb');
 AppendCmd('cmp %rdb,0x'+IntToHex(BDataA,8));
 AppendCmd('bne '+AErrLabel);
 // Words
 AppendCmd(';// Words');
 BDataA:=AData and $FFFF; BDataB:=BDataA; if (BDataB and $8000)<>0 then BDataB:=BDataB or $FFFF0000;
 AppendCmd('mov %rwa,0x'+IntToHex(BDataA,4));
 AppendCmd('pushzx %rwa');
 AppendCmd('pop %rdb');
 AppendCmd('cmp %rdb,0x'+IntToHex(BDataA,8));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('pushsx %rwa');
 AppendCmd('pop %rdb');
 AppendCmd('cmp %rdb,0x'+IntToHex(BDataB,8));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('push 0x'+IntToHex(BDataA,4));
 AppendCmd('pop %rdb');
 AppendCmd('cmp %rdb,0x'+IntToHex(BDataA,8));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('push 0x'+IntToHex(AData,8));
 AppendCmd('pop %rdb');
 AppendCmd('cmp %rdb,0x'+IntToHex(AData,8));
 AppendCmd('bne '+AErrLabel);
 // Stream byte
 AppendCmd(';// Stream Byte');
 AppendCmd('mov %rdc,esp');
 AppendCmd('mov %red,0x'+IntToHex(AData,8));
 AppendCmd('mov [%rdc++],%rld');
 AppendCmd('mov [%rdc++],%rhd'); AppendCmd('rol %red,16');
 AppendCmd('mov [%rdc++],%rld');
 AppendCmd('mov [%rdc++],%rhd');
 AppendCmd('mov %rdd,[--%rdc]');
 AppendCmd('cmp %rdd,0x'+IntToHex(AData,8));
 AppendCmd('bne '+AErrLabel);
 // Stream word
 AppendCmd(';// Stream Word');
 AppendCmd('mov %rdc,esp');
 AppendCmd('mov %red,0x'+IntToHex(AData,8));
 AppendCmd('mov [%rdc++],%rxd');
 AppendCmd('mov [%rdc++],%ryd');
 AppendCmd('mov %rdd,[--%rdc]');
 AppendCmd('cmp %rdd,0x'+IntToHex(AData,8));
 AppendCmd('bne '+AErrLabel);
 // Stream dword
 AppendCmd(';// Stream DWord');
 AppendCmd('mov %rdc,esp');
 AppendCmd('mov %rdd,0x'+IntToHex(AData,8));
 AppendCmd('mov [%rdc++],%rdd'); AppendCmd('rol %rdd,16');
 AppendCmd('mov [%rdc++],%rdd');
 AppendCmd('mov %rdd,[--%rdc]'); AppendCmd('rol %rdd,16');
 AppendCmd('cmp %rdd,0x'+IntToHex(AData,8));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('sub %rdc,4');
 AppendCmd('mov %rdd,[%rdc]');
 AppendCmd('cmp %rdd,0x'+IntToHex(AData,8));
 AppendCmd('bne '+AErrLabel);
 // Stream qword
 while (FRegIdxC mod 7)=(FRegIdxD mod 7) do FRegIdxD:=(FRegIdxD+1) mod 14;
 AppendCmd(';// Stream QWord');
 AppendCmd('mov %rdc,esp');
 AppendCmd('add %rdc,7');
 AppendCmd('and %rdc,0xFFFFFFF8');
 AppendCmd('mov %red,0x'+IntToHex(AData,8));
 AppendCmd('mov %rrd,%red');
 AppendCmd('rol %rrd,16');
 AppendCmd('mov [%rdc++],%rqd');
 AppendCmd('mov %rqd,[--%rdc]');
 AppendCmd('cmp %red,0x'+IntToHex(AData,8));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('rol %rrd,16');
 AppendCmd('cmp %rrd,0x'+IntToHex(AData,8));
 AppendCmd('bne '+AErrLabel);
 // End
 AppendCmd('');
 FRegIdxA:=(FRegIdxA+1) mod 14;
 FRegIdxB:=(FRegIdxB+1) mod 14;
 FRegIdxC:=(FRegIdxC+1) mod 14;
 FRegIdxD:=(FRegIdxD+1) mod 14;
End;


Const
  CFloatFormat = '#0.0#############';

Procedure TTcProc.GenTestF ( ADataA, ADataB : Single; Const AErrLabel : string );
Var
  BOpcode   : TTcOpF;
  BCmdS     : string;
  BResult   : Single;
  BResultD  : Cardinal absolute BResult;
  BFloatS   : string;
Begin
 for BOpcode in TTcOpF do
  begin
  DecOpF(BOpcode,ADataA,ADataB,BResult,BCmdS);
  // FRRS
  AppendCmd('mov %rda,'+FormatFloat(CFloatFormat,ADataA));
  AppendCmd('mov %rdc,%rda');
  AppendCmd('mov %rdb,'+FormatFloat(CFloatFormat,ADataB));
  AppendCmd(BCmdS+' %rda,%rdb');
  if BResultD=$80000000 then BFloatS:='-0.0' else BFloatS:=FormatFloat(CFloatFormat,BResult);
  AppendCmd('cmp %rda,'+BFloatS);
  AppendCmd('bne '+AErrLabel);
  // FRUC
  AppendCmd('mov %rda,%rdc');
  AppendCmd(BCmdS+' %rdc,'+FormatFloat(CFloatFormat,ADataB));
  AppendCmd('cmp %rdc,'+BFloatS);
  AppendCmd('bne '+AErrLabel);
  // ARUS
  AppendCmd(BCmdS+' %rdc,%rda,%rdb');
  AppendCmd('cmp %rdc,'+BFloatS);
  AppendCmd('bne '+AErrLabel);
  // AF
  if IsInfinite(BResult) then
  else
   begin
   AppendCmd('mov %rda,%rdc');
   AppendCmd('round %rda');
   AppendCmd('cmp %rda,'+IntToStr(Round(BResult)));
   AppendCmd('bne '+AErrLabel);
   AppendCmd('trunc %rdc');
   AppendCmd('cmp %rdc,'+IntToStr(Trunc(BResult)));
   AppendCmd('bne '+AErrLabel);
   AppendCmd('itf %rda');
   AppendCmd('cmp %rda,'+FormatFloat(CFloatFormat,Round(BResult)));
   AppendCmd('bne '+AErrLabel);
   AppendCmd('itf %rdc');
   AppendCmd('cmp %rdc,'+FormatFloat(CFloatFormat,Trunc(BResult)));
   AppendCmd('bne '+AErrLabel);
   end;
  // End
  AppendCmd('');
  FRegIdxA:=(FRegIdxA+1) mod 14;
  FRegIdxB:=(FRegIdxB+1) mod 14;
  FRegIdxC:=(FRegIdxC+1) mod 14;
  end;
End;

Procedure TTcProc.GenTestBTM_RB ( ADataA, ADataB : byte; Const AErrLabel : string );
Var
  BBitSet   : boolean;
  BDataRes  : byte;
Begin
 // Common
 BBitSet:=(ADataA and (1 shl ADataB))<>0;
 AppendCmd('mov %rdb,esp');
 // Memory
 // BT
 AppendCmd('mov %rba,0x'+IntToHex(ADataA,2));
 AppendCmd('mov [%rdb+'+IntToStr(ADataB and $3)+'],%rba');
 AppendCmd('bt [%rdb+'+IntToStr(ADataB and $3)+'],'+IntToStr(ADataB));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('mov %rba,[%rdb+'+IntToStr(ADataB and $3)+']');
 AppendCmd('cmp %rba,0x'+IntToHex(ADataA,2));
 AppendCmd('bne '+AErrLabel);
 // BTR
 BDataRes:=ADataA and ((1 shl ADataB) xor $FF);
 AppendCmd('mov %rba,0x'+IntToHex(ADataA,2));
 AppendCmd('mov [%rdb+'+IntToStr(ADataB and $3)+'],%rba');
 AppendCmd('btr [%rdb+'+IntToStr(ADataB and $3)+'],'+IntToStr(ADataB));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('mov %rba,[%rdb+'+IntToStr(ADataB and $3)+']');
 AppendCmd('cmp %rba,0x'+IntToHex(BDataRes,2));
 AppendCmd('bne '+AErrLabel);
 // BTS
 BDataRes:=ADataA or (1 shl ADataB);
 AppendCmd('mov %rba,0x'+IntToHex(ADataA,2));
 AppendCmd('mov [%rdb+'+IntToStr(ADataB and $3)+'],%rba');
 AppendCmd('bts [%rdb+'+IntToStr(ADataB and $3)+'],'+IntToStr(ADataB));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('mov %rba,[%rdb+'+IntToStr(ADataB and $3)+']');
 AppendCmd('cmp %rba,0x'+IntToHex(BDataRes,2));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('');
 // Register
 // BT
 AppendCmd('mov %rba,0x'+IntToHex(ADataA,2));
 AppendCmd('bt %rba,'+IntToStr(ADataB));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('cmp %rba,0x'+IntToHex(ADataA,2));
 AppendCmd('bne '+AErrLabel);
 // BTR
 BDataRes:=ADataA and ((1 shl ADataB) xor $FF);
 AppendCmd('mov %rba,0x'+IntToHex(ADataA,2));
 AppendCmd('btr %rba,'+IntToStr(ADataB));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('cmp %rba,0x'+IntToHex(BDataRes,2));
 AppendCmd('bne '+AErrLabel);
 // BTS
 BDataRes:=ADataA or (1 shl ADataB);
 AppendCmd('mov %rba,0x'+IntToHex(ADataA,2));
 AppendCmd('bts %rba,'+IntToStr(ADataB));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('cmp %rba,0x'+IntToHex(BDataRes,2));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('');
 // Ending
 FRegIdxA:=(FRegIdxA+1) mod 14;
 FRegIdxB:=(FRegIdxB+1) mod 14;
End;

Procedure TTcProc.GenTestBTR_RW ( AData : Cardinal; ABitIdx : byte; Const AErrLabel : string );
Var
  BBitSet   : boolean;
  BDataRes  : Cardinal;
Begin
 // Common
 BBitSet:=(AData and (1 shl ABitIdx))<>0;
 // BT
 AppendCmd('mov %rwa,0x'+IntToHex(AData,4));
 AppendCmd('bt %rwa,'+IntToStr(ABitIdx));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('cmp %rwa,0x'+IntToHex(AData,4));
 AppendCmd('bne '+AErrLabel);
 // BTR
 BDataRes:=AData and ((1 shl ABitIdx) xor $FFFFFFFF);
 AppendCmd('mov %rwa,0x'+IntToHex(AData,4));
 AppendCmd('btr %rwa,'+IntToStr(ABitIdx));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('cmp %rwa,0x'+IntToHex(BDataRes,4));
 AppendCmd('bne '+AErrLabel);
 // BTS
 BDataRes:=AData or (1 shl ABitIdx);
 AppendCmd('mov %rwa,0x'+IntToHex(AData,4));
 AppendCmd('bts %rwa,'+IntToStr(ABitIdx));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('cmp %rwa,0x'+IntToHex(BDataRes,4));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('');
 // Ending
 FRegIdxA:=(FRegIdxA+1) mod 14;
End;

Procedure TTcProc.GenTestBTR_RD ( AData : Cardinal; ABitIdx : byte; Const AErrLabel : string );
Var
  BBitSet   : boolean;
  BDataRes  : Cardinal;
Begin
 // Common
 BBitSet:=(AData and (1 shl ABitIdx))<>0;
 // BT
 AppendCmd('mov %rda,0x'+IntToHex(AData,8));
 AppendCmd('bt %rda,'+IntToStr(ABitIdx));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('cmp %rda,0x'+IntToHex(AData,8));
 AppendCmd('bne '+AErrLabel);
 // BTR
 BDataRes:=AData and ((1 shl ABitIdx) xor $FFFFFFFF);
 AppendCmd('mov %rda,0x'+IntToHex(AData,8));
 AppendCmd('btr %rda,'+IntToStr(ABitIdx));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('cmp %rda,0x'+IntToHex(BDataRes,8));
 AppendCmd('bne '+AErrLabel);
 // BTS
 BDataRes:=AData or (1 shl ABitIdx);
 AppendCmd('mov %rda,0x'+IntToHex(AData,8));
 AppendCmd('bts %rda,'+IntToStr(ABitIdx));
 if BBitSet then AppendCmd('bnc '+AErrLabel) else AppendCmd('bc '+AErrLabel);
 AppendCmd('cmp %rda,0x'+IntToHex(BDataRes,8));
 AppendCmd('bne '+AErrLabel);
 AppendCmd('');
 // Ending
 FRegIdxA:=(FRegIdxA+1) mod 14;
End;

Procedure TTcProc.GenTestAluA_RI_File ( ARegLen : char; Const ATestName : string; Const ADataSrc : array of Cardinal );
Var
  BIdxA,
  BIdxB     : Integer;
  BDataA,
  BDataB    : Cardinal;
Begin
 StdHeaderAsm(ATestName);
 EnterProcA(ATestName,0);
 FRegIdxA:=0;
 BIdxA:=0;
 while BIdxA<Length(ADataSrc) do
  begin
  BDataA:=ADataSrc[BIdxA];
  BIdxB:=BIdxA;
  while BIdxB<Length(ADataSrc) do
   begin
   BDataB:=ADataSrc[BIdxB];
   GenTestAluA_RI(ARegLen,BDataA,BDataB,ATestName+'Err');
   inc(BIdxB);
   end;
  inc(BIdxA);
  end;
 LeaveProcA(ATestName,0);
 SaveOpti(ATestName);
End;

Procedure TTcProc.GenTestAluA_RI_All ( Const ATestName : string );
Begin
 GenTestAluA_RI_File('b',ATestName,CSrcListB);
 GenTestAluA_RI_File('w',ATestName,CSrcListW);
 GenTestAluA_RI_File('d',ATestName,CSrcListD);
End;

Procedure TTcProc.GenTestAluA_RR_File ( ARegLen : char; Const ATestName : string; Const ADataSrc : array of Cardinal );
Var
  BIdxA,
  BIdxB     : Integer;
  BDataA,
  BDataB    : Cardinal;
Begin
 StdHeaderAsm(ATestName);
 EnterProcA(ATestName,8);
 FRegIdxA:=0; FRegIdxB:=1;
 BIdxA:=0;
 while BIdxA<Length(ADataSrc) do
  begin
  BDataA:=ADataSrc[BIdxA];
  BIdxB:=BIdxA;
  while BIdxB<Length(ADataSrc) do
   begin
   BDataB:=ADataSrc[BIdxB];
   GenTestAluA_RR(ARegLen,BDataA,BDataB,ATestName+'Err');
   inc(BIdxB);
   end;
  inc(BIdxA);
  end;
 LeaveProcA(ATestName,8);
 SaveOpti(ATestName);
End;

Procedure TTcProc.GenTestAluA_RR_All ( Const ATestName : string );
Begin
 GenTestAluA_RR_File('b',ATestName,CSrcListB);
 GenTestAluA_RR_File('w',ATestName,CSrcListW);
 GenTestAluA_RR_File('d',ATestName,CSrcListD);
End;

Procedure TTcProc.GenTestBasic_All ( Const ATestName : string; Const ADataSrc : array of Cardinal );
Var
  BIdxA     : Integer;
  BDataA    : Cardinal;
Begin
 StdHeaderAsm(ATestName);
 EnterProcA(ATestName,16);
 FRegIdxA:=0; FRegIdxB:=1; FRegIdxC:=2; FRegIdxD:=3;
 BIdxA:=0;
 while BIdxA<Length(ADataSrc) do
  begin
  BDataA:=ADataSrc[BIdxA];
  GenTestBasic(BDataA,ATestName+'Err');
  inc(BIdxA);
  end;
 LeaveProcA(ATestName,16);
 SaveOpti(ATestName);
End;

Procedure TTcProc.GenTestF_All ( Const ATestName : string; Const ADataSrc : array of Single );
Var
  BIdxA,
  BIdxB     : Integer;
  BDataA,
  BDataB    : Single;
Begin
 StdHeaderAsm(ATestName);
 EnterProcA(ATestName,0);
 FRegIdxA:=0; FRegIdxB:=1; FRegIdxC:=2;
 BIdxA:=0;
 while BIdxA<Length(ADataSrc) do
  begin
  BDataA:=ADataSrc[BIdxA];
  BIdxB:=BIdxA;
  while BIdxB<Length(ADataSrc) do
   begin
   BDataB:=ADataSrc[BIdxB];
   GenTestF(BDataA,BDataB,ATestName+'Err');
   inc(BIdxB);
   end;
  inc(BIdxA);
  end;
 LeaveProcA(ATestName,0);
 SaveOpti(ATestName);
End;

Procedure TTcProc.GenTestBTM_All ( Const ATestName : string );
Var
  BIdxA,
  BIdxB     : Integer;
  BDataA,
  BDataB    : byte;
Begin
 StdHeaderAsm(ATestName);
 EnterProcA(ATestName,4);
 FRegIdxA:=0; FRegIdxB:=1;
 BIdxA:=0;
 while BIdxA<Length(CSrcListB) do
  begin
  BDataA:=CSrcListB[BIdxA];
  BIdxB:=0;
  while BIdxB<8 do
   begin
   BDataB:=Byte(BIdxB);
   GenTestBTM_RB(BDataA,BDataB,ATestName+'Err');
   inc(BIdxB);
   end;
  inc(BIdxA);
  end;
 LeaveProcA(ATestName,4);
 SaveOpti(ATestName);
End;

Procedure TTcProc.GenTestBTR_All ( Const ATestName : string );
Var
  BTestName : string;
  BDataIdx  : Integer;
  BData     : Cardinal;
  BBitIdx   : byte;
Begin
 // Words
 BTestName:=ATestName+'_RW';
 StdHeaderAsm(BTestName);
 EnterProcA(BTestName,0);
 FRegIdxA:=0;
 BDataIdx:=0;
 while BDataIdx<Length(CSrcListW) do
  begin
  BData:=CSrcListW[BDataIdx];
  BBitIdx:=0;
  while BBitIdx<16 do
   begin
   GenTestBTR_RW(BData,BBitIdx,BTestName+'Err');
   inc(BBitIdx);
   end;
  inc(BDataIdx);
  end;
 LeaveProcA(BTestName,0);
 SaveOpti(BTestName);
 // DWords
 BTestName:=ATestName+'_RD';
 StdHeaderAsm(BTestName);
 EnterProcA(BTestName,0);
 FRegIdxA:=0;
 BDataIdx:=0;
 while BDataIdx<Length(CSrcListD) do
  begin
  BData:=CSrcListD[BDataIdx];
  BBitIdx:=0;
  while BBitIdx<32 do
   begin
   GenTestBTR_RD(BData,BBitIdx,BTestName+'Err');
   inc(BBitIdx);
   end;
  inc(BDataIdx);
  end;
 LeaveProcA(BTestName,0);
 SaveOpti(BTestName);
End;

Procedure TTcProc.GenTestArus_File ( Const ARegLen : string; AExt : char; Const ATestName : string );
Var
  BDataSrcU,
  BDataSrcS : array of cardinal;
  BIdxA,
  BIdxB     : Integer;
  BDataA,
  BDataB    : Cardinal;
Begin
 StdHeaderAsm(ATestName);
 EnterProcA(ATestName,0);
 BDataSrcU:=nil; BDataSrcS:=nil;
 case ARegLen[2] of
   'b': BDataSrcU:=CSrcListB;
   'w': BDataSrcU:=CSrcListW;
   'd': BDataSrcU:=CSrcListD;
 end;
 case ARegLen[3] of
   'b': BDataSrcS:=CSrcListB;
   'w': BDataSrcS:=CSrcListW;
   'd': BDataSrcS:=CSrcListD;
 end;
 FRegIdxA:=0; FRegIdxB:=2; FRegIdxC:=4;
 BIdxA:=0;
 while BIdxA<Length(BDataSrcU) do
  begin
  BDataA:=BDataSrcU[BIdxA];
  BIdxB:=BIdxA;
  while BIdxB<Length(BDataSrcS) do
   begin
   BDataB:=BDataSrcS[BIdxB];
   GenTestArus(ARegLen,BDataA,BDataB,AExt,ATestName+'Err');
   inc(BIdxB);
   end;
  inc(BIdxA);
  end;
 BDataSrcU:=nil; BDataSrcS:=nil;
 LeaveProcA(ATestName,0);
 SaveOpti(ATestName);
End;

Procedure TTcProc.GenTestArui_File ( Const ARegLen : string; AExt : char; Const ATestName : string );
Var
  BDataSrcU,
  BDataSrcS : array of cardinal;
  BIdxA,
  BIdxB     : Integer;
  BDataA,
  BDataB    : Cardinal;
Begin
 StdHeaderAsm(ATestName);
 EnterProcA(ATestName,0);
 BDataSrcU:=nil; BDataSrcS:=nil;
 case ARegLen[2] of
   'b': BDataSrcU:=CSrcListB;
   'w': BDataSrcU:=CSrcListW;
   'd': BDataSrcU:=CSrcListD;
 end;
 BDataSrcS:=CSrcListB;
 FRegIdxA:=0; {FRegIdxB:=2;} FRegIdxC:=4;
 BIdxA:=0;
 while BIdxA<Length(BDataSrcU) do
  begin
  BDataA:=BDataSrcU[BIdxA];
  BIdxB:=BIdxA;
  while BIdxB<Length(BDataSrcS) do
   begin
   BDataB:=BDataSrcS[BIdxB];
   GenTestArui(ARegLen,BDataA,BDataB,AExt,ATestName+'Err');
   inc(BIdxB);
   end;
  inc(BIdxA);
  end;
 BDataSrcU:=nil; BDataSrcS:=nil;
 LeaveProcA(ATestName,0);
 SaveOpti(ATestName);
End;

Procedure TTcProc.GenTestArus_All ( Const ATestName : string );
Var
  BRegR,
  BRegU,
  BRegS         : char;
  BExt          : char;
  BTestNameA    : string;
Begin
 for BExt in ['z', 's'] do
  begin
  for BRegR in ['b','w','d'] do
   begin
   for BRegU in ['b','w','d'] do
    begin
    for BRegS in ['b','w','d'] do
     begin
     BTestNameA:=BRegR+BRegU+BRegS+BExt;
     GenTestArus_File(BTestNameA,BExt,ATestName+'_'+UpperCase(BTestNameA));
     end;
    end;
   end;
  end;
End;

Procedure TTcProc.GenTestArui_All ( Const ATestName : string );
Var
  BRegR,
  BRegU         : char;
  BExt          : char;
  BTestNameA    : string;
Begin
 for BExt in ['z', 's'] do
  begin
  for BRegR in ['b','w','d'] do
   begin
   for BRegU in ['b','w','d'] do
    begin
    BTestNameA:=BRegR+BRegU+BExt;
    GenTestArui_File(BTestNameA,BExt,ATestName+'_'+UpperCase(BTestNameA));
    end;
   end;
  end;
End;

Procedure TTcProc.AddProcNames;
Var
  BProcNames    : string;
  BProcNameA    : string;
  BInsertIdx    : Integer;
Begin
 BProcNames:=FProcNames;
 BInsertIdx:=0;
 repeat
 BProcNameA:=ReadParamStr(BProcNames);
 if BProcNameA='' then break;
 FCode.Insert(BInsertIdx,'Public '+BProcNameA);
 inc(BInsertIdx);
 until FALSE;
End;

Procedure TTcProc.StdHeaderAsm ( Const ATestName : string );
Begin
 FCode.Clear;
 FCode.Append('.seg data');
 FCode.Append('        #Stack StartA, 0');
 FCode.Append('    FStackA:');
 FCode.Append('');
 FCode.Append('.seg code');
 FCode.Append('');
 FCode.Append('Start_@ep:');
 FCode.Append('StartTable:');
 FCode.Append('        dd StartA or $10000000');
 FCode.Append('        dd StartB or $10000000');
 FCode.Append('');
 FCode.Append('StartA:');
 FCode.Append('        mov     esp,FStackA');
 FCode.Append('');
 FCode.Append('        bra     '+ATestName);
 FCode.Append('');
 FCode.Append('        nop');
 FCode.Append('        test_end');
 FCode.Append('        nop');
 FCode.Append('');
 FCode.Append('  saWait:');
 FCode.Append('        bra     saWait');
 FCode.Append('');
 FCode.Append('StartB:');
 FCode.Append('        mov     awx,10');
 FCode.Append('        bra     StartB');
 FCode.Append('');
End;

Procedure TTcProc.SaveOpti ( Const ATestName : string );
Var
  BFilename : string;
Begin
 BFilename:=IncludeTrailingPathDelimiter(FPath)+ATestName+'.asm';
 repeat
 try
   FCode.SaveToFile(BFilename);
 except
   Writeln('Cannot save file '+BFilename);
   break;
 end;
 Writeln(BFilename+' is created');
 until TRUE;
End;

Procedure TTcProc.Build;
Begin
 repeat
 if ParamCount<1 then begin Writeln('Not enough parameters'); break; end;
 FPath:=ParamStr(1);

 GenTestBasic_All('TestBasic',CSrcListBasic);
 GenTestF_All('TestF',CSrcListF);
 GenTestAluA_RI_All('TestAluA_RI');
 GenTestAluA_RR_All('TestAluA_RR');
 GenTestArus_All('TestArus');
 GenTestArui_All('TestArui');
 GenTestBTM_All('TestBTM_RB');
 GenTestBTR_All('TestBTR');

 until TRUE;
End;

end.

