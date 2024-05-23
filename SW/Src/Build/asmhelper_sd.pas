unit AsmHelper_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Const
  CCmdNamesMs    = 'push pop pushl popl enter leave '+
                   'bra bany bb bbe bc bnc bz be bnz bne ba bae bg bge bs bse bv bnv bn bne '+
                   'call '+
                   'swt swt_b swt_be swt_c swt_nc swt_z swt_e swt_nz swt_ne swt_a swt_ae swt_g swt_ge swt_s swt_se swt_v swt_nv swt_n swt_ne '+
                   'add sub mov cmp and or test xor shl shr rol asr '+
                   'addsx subsx movsx mulsx udivsx sdivsx addzx subzx movzx mulzx udivzx sdivzx '+
                   'shlzx shrzx rolzx asrzx shlsx shrsx rolsx asrsx '+
                   'urem srem '+
                   'inc dec nop trap getfl setfl udiv sdiv mul fadd fsub fmul fdiv itf trunc round '+
                   'info siend silock siunlock siconf pushzx pushsx bt btr bts btx';
  CCmdNamesRV    = 'swt ecall eret auipc fence add sub sll slt sltu xor srl sra or and addi slti sltiu xori ori andi slli srli srai '+
                   'jal jalr lui beq bne blt bge bltu bgeu '+
                   'mul mulh mulhsu mulhu div divu rem remu '+
                   'fadd fsub fmul fdiv fcvt feq flt fle fsgnj fsgnjn fsgnjx '+
                   'amoswap amoadd amoxor amoand amoor';
  CCmdPseudoRV   = 'ret lb lh lw lbu lhu sb sh sw jr call li la mv j beqz bnez bltz bgez blez bgtu bgt ble bgtu bleu trap nop snez sgtz sltz seqz tail neg';

implementation

end.

