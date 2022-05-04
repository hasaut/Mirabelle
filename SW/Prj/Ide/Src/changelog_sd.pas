unit ChangeLog_sd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Const
  CVersion = $01020102;

  CChangeLog =
    '[2020.06.17] Version numeration started'+#13+
    '[2020.07.02] C.ANDI fix in ISS'+#13+
    '[2020.07.03a] Memory read FPGA is implemented'+#13+
    '[2020.07.03b] Bugfix decoder I'+#13+
    '[2020.07.04a] Dasm shift_i bug fix'+#13+
    '[2020.07.04b] FPGA successful run'+#13+
    '[2020.07.06] In ISS, after ECall/EBreak PC will stop on the next command like in FPGA'+#13+
    '[2020.07.08] ISS Sltiu bug fix'+#13+
    '[2020.07.13] ISS StepBack is implemented'+#13+
    '[2021.03.25] Start FP testing'+#13+
    '[2021.03.27] CSR counters are implemented in the simulator'+#13+
    '[2021.08.16] ISS trap hit / StepOver bugfix'+#13+
    '[2022.05.04] Format settings are localized to unify decimal separator parse'+#13;


implementation

end.

