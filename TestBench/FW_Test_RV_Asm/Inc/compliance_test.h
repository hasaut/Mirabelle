// RISC-V Compliance Test Header File
// Copyright (c) 2017, Codasip Ltd. All Rights Reserved.
// See LICENSE for license details.
//
// Description: Common header file for RV32I tests

#ifndef _COMPLIANCE_TEST_H
#define _COMPLIANCE_TEST_H

#include "riscv_test.h"

//-----------------------------------------------------------------------
// RV Compliance Macros
//-----------------------------------------------------------------------

#define RV_COMPLIANCE_HALT                                              \
        RVTEST_SYNC;                                                    \
        li TESTNUM, 1;                                                  \
        SWSIG (0, TESTNUM);                                             \
        ecall;                                                          \
        nop;                                                            \
        unimp                                                           \

#define RV_COMPLIANCE_RV32M                                                   \
                                                                              \

#define RV_COMPLIANCE_CODE_BEGIN                                              \
        .section .text.init;                                                  \
        .align  4;                                                            \
        .global _start;                                                       \
        .global _main_1;                                                      \
        .global _main_2;                                                      \
_start:                                                                       \
        .word _main_1;                                                        \
        .word _main_2;                                                        \
_main_2:                                                                      \
        j .          ;                                                        \
_main_1:                                                                      \
begin_testcode:


#define RV_COMPLIANCE_CODE_END                                                \
                                                                              \

#define RV_COMPLIANCE_DATA_BEGIN                                        \
        EXTRA_DATA                                                      \
        .align 4; .global begin_signature; begin_signature:

#define RV_COMPLIANCE_DATA_END                                          \
        .align 4; .global end_signature; end_signature:                 \

#endif
