/* --------------------------------------------------------------------------
 * $Id: mkNativeHdr.c,v 1.2 1998/12/02 13:21:50 simonm Exp $
 *
 * (c) The GHC Team, 1992-1998
 *
 * Generate a header for the native code generator
 *
 * ------------------------------------------------------------------------*/

#include "Stg.h"

#define OFFSET(table, x) ((StgUnion *) &(x) - (StgUnion *) (&table))

#define OFFSET_R1    OFFSET(RegTable, RegTable.rR1)
#define OFFSET_R2    OFFSET(RegTable, RegTable.rR2)
#define OFFSET_R3    OFFSET(RegTable, RegTable.rR3)
#define OFFSET_R4    OFFSET(RegTable, RegTable.rR4)
#define OFFSET_R5    OFFSET(RegTable, RegTable.rR5)
#define OFFSET_R6    OFFSET(RegTable, RegTable.rR6)
#define OFFSET_R7    OFFSET(RegTable, RegTable.rR7)
#define OFFSET_R8    OFFSET(RegTable, RegTable.rR8)
#define OFFSET_F1    OFFSET(RegTable, RegTable.rF1)
#define OFFSET_F2    OFFSET(RegTable, RegTable.rF2)
#define OFFSET_F3    OFFSET(RegTable, RegTable.rF3)
#define OFFSET_F4    OFFSET(RegTable, RegTable.rF4)
#define OFFSET_D1    OFFSET(RegTable, RegTable.rD1)
#define OFFSET_D2    OFFSET(RegTable, RegTable.rD2)
#define OFFSET_L1    OFFSET(RegTable, RegTable.rL1)
#define OFFSET_Sp    OFFSET(RegTable, RegTable.rSp)
#define OFFSET_Su    OFFSET(RegTable, RegTable.rSu)
#define OFFSET_SpLim OFFSET(RegTable, RegTable.rSpLim)
#define OFFSET_Hp    OFFSET(RegTable, RegTable.rHp)
#define OFFSET_HpLim OFFSET(RegTable, RegTable.rHpLim)

#define TSO_SP       OFFSET(tso, tso.sp)
#define TSO_SPLIM    OFFSET(tso, tso.splim)
#define TSO_SU       OFFSET(tso, tso.su)

StgRegTable RegTable;
StgTSO tso;

int
main()
{
    printf("-- This file is created automatically.  Do not edit by hand.\n\n");

    printf("\n-- Base table offsets for the Native Code Generator\n");

    printf("#define OFFSET_R1 %d\n", OFFSET_R1);
    printf("#define OFFSET_R2 %d\n", OFFSET_R2);
    printf("#define OFFSET_R3 %d\n", OFFSET_R3);
    printf("#define OFFSET_R4 %d\n", OFFSET_R4);
    printf("#define OFFSET_R5 %d\n", OFFSET_R5);
    printf("#define OFFSET_R6 %d\n", OFFSET_R6);
    printf("#define OFFSET_R7 %d\n", OFFSET_R7);
    printf("#define OFFSET_R8 %d\n", OFFSET_R8);
    printf("#define OFFSET_F1 %d\n", OFFSET_F1);
    printf("#define OFFSET_F2 %d\n", OFFSET_F2);
    printf("#define OFFSET_F3 %d\n", OFFSET_F3);
    printf("#define OFFSET_F4 %d\n", OFFSET_F4);
    printf("#define OFFSET_D1 %d\n", OFFSET_D1);
    printf("#define OFFSET_D2 %d\n", OFFSET_D2);
#ifdef SUPPORT_LONG_LONGS
    printf("#define OFFSET_L1 %d\n", OFFSET_L1);
#endif
    printf("#define OFFSET_Sp %d\n", OFFSET_Sp);
    printf("#define OFFSET_Su %d\n", OFFSET_Su);
    printf("#define OFFSET_SpLim %d\n", OFFSET_SpLim);
    printf("#define OFFSET_Hp %d\n", OFFSET_Hp);
    printf("#define OFFSET_HpLim %d\n", OFFSET_HpLim);

    printf("\n-- Storage Manager offsets for the Native Code Generator\n");

    printf("\n-- TSO offsets for the Native Code Generator\n");

    printf("#define TSO_SP %d\n", TSO_SP);
    printf("#define TSO_SU %d\n", TSO_SU);
    printf("#define TSO_SPLIM %d\n", TSO_SPLIM);

    printf("\n-- FILE size for the Native Code Generator\n");

    printf("#define FILE_SIZE %d\n", sizeof(*stdin));

    exit(0);
}
