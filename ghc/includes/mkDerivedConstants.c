/* --------------------------------------------------------------------------
 * $Id: mkDerivedConstants.c,v 1.4 2002/12/11 15:36:40 simonmar Exp $
 *
 * (c) The GHC Team, 1992-1998
 *
 * Generate a header for the native code generator
 *
 * ------------------------------------------------------------------------*/

#define IN_STG_CODE 0
#include "Stg.h"

#define OFFSET(s_type, field) ((unsigned int)&(((s_type*)0)->field))

int
main(int argc, char *argv[])
{
    printf("-- This file is created automatically.  Do not edit by hand.\n\n");

    printf("#define STD_HDR_SIZE   %d\n", sizeofW(StgHeader));
    printf("#define PROF_HDR_SIZE  %d\n", sizeofW(StgProfHeader));
    printf("#define GRAN_HDR_SIZE  %d\n", sizeofW(StgGranHeader));

    printf("#define ARR_WORDS_HDR_SIZE  %d\n", 
	   sizeofW(StgArrWords) - sizeofW(StgHeader));

    printf("#define ARR_PTRS_HDR_SIZE   %d\n", 
	   sizeofW(StgMutArrPtrs) - sizeofW(StgHeader));

    printf("#define STD_ITBL_SIZE   %d\n", sizeofW(StgInfoTable));
    printf("#define RET_ITBL_SIZE   %d\n", sizeofW(StgRetInfoTable) - sizeofW(StgInfoTable));
    printf("#define PROF_ITBL_SIZE  %d\n", sizeofW(StgProfInfo));
    printf("#define GRAN_ITBL_SIZE  %d\n", 0);
    printf("#define TICKY_ITBL_SIZE %d\n", sizeofW(StgTickyInfo));

    printf("#define STD_UF_SIZE   %d\n", sizeofW(StgUpdateFrame));
    printf("#define GRAN_UF_SIZE   %d\n",  
	   sizeofW(StgUpdateFrame) + sizeofW(StgGranHeader));
    printf("#define PROF_UF_SIZE   %d\n",  
	   sizeofW(StgUpdateFrame) + sizeofW(StgProfHeader));

    printf("#define UF_RET     %d\n",
	   OFFSET(StgUpdateFrame,header.info));

    printf("#define UF_UPDATEE %d\n",
	   OFFSET(StgUpdateFrame,updatee) / sizeof(W_));

    printf("#define BLOCK_SIZE   %d\n", BLOCK_SIZE);
    printf("#define MBLOCK_SIZE   %d\n", MBLOCK_SIZE);  
    return 0;
}
