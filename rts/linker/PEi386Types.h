#pragma once

#if defined(OBJFORMAT_PEi386)

#include "ghcplatform.h"
#include "PEi386.h"
#include <stdint.h>
#include <stdio.h>

struct SectionFormatInfo {
    char* name;
    size_t alignment;
    COFF_reloc* relocs;
    uint32_t noRelocs;
    uint32_t props;
    uint64_t virtualSize;
    uint64_t virtualAddr;
 };
struct ObjectCodeFormatInfo {
    Section* init;
    Section* fini;
    Section* pdata;
    Section* xdata;
    COFF_HEADER_INFO* ch_info; // Freed by ocResolve_PEi386
    COFF_symbol* symbols;      // Freed by ocResolve_PEi386
    char* str_tab;
 };

#endif /* OBJFORMAT_PEi386.  */
