#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

#include "elf_reloc_aarch64.h"
#include "elf_reloc_riscv64.h"

RTS_PUBLIC bool
relocateObjectCode(ObjectCode * oc);

RTS_PUBLIC void flushInstructionCache(ObjectCode *oc);
#endif /* OBJETFORMAT_ELF */
