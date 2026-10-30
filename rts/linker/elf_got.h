#pragma once

#include "LinkerInternals.h"

#include <stdbool.h>
#include <linker/ElfTypes.h>

#if defined(OBJFORMAT_ELF)
RTS_PUBLIC bool needGotSlot(Elf_Sym * symbol);
RTS_PUBLIC bool makeGot(ObjectCode * oc);
RTS_PUBLIC bool fillGot(ObjectCode * oc);
RTS_PUBLIC bool verifyGot(ObjectCode * oc);
RTS_PUBLIC void freeGot(ObjectCode * oc);
#endif
