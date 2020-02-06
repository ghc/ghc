#pragma once

#include "LinkerInternals.h"

#include <stdbool.h>
#include <linker/ElfTypes.h>

#if defined(OBJFORMAT_ELF)
bool needGotSlot(Elf_Sym * symbol);
bool makeGot(ObjectCode * oc);
bool fillGot(ObjectCode * oc);
bool verifyGot(ObjectCode * oc);
void freeGot(ObjectCode * oc);
#endif
