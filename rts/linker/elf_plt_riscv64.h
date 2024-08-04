#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

extern const size_t stubSizeRISCV64;
bool needStubForRelRISCV64(Elf_Rel * rel);
bool needStubForRelaRISCV64(Elf_Rela * rel);
bool makeStubRISCV64(Stub * s);

#endif
