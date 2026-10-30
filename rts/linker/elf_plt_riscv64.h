#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

extern RTS_PUBLIC const size_t stubSizeRISCV64;
RTS_PUBLIC bool needStubForRelRISCV64(Elf_Rel * rel);
RTS_PUBLIC bool needStubForRelaRISCV64(Elf_Rela * rel);
RTS_PUBLIC bool makeStubRISCV64(Stub * s);

#endif
