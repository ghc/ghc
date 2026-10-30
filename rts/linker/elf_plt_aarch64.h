#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

extern RTS_PUBLIC const size_t stubSizeAarch64;
RTS_PUBLIC bool needStubForRelAarch64(Elf_Rel * rel);
RTS_PUBLIC bool needStubForRelaAarch64(Elf_Rela * rel);
RTS_PUBLIC bool makeStubAarch64(Stub * s);

#endif
