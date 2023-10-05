#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

extern const size_t stubSizeAarch64;
bool needStubForRelAarch64(Elf_Rel * rel);
bool needStubForRelaAarch64(Elf_Rela * rel);
bool makeStubAarch64(Stub * s);

#endif
