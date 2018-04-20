#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

extern const size_t stubSizeArm;
bool needStubForRelArm(Elf_Rel * rel);
bool needStubForRelaArm(Elf_Rela * rel);
bool makeStubArm(Stub * s);

#endif
