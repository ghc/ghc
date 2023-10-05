#pragma once

#include <LinkerInternals.h>

#if defined(OBJFORMAT_MACHO)

#if defined(x86_64_HOST_ARCH)
#  include <mach-o/x86_64/reloc.h>
#endif

#if defined(aarch64_HOST_ARCH)
#  include <mach-o/arm64/reloc.h>
#endif

#include "../MachOTypes.h"


extern const size_t stubSizeAarch64;
bool needStubForRelAarch64(MachORelocationInfo * rel);
bool makeStubAarch64(Stub * s);

#endif

