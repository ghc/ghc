#pragma once

#include <LinkerInternals.h>

#include "plt_aarch64.h"

#if defined(aarch64_HOST_ARCH)

#if defined(OBJFORMAT_MACHO)

#if defined(__x86_64__)
#define __suffix__ X86_64
#elif defined(__aarch64__) || defined(__arm64__)
#define __suffix__ Aarch64
#else
#error "unknown architecture"
#endif

#define PASTE(x,y) x ## y
#define EVAL(x,y) PASTE(x,y)
#define ADD_SUFFIX(x) EVAL(PASTE(x,),__suffix__)

unsigned numberOfStubsForSection( ObjectCode *oc, unsigned sectionIndex);

#define STUB_SIZE          ADD_SUFFIX(stubSize)

bool findStub(Section * section, void* * addr, uint8_t flags);
bool makeStub(Section * section, void* * addr, void* got_addr, uint8_t flags);

void freeStubs(Section * section);

#endif // OBJECTFORMAT_MACHO

#endif // aarch64_HOST_ARCH

