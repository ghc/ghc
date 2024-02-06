#pragma once

#include <LinkerInternals.h>

#include "elf_plt_arm.h"
#include "elf_plt_aarch64.h"
#include "elf_plt_riscv64.h"

#if defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH) || defined (riscv64_HOST_ARCH)

#if defined(OBJFORMAT_ELF)

#if defined(__x86_64__)
#define __suffix__ X86_64
#elif defined(__aarch64__)
#define __suffix__ Aarch64
#elif defined(__mips64__)
#define __suffix__ Mips64
#elif defined(__i386__)
#define __suffix__ X86
#elif defined(__arm__)
#define __suffix__ Arm
#elif defined(__mips__)
#define __suffix__ Mips
#elif defined(__riscv)
#define __suffix__ RISCV64
#else
#error "unknown architecture"
#endif

#define PASTE(x,y) x ## y
#define EVAL(x,y) PASTE(x,y)
#define ADD_SUFFIX(x) EVAL(PASTE(x,),__suffix__)

unsigned  numberOfStubsForSection( ObjectCode *oc, unsigned sectionIndex);

#define STUB_SIZE          ADD_SUFFIX(stubSize)

bool findStub(Section * section, void* * addr, uint8_t flags);
bool makeStub(Section * section, void* * addr, uint8_t flags);

void freeStubs(Section * section);

#endif // OBJECTFORMAT_ELF

#endif // arm/aarch64_HOST_ARCH/riscv64_HOST_ARCH
