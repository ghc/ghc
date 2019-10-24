// This file contains relocations, for various
// architectures for the ELF format.  We include
// to not have to depend on the (limited set of)
// relocations provided by the system header.
//
// The files in ELFRelocs/ have been taken from
// the LLVM project. See ELFRelocs/LICENSE-LLVM.TXT
// for the University of Illinois Open Source License
// under which it is distrubuted.
//

#pragma once

#define PASTE(x,y) x ## y
#define EVAL(x,y) PASTE(x,y)

// generate COMPAT_R_<ARCH>_<REL> relocations to
// prevent clashes with definitions in <elf.h>.
#define ELF_RELOC(name, value)   PASTE(COMPAT_,name) = value,

enum RelocAarch64 {
#include "ELFRelocs/AArch64.def"
};
enum RelocARM {
#include "ELFRelocs/ARM.def"
};
enum Reloci386 {
#include "ELFRelocs/i386.def"
};
enum RelocX86_64 {
#include "ELFRelocs/x86_64.def"
};
