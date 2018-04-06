#ifndef RTS_LINKER_ELF_UTIL_H
#define RTS_LINKER_ELF_UTIL_H

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

ElfSymbolTable * findSymbolTable(ObjectCode * oc,
                                 unsigned symbolTableIndex);

ElfSymbol * findSymbol(ObjectCode * oc,
                       unsigned symbolTableIndex,
                       unsigned long symbolIndex);

#endif
#endif //RTS_LINKER_ELF_UTIL_H
