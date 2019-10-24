#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

ElfSymbolTable * findSymbolTable(ObjectCode * oc,
                                 unsigned symbolTableIndex);

ElfSymbol * findSymbol(ObjectCode * oc,
                       unsigned symbolTableIndex,
                       unsigned long symbolIndex);

#endif
