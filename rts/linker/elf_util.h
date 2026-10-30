#pragma once

#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

RTS_PUBLIC ElfSymbolTable * findSymbolTable(ObjectCode * oc,
                                 unsigned symbolTableIndex);

RTS_PUBLIC ElfSymbol * findSymbol(ObjectCode * oc,
                       unsigned symbolTableIndex,
                       unsigned long symbolIndex);

#endif
