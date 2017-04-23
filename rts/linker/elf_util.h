#ifndef RTS_LINKER_ELF_UTIL_H
#define RTS_LINKER_ELF_UTIL_H

#include <stdint.h>
#include <stdbool.h>
#include "LinkerInternals.h"

#if defined(OBJFORMAT_ELF)

ElfSymbolTable * find_symbol_table(ObjectCode * oc,
                                   unsigned symbolTableIndex);

ElfSymbol * find_symbol(ObjectCode * oc,
                        unsigned symbolTableIndex,
                        unsigned long symbolIndex);

#endif
#endif //RTS_LINKER_ELF_UTIL_H
