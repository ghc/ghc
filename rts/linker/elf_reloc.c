#include "Rts.h"
#include "elf_reloc.h"
#include "elf_plt.h"

#if defined(OBJFORMAT_ELF)

/* we currently only use this abstraction for elf/aarch64 and elf/riscv64 */
#if defined(aarch64_HOST_ARCH) | defined(riscv64_HOST_ARCH)

bool
relocateObjectCode(ObjectCode * oc) {
    return ADD_SUFFIX(relocateObjectCode)(oc);
}


void flushInstructionCache(ObjectCode * oc){
   return ADD_SUFFIX(flushInstructionCache)(oc);
}
#endif

#endif
