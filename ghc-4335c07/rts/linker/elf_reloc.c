#include "elf_reloc.h"
#include "elf_plt.h"

#if defined(OBJFORMAT_ELF)
/* we currently only use this abstraction for elf/arm64 */
#if defined(aarch64_HOST_ARCH)
bool
relocateObjectCode(ObjectCode * oc) {
    return ADD_SUFFIX(relocateObjectCode)(oc);
}
#endif
#endif
