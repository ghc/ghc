/*
 * Utilities for managing dynamically-allocated executable pages.
 *
 * These are primarily used to back the adjustor code produced by the native
 * adjustor implementations.
 */

#include "Rts.h"
#include "sm/OSMem.h"
#include "linker/MMap.h"

ExecPage *allocateExecPage(void) {
#if defined(wasm32_HOST_ARCH)
    return NULL;
#else
    ExecPage *page = (ExecPage *) mmapAnon(getPageSize());
    return page;
#endif
}

void freezeExecPage(ExecPage *page) {
#if !defined(wasm32_HOST_ARCH)
    mprotectForLinker(page, getPageSize(), MEM_READ_EXECUTE);
    flushExec(getPageSize(), page);
#endif
}

void freeExecPage(ExecPage *page) {
#if !defined(wasm32_HOST_ARCH)
    munmapForLinker(page, getPageSize(), "freeExecPage");
#endif
}
