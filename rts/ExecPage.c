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
    ExecPage *page = (ExecPage *) mmapAnon(getPageSize());
    return page;
}

void freezeExecPage(ExecPage *page) {
    mprotectForLinker(page, getPageSize(), MEM_READ_EXECUTE);
    flushExec(getPageSize(), page);
}

void freeExecPage(ExecPage *page) {
    munmapForLinker(page, getPageSize(), "freeExecPage");
}
