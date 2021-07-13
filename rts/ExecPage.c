/*
 * Utilities for managing dynamically-allocated executable pages.
 *
 * These are primarily used to back the adjustor code produced by the native
 * adjustor implementations.
 */

#include "Rts.h"
#include "LinkerInternals.h"
#include "sm/OSMem.h"

ExecPage *allocateExecPage() {
    ExecPage *page = (ExecPage *) mmapAnonForLinker(getPageSize());
    return page;
}

void freezeExecPage(ExecPage *page) {
    mmapForLinkerMarkExecutable(page, getPageSize());
    flushExec(getPageSize(), page);
}

void freeExecPage(ExecPage *page) {
    munmapForLinker(page, getPageSize(), "freeExecPage");
}
