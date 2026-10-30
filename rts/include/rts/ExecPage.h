/*
 * Utilities for managing dynamically-allocated executable pages.
 */

#pragma once

typedef struct {
    char contents;
} ExecPage;

/* Allocate a writable page. */
RTS_PUBLIC ExecPage *allocateExecPage(void);

/* Make a page previously allocated by allocateExecPage. */
RTS_PUBLIC void freezeExecPage(ExecPage *page);

/* Free a page previously allocated by allocateExecPage. */
RTS_PUBLIC void freeExecPage(ExecPage *page);
