/*
 * Utilities for managing dynamically-allocated executable pages.
 */

#pragma once

#ifndef RTS_EXPORT
# define RTS_EXPORT
#endif

typedef struct {
    char contents;
} ExecPage;

/* Allocate a writable page. */
RTS_EXPORT ExecPage *allocateExecPage(void);

/* Make a page previously allocated by allocateExecPage. */
RTS_EXPORT void freezeExecPage(ExecPage *page);

/* Free a page previously allocated by allocateExecPage. */
RTS_EXPORT void freeExecPage(ExecPage *page);
