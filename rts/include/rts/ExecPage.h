/*
 * Utilities for managing dynamically-allocated executable pages.
 */

#pragma once

typedef struct {
    char contents;
} ExecPage;

/* Allocate a writable page. */
ExecPage *allocateExecPage(void);

/* Make a page previously allocated by allocateExecPage. */
void freezeExecPage(ExecPage *page);

/* Free a page previously allocated by allocateExecPage. */
void freeExecPage(ExecPage *page);
