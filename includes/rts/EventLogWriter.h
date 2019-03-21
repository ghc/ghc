/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2017
 *
 * Support for fast binary event logging.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stddef.h>
#include <stdbool.h>

/*
 *  Abstraction for writing eventlog data.
 */
typedef struct {
    // Initialize an EventLogWriter (may be NULL)
    void (* initEventLogWriter) (void);

    // Write a series of events
    bool (* writeEventLog) (void *eventlog, size_t eventlog_size);

    // Flush possibly existing buffers (may be NULL)
    void (* flushEventLog) (void);

    // Close an initialized EventLogOutput (may be NULL)
    void (* stopEventLogWriter) (void);
} EventLogWriter;

/*
 * An EventLogWriter which writes eventlogs to
 * a file `program.eventlog`.
 */
extern const EventLogWriter FileEventLogWriter;
