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

    // Write a series of events returning true on success.
    // Note that this may be called by multiple threads simultaneously.
    // The writer is responsible for concurrency control.
    bool (* writeEventLog) (void *eventlog, size_t eventlog_size);

    // Flush possibly existing buffers (may be NULL)
    // Note that this may be called by multiple threads simultaneously.
    // The writer is responsible for concurrency control.
    void (* flushEventLog) (void);

    // Close an initialized EventLogOutput (may be NULL)
    void (* stopEventLogWriter) (void);
} EventLogWriter;

/*
 * An EventLogWriter which writes eventlogs to
 * a file `program.eventlog`.
 */
extern const EventLogWriter FileEventLogWriter;

enum EventLogStatus {
  /* The runtime system wasn't compiled with eventlog support. */
  EVENTLOG_NOT_SUPPORTED,
  /* An EventLogWriter has not yet been configured */
  EVENTLOG_NOT_CONFIGURED,
  /* An EventLogWriter has been configured and is running. */
  EVENTLOG_RUNNING,
};

/*
 * Query whether the current runtime system supports eventlogging.
 */
enum EventLogStatus eventLogStatus(void);

/*
 * Initialize event logging using the given EventLogWriter.
 * Returns true on success or false if an EventLogWriter is already configured
 * or eventlogging isn't supported by the runtime.
 */
bool startEventLogging(const EventLogWriter *writer);

/*
 * Stop event logging and destroy the current EventLogWriter.
 */
void endEventLogging(void);
