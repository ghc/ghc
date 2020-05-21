/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2008-2009
 *
 * Support for fast binary event logging.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "rts/EventLogWriter.h"

#include <string.h>
#include <stdio.h>
#include <fs_rts.h>
#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif

// PID of the process that writes to event_log_filename (#4512)
static pid_t event_log_pid = -1;

// File for logging events
static FILE *event_log_file = NULL;

#if defined(THREADED_RTS)
// Protects event_log_file
static Mutex event_log_mutex;

static void acquire_event_log_lock(void) { ACQUIRE_LOCK(&event_log_mutex); }
static void release_event_log_lock(void) { RELEASE_LOCK(&event_log_mutex); }
#else
static void acquire_event_log_lock(void) {}
static void release_event_log_lock(void) {}
#endif

static void initEventLogFileWriter(void);
static bool writeEventLogFile(void *eventlog, size_t eventlog_size);
static void flushEventLogFile(void);
static void stopEventLogFileWriter(void);

static char *outputFileName(void)
{

    if (RtsFlags.TraceFlags.trace_output) {
        return strdup(RtsFlags.TraceFlags.trace_output);
    } else {
        char *prog = stgMallocBytes(strlen(prog_name) + 1,
                                    "initEventLogFileWriter");
        strcpy(prog, prog_name);
#if defined(mingw32_HOST_OS)
        // on Windows, drop the .exe suffix if there is one
        {
            char *suff;
            suff = strrchr(prog,'.');
            if (suff != NULL && !strcmp(suff,".exe")) {
                *suff = '\0';
            }
        }
#endif
        char *filename = stgMallocBytes(strlen(prog)
                                        + 10 /* .%d */
                                        + 10 /* .eventlog */,
                                        "initEventLogFileWriter");

        if (event_log_pid == -1) { // #4512
            // Single process
            sprintf(filename, "%s.eventlog", prog);
            event_log_pid = getpid();
        } else {
            // Forked process, eventlog already started by the parent
            // before fork
            event_log_pid = getpid();
            // We don't have a FMT* symbol for pid_t, so we go via Word64
            // to be sure of not losing range. It would be nicer to have a
            // FMT* symbol or similar, though.
            sprintf(filename, "%s.%" FMT_Word64 ".eventlog",
                    prog, (StgWord64)event_log_pid);
        }
        stgFree(prog);
        return filename;
    }
}

static void
initEventLogFileWriter(void)
{
    char *event_log_filename = outputFileName();

    /* Open event log file for writing. */
    if ((event_log_file = __rts_fopen(event_log_filename, "wb+")) == NULL) {
        sysErrorBelch(
            "initEventLogFileWriter: can't open %s", event_log_filename);
        stg_exit(EXIT_FAILURE);
    }

    stgFree(event_log_filename);
#if defined(THREADED_RTS)
    initMutex(&event_log_mutex);
#endif
}

static bool
writeEventLogFile(void *eventlog, size_t eventlog_size)
{
    unsigned char *begin = eventlog;
    size_t remain = eventlog_size;

    acquire_event_log_lock();
    while (remain > 0) {
        size_t written = fwrite(begin, 1, remain, event_log_file);
        if (written == 0) {
            release_event_log_lock();
            return false;
        }
        remain -= written;
        begin += written;
    }
    release_event_log_lock();
    return true;
}

static void
flushEventLogFile(void)
{
    if (event_log_file != NULL) {
        fflush(event_log_file);
    }
}

static void
stopEventLogFileWriter(void)
{
    if (event_log_file != NULL) {
        fclose(event_log_file);
        event_log_file = NULL;
    }
#if defined(THREADED_RTS)
    closeMutex(&event_log_mutex);
#endif
}

const EventLogWriter FileEventLogWriter = {
    .initEventLogWriter = initEventLogFileWriter,
    .writeEventLog = writeEventLogFile,
    .flushEventLog = flushEventLogFile,
    .stopEventLogWriter = stopEventLogFileWriter
};
