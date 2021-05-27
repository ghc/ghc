/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2014-2015
 *
 * Producing DWARF-based stacktraces with libdw.
 *
 * --------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "Libdw.h"

#if USE_LIBDW

#include <elfutils/libdwfl.h>
#include <dwarf.h>
#include <unistd.h>
#include <semaphore.h>

const int max_backtrace_depth = 5000;

static BacktraceChunk *backtraceAllocChunk(BacktraceChunk *next) {
    BacktraceChunk *chunk = stgMallocBytes(sizeof(BacktraceChunk),
                                           "backtraceAllocChunk");
    chunk->n_frames = 0;
    chunk->next = next;
    return chunk;
}

// Allocate a Backtrace
static Backtrace *backtraceAlloc(void) {
    Backtrace *bt = stgMallocBytes(sizeof(Backtrace), "backtraceAlloc");
    bt->n_frames = 0;
    bt->last = backtraceAllocChunk(NULL);
    return bt;
}

static void backtracePush(Backtrace *bt, StgPtr pc) {
    // Is this chunk full?
    if (bt->last->n_frames == BACKTRACE_CHUNK_SZ)
        bt->last = backtraceAllocChunk(bt->last);

    // Push the PC
    bt->last->frames[bt->last->n_frames] = pc;
    bt->last->n_frames++;
    bt->n_frames++;
}

void backtraceFree(Backtrace *bt) {
    if (bt == NULL)
        return;
    BacktraceChunk *chunk = bt->last;
    while (chunk != NULL) {
        BacktraceChunk *next = chunk->next;
        stgFree(chunk);
        chunk = next;
    }
    stgFree(bt);
}

struct LibdwSession_ {
    Dwfl *dwfl;

    // The current backtrace we are collecting (if any)
    Backtrace *cur_bt;
    int max_depth;
};

static const Dwfl_Thread_Callbacks thread_cbs;

void libdwFree(LibdwSession *session) {
    if (session == NULL)
        return;
    dwfl_end(session->dwfl);
    stgFree(session);
}

// Create a libdw session with DWARF information for all loaded modules
LibdwSession *libdwInit() {
    LibdwSession *session = stgCallocBytes(1, sizeof(LibdwSession),
                                           "libdwInit");
    // Initialize ELF library
    if (elf_version(EV_CURRENT) == EV_NONE) {
        sysErrorBelch("libelf version too old!");
        return NULL;
    }

    // Initialize a libdwfl session
    static char *debuginfo_path;
    static const Dwfl_Callbacks proc_callbacks =
        {
            .find_debuginfo = dwfl_standard_find_debuginfo,
            .debuginfo_path = &debuginfo_path,
            .find_elf = dwfl_linux_proc_find_elf,
        };
    session->dwfl = dwfl_begin (&proc_callbacks);
    if (session->dwfl == NULL) {
        sysErrorBelch("dwfl_begin failed: %s", dwfl_errmsg(dwfl_errno()));
        free(session);
        return NULL;
    }

    // Report the loaded modules
    int ret = dwfl_linux_proc_report(session->dwfl, getpid());
    if (ret < 0) {
        sysErrorBelch("dwfl_linux_proc_report failed: %s",
                      dwfl_errmsg(dwfl_errno()));
        goto fail;
    }
    if (dwfl_report_end (session->dwfl, NULL, NULL) != 0) {
        sysErrorBelch("dwfl_report_end failed: %s", dwfl_errmsg(dwfl_errno()));
        goto fail;
    }

    pid_t pid = getpid();
    if (! dwfl_attach_state(session->dwfl, NULL, pid, &thread_cbs, NULL)) {
        sysErrorBelch("dwfl_attach_state failed: %s",
                      dwfl_errmsg(dwfl_errno()));
        goto fail;
    }

    return session;

 fail:
    dwfl_end(session->dwfl);
    free(session);
    return NULL;
}

int libdwLookupLocation(LibdwSession *session, Location *frame,
                        StgPtr pc) {
    Dwarf_Addr addr = (Dwarf_Addr) (uintptr_t) pc;
    // Find the module containing PC
    Dwfl_Module *mod = dwfl_addrmodule(session->dwfl, addr);
    if (mod == NULL)
        return 1;
    // avoid unaligned pointer value
    // Using &frame->object_file as argument to dwfl_module_info leads to
    //
    //   error: taking address of packed member of ‘struct Location_’ may result in an unaligned pointer value [-Werror=address-of-packed-member]
    //
    void *object_file = &frame->object_file;
    dwfl_module_info(mod, NULL, NULL, NULL, NULL, NULL,
                     object_file, NULL);

    // Find function name
    frame->function = dwfl_module_addrname(mod, addr);

    // Try looking up source location
    Dwfl_Line *line = dwfl_module_getsrc(mod, addr);
    if (line != NULL) {
        Dwarf_Addr addr;
        int lineno, colno;
        /* libdwfl owns the source_file buffer, don't free it */
        frame->source_file = dwfl_lineinfo(line, &addr, &lineno,
                                           &colno, NULL, NULL);
        frame->lineno = lineno;
        frame->colno = colno;
    }

    if (line == NULL || frame->source_file == NULL) {
        frame->source_file = NULL;
        frame->lineno = 0;
        frame->colno = 0;
    }
    return 0;
}

int libdwForEachFrameOutwards(Backtrace *bt,
                              int (*cb)(StgPtr, void*),
                              void *user_data)
{
    int n_chunks = bt->n_frames / BACKTRACE_CHUNK_SZ;
    if (bt->n_frames % BACKTRACE_CHUNK_SZ != 0)
        n_chunks++;

    BacktraceChunk **chunks =
        stgMallocBytes(n_chunks * sizeof(BacktraceChunk *),
                       "libdwForEachFrameOutwards");

    // First build a list of chunks, ending with the inner-most chunk
    int chunk_idx;
    chunks[0] = bt->last;
    for (chunk_idx = 1; chunk_idx < n_chunks; chunk_idx++) {
        chunks[chunk_idx] = chunks[chunk_idx-1]->next;
    }

    // Now iterate back through the frames
    int res = 0;
    for (chunk_idx = n_chunks-1; chunk_idx >= 0 && res == 0; chunk_idx--) {
        unsigned int i;
        BacktraceChunk *chunk = chunks[chunk_idx];
        for (i = 0; i < chunk->n_frames; i++) {
            res = cb(chunk->frames[i], user_data);
            if (res != 0) break;
        }
    }
    free(chunks);
    return res;
}

struct PrintData {
    LibdwSession *session;
    FILE *file;
};

static int printFrame(StgPtr pc, void *cbdata)
{
    struct PrintData *pd = (struct PrintData *) cbdata;
    Location loc;
    libdwLookupLocation(pd->session, &loc, pc);
    fprintf(pd->file, "  %24p    %s ",
            (void*) pc, loc.function);
    if (loc.source_file)
        fprintf(pd->file, "(%s:%d.%d)\n",
                loc.source_file, loc.lineno, loc.colno);
    else
        fprintf(pd->file, "(%s)\n", loc.object_file);
    return 0;
}

void libdwPrintBacktrace(LibdwSession *session, FILE *file, Backtrace *bt) {
    if (bt == NULL) {
        fprintf(file, "Warning: tried to print failed backtrace\n");
        return;
    }

    struct PrintData pd = { session, file };
    libdwForEachFrameOutwards(bt, printFrame, &pd);
}

// Remember that we are traversing from the inner-most to the outer-most frame
static int getBacktraceFrameCb(Dwfl_Frame *frame, void *arg) {
    LibdwSession *session = arg;
    Dwarf_Addr pc;
    bool is_activation;
    if (! dwfl_frame_pc(frame, &pc, &is_activation)) {
        // failed to find PC
        backtracePush(session->cur_bt, 0x0);
    } else {
        // see https://sourceware.org/git/?p=elfutils.git;a=blob;f=src/stack.c;h=534aa93c433551896b67b65845ab4891fd175066;hb=HEAD#l376
        Dwarf_Addr pc_adjusted = pc - (is_activation ? 0 : 1);
        backtracePush(session->cur_bt, (StgPtr) (uintptr_t) pc_adjusted);
    }
    session->max_depth--;
    if (session->max_depth == 0) {
        return DWARF_CB_ABORT;
    } else {
        return DWARF_CB_OK;
    }
}

Backtrace *libdwGetBacktrace(LibdwSession *session) {
    if (session->cur_bt != NULL) {
        sysErrorBelch("Already collecting backtrace. Uh oh.");
        return NULL;
    }

    Backtrace *bt = backtraceAlloc();
    session->cur_bt = bt;
    session->max_depth = max_backtrace_depth;

    int pid = getpid();
    int ret = dwfl_getthread_frames(session->dwfl, pid,
                                    getBacktraceFrameCb, session);
    if (ret == -1)
        sysErrorBelch("Failed to get stack frames of current process: %s",
                      dwfl_errmsg(dwfl_errno()));

    session->cur_bt = NULL;
    return bt;
}

static pid_t next_thread(Dwfl *dwfl, void *arg, void **thread_argp) {
    /* there is only the current thread */
    if (*thread_argp != NULL)
        return 0;

    *thread_argp = arg;
    return dwfl_pid(dwfl);
}

static bool memory_read(Dwfl *dwfl STG_UNUSED, Dwarf_Addr addr,
                        Dwarf_Word *result, void *arg STG_UNUSED) {
    *result = *(Dwarf_Word *) (uintptr_t) addr;
    return true;
}

static bool set_initial_registers(Dwfl_Thread *thread, void *arg);

#if defined(x86_64_HOST_ARCH)
static bool set_initial_registers(Dwfl_Thread *thread,
                                  void *arg STG_UNUSED) {
    Dwarf_Word regs[17];
    __asm__ ("movq %%rax, 0x00(%0)\n\t"
             "movq %%rdx, 0x08(%0)\n\t"
             "movq %%rcx, 0x10(%0)\n\t"
             "movq %%rbx, 0x18(%0)\n\t"
             "movq %%rsi, 0x20(%0)\n\t"
             "movq %%rdi, 0x28(%0)\n\t"
             "movq %%rbp, 0x30(%0)\n\t"
             "movq %%rsp, 0x38(%0)\n\t"
             "movq %%r8,  0x40(%0)\n\t"
             "movq %%r9,  0x48(%0)\n\t"
             "movq %%r10, 0x50(%0)\n\t"
             "movq %%r11, 0x58(%0)\n\t"
             "movq %%r12, 0x60(%0)\n\t"
             "movq %%r13, 0x68(%0)\n\t"
             "movq %%r14, 0x70(%0)\n\t"
             "movq %%r15, 0x78(%0)\n\t"
             "lea 0(%%rip), %%rax\n\t"
             "movq %%rax, 0x80(%0)\n\t"
             :                            /* no output */
             :"r" (&regs[0])              /* input */
             :"%rax"                      /* clobbered */
        );
    return dwfl_thread_state_registers(thread, 0, 17, regs);
}
#elif defined(i386_HOST_ARCH)
static bool set_initial_registers(Dwfl_Thread *thread,
                                  void *arg STG_UNUSED) {
    Dwarf_Word regs[9];
    __asm__ ("movl %%eax, 0x00(%0)\n\t"
             "movl %%ecx, 0x04(%0)\n\t"
             "movl %%edx, 0x08(%0)\n\t"
             "movl %%ebx, 0x0c(%0)\n\t"
             "movl %%esp, 0x10(%0)\n\t"
             "movl %%ebp, 0x14(%0)\n\t"
             "movl %%esp, 0x18(%0)\n\t"
             "movl %%edi, 0x1c(%0)\n\t"
             "here:\n\t"
             "movl here,  %%eax\n\t"
             "movl %%eax, 0x20(%0)\n\t"
             :                            /* no output */
             :"r" (&regs[0])              /* input */
             :"%eax"                      /* clobbered */
        );
    return dwfl_thread_state_registers(thread, 0, 9, regs);
}
#elif defined(s390x_HOST_ARCH)
static bool set_initial_registers(Dwfl_Thread *thread,
                                  void *arg STG_UNUSED) {
    Dwarf_Word regs[32];
    __asm__ ("stmg %%r0,%%r15,0(%0)\n\t"
             "std  %%f0,  128(0,%0)\n\t"
             "std  %%f2,  136(0,%0)\n\t"
             "std  %%f4,  144(0,%0)\n\t"
             "std  %%f6,  152(0,%0)\n\t"
             "std  %%f1,  160(0,%0)\n\t"
             "std  %%f3,  168(0,%0)\n\t"
             "std  %%f5,  176(0,%0)\n\t"
             "std  %%f7,  184(0,%0)\n\t"
             "std  %%f8,  192(0,%0)\n\t"
             "std  %%f10, 200(0,%0)\n\t"
             "std  %%f12, 208(0,%0)\n\t"
             "std  %%f14, 216(0,%0)\n\t"
             "std  %%f9,  224(0,%0)\n\t"
             "std  %%f11, 232(0,%0)\n\t"
             "std  %%f13, 240(0,%0)\n\t"
             "std  %%f15, 248(0,%0)\n\t"
             "larl %%r0,0\n\t"
             "stg  %%r0,  112(0,%0)\n\t"
             :                            /* no output */
             :"r" (&regs[0])              /* input */
             :"%r0"                       /* clobbered */
        );
    return dwfl_thread_state_registers(thread, 0, 32, regs);
}
#else
#    error "Please implement set_initial_registers() for your arch"
#endif

static const Dwfl_Thread_Callbacks thread_cbs = {
    .next_thread = next_thread,
    .memory_read = memory_read,
    .set_initial_registers = set_initial_registers,
};


#define BTQ_MAX_DEPTH 50 // TODO adjust or turn into a config

/* Minimum information (just Dwarf_Addr of each frame, to infer source function
 * name and location etc.) of statically limited capacity for a backtrace, with
 * next ptr to form a singly linked list as a queue of backtraces.
 *
 * Meant to be statically allocated with thread-local storage, then filled
 * from a signal handler, where heap allocation is unsafe.
 */
struct BtQue {
    pid_t tid;
    int dwflerr;
    int nFrames;
    Dwarf_Addr framePCs[BTQ_MAX_DEPTH];
    struct BtQue *next; // to form a list as the print queue
    int btPrintNum; // to avoid duplicated printing
};

// globle lock-free list of backtraces pending to be printed out
static struct BtQue *btqPending;
// btqSema reflects those many backtraces posted to btqPending
static sem_t btqSema;

static void* OSThreadProcAttr
btPrintThread(void *_ STG_UNUSED)
{
    LibdwSession *session = libdwThreadSession(true);
    if(!session) {
        sysErrorBelch("Failed to create libdw session on printer thread,"
            " backtrace will not be printed on SIGQUIT signal.");
        return NULL;
    }

    for(int printCntr=1; ; printCntr++) { // it is okay printCntr to overflow
        sem_wait(&btqSema); // go idle when there's no backtrace to print
        for(int i=0; i<32; i++) { // TODO tune this number or improve the algo
            // move out of the way for propagated SIGQUIT signals to have their
            // respective threads capture their bts then enlist to btqPending,
            // in a short burst
            sched_yield();
        }
        struct BtQue *last = NULL;
        // take the whole list concurrently with many threads (hopefully done)
        // adding their respective backtrace in responding to SIGQUIT
        for(;;) {
            last = SEQ_CST_LOAD(&btqPending);
            if(!last) {
                // this is possible as btqSema is counting each backtrace as
                // inserted, while we are consuming the full list at a time
                break;
            }
            if( last == cas(&btqPending, last, NULL) ) {
                break;
            }
        }
        if(!last) {
            continue;
        }
        fprintf(stderr, "\nCaught SIGQUIT:\n");
        // we are printing backtraces from the list in LIFO fashion,
        // intuitively it is better FIFO, but it does not matter that much
        for(struct BtQue *curr=last; curr; curr = curr->next) {
            // avoid duplicate printing of a same bt record. each thread has
            // its local storage, will be overwritten from time to time, then
            // if we are so slow to have not printed a previous snapshot in
            // time, we'll print out the latest content at first sight of a
            // thread's bt record, then it's meaningless to print it for
            // subsequent occurrences of the record struct in btqPending list
            // TODO need memory barriers for btPrintNum field access?
            if( curr->btPrintNum == printCntr ) {
                continue;
            } else {
                curr->btPrintNum = printCntr;
            }
            // print out, this is on a normal thread, so async-signal-safety
            // is no longer a concern
            fprintf(stderr, "\n * Backtrace of thread %d with %d frame(s):\n",
                    curr->tid, curr->nFrames);
            if(curr->dwflerr) {
                fprintf(stderr, " *-* with dwfl error (%d): %s\n",
                        curr->dwflerr, dwfl_errmsg(curr->dwflerr));
            }
            for(int i = curr->nFrames - 1; i >= 0; i--) {
                Dwarf_Addr pc = curr->framePCs[i];
                Location loc;
                libdwLookupLocation(session, &loc, pc);
                fprintf(stderr, "  %24p    %s ",
                        (void*) pc, loc.function);
                if (loc.source_file)
                    fprintf(stderr, "(%s:%d.%d)\n",
                            loc.source_file, loc.lineno, loc.colno);
                else
                    fprintf(stderr, "(%s)\n", loc.object_file);
            }
        }
    }
}

static int btp_init_err;
static pthread_key_t tls_session_key;

LibdwSession *libdwThreadSession(bool allocAsNeeded) {
    LibdwSession *sess = pthread_getspecific(tls_session_key);
    if( !sess && allocAsNeeded ) {
        sess = libdwInit();
        if( sess ) {
            int r;
            if ((r = pthread_setspecific(tls_session_key,sess)) != 0) {
                barf("libdwThreadSession: %s", strerror(r));
            }
        }
    }
    return sess;
}

static int cbGetBacktraceFramePC(Dwfl_Frame *frame, void *arg) {
    struct BtQue *btq = arg;
    if( btq->nFrames >= BTQ_MAX_DEPTH ) {
        return DWARF_CB_ABORT;
    }
    Dwarf_Addr pc;
    bool is_activation;
    if (! dwfl_frame_pc(frame, &pc, &is_activation) ) {
        // failed to find PC
        btq->framePCs[ btq->nFrames ] = 0x0;
    } else {
        // see https://sourceware.org/git/?p=elfutils.git;a=blob;f=src/stack.c;h=534aa93c433551896b67b65845ab4891fd175066;hb=HEAD#l376
        Dwarf_Addr pc_adjusted = pc - (is_activation ? 0 : 1);
        btq->framePCs[ btq->nFrames ] = pc_adjusted;
    }
    ++btq->nFrames;
    return DWARF_CB_OK;
}

#define LOG_TO_STDERR(msg) write(STDERR_FILENO, msg, strlen(msg))

static pthread_t btPrintThId;

// Note this is designed to be safe to call from signal handlers, it should be
// async-singal-safe as described in:
//   https://man7.org/linux/man-pages/man7/signal-safety.7.html
// but currently still contains allocation code, i.e. dwfl_getthread_frames()
void libdwDumpBacktrace(void) {
    if( btp_init_err ) {
        LOG_TO_STDERR( "Backtrace printing initialization has failed." );
        return;
    }

    if( pthread_kill(btPrintThId, 0) ) {
        LOG_TO_STDERR( "Backtrace print thread has died." );
        return;
    }

    // We expect it already populated at thread initialization, from where
    // allocation is safe, see workerStart() in Task.c and
    // initBacktracePrinting() later in this file
    LibdwSession *session = libdwThreadSession(false);
    if( !session ) {
        LOG_TO_STDERR(
            "Thread local libdw session not available for backtrace.\n");
        return;
    }

    // Note:
    //   It's not safe from a singal handler as in here, to allocate the
    //   struct. Otherwise we'll have to alloc and pass the ptr to
    //   pthread_setspecific() at thread initialization, then
    //   pthread_getspecific() here. For simplification, we use static
    //   thread local struct to avoid allocation at all.
    //
    //   Also, we prefer __thread over intrinsic C/C++11 threadlocal here,
    //   in hope it imposes less quirks with pthread based threading here.
    static __thread struct BtQue tlBtQue;
    struct BtQue *btq = &tlBtQue;

    // kernelThreadId() is more meaningful than osThreadId() esp. on Linux
    btq->tid = kernelThreadId();
    btq->nFrames = 0;
    // TODO dwfl_getthread_frames() does allocation, need to find an
    //      async-signal-safe alternative, or fast repeating SIGQUIT can
    //      hang the process, which shows the unsafety
    btq->dwflerr = dwfl_getthread_frames(session->dwfl, dwfl_pid(session->dwfl),
                                         cbGetBacktraceFramePC, btq);
    // see: https://sourceware.org/git?p=elfutils.git;a=blob;f=src/stack.c;h=534aa93c433551896b67b65845ab4891fd175066;hb=HEAD#l717
    switch (btq->dwflerr) {
        case DWARF_CB_OK:
        case DWARF_CB_ABORT:
            btq->dwflerr = 0;
            break;
        case -1:
            btq->dwflerr = dwfl_errno();
            break;
        default:
            break;
    }
    RELEASE_FENCE(); // TODO this redundant given the following cas()?
    // schedule the captured bt to be printed out by the print thread
    for(;;) {
        // Note: btqPending be a lock-free singly linked list, with btqSema
        //       indicating new item(s) inserted into it
        tlBtQue.next = SEQ_CST_LOAD(&btqPending);
        if( tlBtQue.next == cas(&btqPending, tlBtQue.next, &tlBtQue) ) {
            sem_post(&btqSema);
            break;
        }
    }
}

// use a dedicated thread to print backtrace of each thread serving a
// capability, SIGQUIT from os is propagated to those threads, then
// their respective backtraces are captured then scheduled to be
// printed out to stderr by the print thread.
void initBacktracePrinting(void) {
    btp_init_err = sem_init(&btqSema, 0, 0);
    if (btp_init_err != 0) {
        barf("failed to create semaphore (%s),"
             " backtrace will not be printed on SIGQUIT signal.",
             strerror(btp_init_err));
        return;
    }

    if ((btp_init_err = pthread_key_create(&tls_session_key, libdwFree)) != 0) {
        barf("failed to create thread-local key: (%s),"
             " backtrace will not be printed on SIGQUIT signal.",
             strerror(btp_init_err));
        return;
    }

    // populate thread local libdw session for the main thread,
    // so signal handlers on this thread can have it without allocation
    libdwThreadSession(true);

    btp_init_err = createOSThread(&btPrintThId, "backtrace_printer",
        (OSThreadProc*)btPrintThread, NULL);
    if (btp_init_err != 0) {
        barf("failed to create OS thread (%s),"
             " backtrace will not be printed on SIGQUIT signal.",
             strerror(btp_init_err));
        return;
    }
    // TODO use a portable way to make it low priority?
}

#else /* !USE_LIBDW */

void backtraceFree(Backtrace *bt STG_UNUSED) { }

Backtrace *libdwGetBacktrace(LibdwSession *session STG_UNUSED) {
    return NULL;
}

int libdwLookupLocation(LibdwSession *session STG_UNUSED,
                        Location *loc STG_UNUSED,
                        StgPtr pc STG_UNUSED) {
    return 1;
}


void initBacktracePrinting(void) {}

LibdwSession *libdwThreadSession(bool allocAsNeeded STG_UNUSED) {
    return NULL;
}

void libdwDumpBacktrace(void) {}

#endif /* USE_LIBDW */
