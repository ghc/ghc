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
    dwfl_module_info(mod, NULL, NULL, NULL, NULL, NULL,
                     &frame->object_file, NULL);

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
        if (is_activation)
            pc -= 1; // TODO: is this right?
        backtracePush(session->cur_bt, (StgPtr) (uintptr_t) pc);
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
#else
#    error "Please implement set_initial_registers() for your arch"
#endif

static const Dwfl_Thread_Callbacks thread_cbs = {
    .next_thread = next_thread,
    .memory_read = memory_read,
    .set_initial_registers = set_initial_registers,
};

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

#endif /* USE_LIBDW */
