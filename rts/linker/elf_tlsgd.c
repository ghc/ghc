#include "Rts.h"

#if defined(x86_64_HOST_ARCH) && defined(freebsd_HOST_OS)

/*
 * Note [TLSGD relocation]
 * ~~~~~~~~~~~~~~~~~~~~~~~
 * Quick background: FreeBSD's <ctype.h> is poisoned with static inline code
 * that gets compiled into every program that uses functions like isdigit(3).
 * When compiled "-c -fpic" for inclusion in position-independent ".a" files
 * that are used in GHCi and HLS to load dependent packages at runtime, code
 * that uses <ctype.h> in some FFI ends up with previously unsupported
 * thread-specific variable (TLSGD) relocations.  This module narrowly addresses
 * the issue for FreeBSD, where one often ends up using thread-local storage
 * without meaning to.
 *
 * In the "General Dynamic" Thread-Local-Storage (TLSGD) model, relocations need
 * an offset into a block of thread-local data associated with a particular
 * module in which the given thread-local variable is defined.  Such blocks are
 * not used directly, since after all, the variables are thread-specific.
 * Rather, each module's initialized thread locals and uninitialised (zeroed)
 * thread-locals are used to initialise a corresponding block of data in each
 * thread, possibly on first use by a thread of a variable from a given module.
 *
 * A thread that needs the address of a particular TLS variable needs to pass
 * the module id and offset to __tls_get_addr() (provided by the ELF runtime
 * linker ld.so, a.k.a. the RTLD, which also manages the loading and unloading
 * of modules, and dynamic creation of the backing storage for each thread's
 * dynamic thread-local-storage vector (dtv).
 *
 * The data to pass to __tls_get_addr() is found as two consecutive 64-bit
 * values in the global offset table (GOT) of the object being relocated.
 * (There are therefore many GOT tables, what's global is the addresses they
 * point to, which are often outside the current object, not the tables
 * themselves).
 *
 * The module id and offset are not known at compile time, and require
 * relocation with assistance from the RTLD, because only the RTLD knows the
 * logical module number for each loaded object (the main executable, and any
 * shared libraries, such as libc).  Fortunately, modern RTLDs provide an
 * iterator for the currently loaded modules of a program, which exposes
 * the associated module id and ELF section headers of each loaded object.
 * (For static executables, this is instead handled by the C library).
 *
 * The iterator in question is dl_iterate_phdr(3).  It repeatedly invokes
 * the provided callback for each loaded module until the callback returns
 * a non-zero value indicating that it has found what it was looking for
 * and does not need to be called with any further modules.
 *
 * The "dlpi_info" structure provided to the callback contains the module
 * id and a reference to the ELF program header list.  In the program header
 * list the "dynamic" section contains a number of subsections, which include
 * the symbol table, the string table and either or both the sysv or GNU-style
 * symbol hash table.
 *
 * The size of the symbol table is not directly available, so linear search
 * through the symbol table is not only inefficient, but in fact not really
 * possible, since we don't reliably know where the table ends.  However, the
 * hash tables (sysv and/or GNU) do have clear bounds, and substantially speed
 * up symbol lookup, so we need to have code to use these tables.  For now,
 * only the sysv table is supported, but it should be easy to also support the
 * GNU table (which could be the only present).  On FreeBSD it is rumoured (or
 * least anecdotally observed) that the tool chains ensure that the sysv table
 * is always present.
 *
 * Thus armed with the symbol, string and hash table for a module, we can use
 * our wanted symbol's hash to quickly find the relevant hash bucket, and from
 * there traverse the list of symbols that share that hash, checking that
 * whether the name is in fact an exact match.
 *
 * Note that the name we want may also appear as an undefined entry in the
 * symbol tables of other modules that also reference it as an external symbol.
 * Thus the module we're looking for is the one where the symbol's st_value is
 * non-zero (indicating that it is actually defined in that module).
 *
 * Since we're looking for a TLS variable, we just in case also check the type
 * and avoid erroneous bindings to some other sort of symbol.
 *
 * Once the right module is found, we need to push two values into a new slot
 * in the GOT.  This is done via the makeSymbolExtra() function of the GHC RTS.
 * Our GOT entries must therefore be wide enough to hold two 64-bit values, but
 * previously their X86_64 incarnation was only 14 bytes wide.  It has now been
 * expanded to 16 bytes, by adding two padding bytes to the jumpIsland slot
 * that follows the `addr` field field of the original GOT entry.  We store the
 * module id in the `addr` field and the symbol's offset in the expanded
 * jumpIsland field.  The address `S` of the start of the new GOT entry is
 * then adjusted to form the relative address `S + A - P` which is stored at the
 * relocation address `P`.
 *
 * The magic additional offsets `0x8000` and `0x800` for MIPS, ... and RISC-V,
 * were suggested by Fangrui Song (a.k.a. @MaskRay) in a comment on the ticket
 * discussing the motivating FreeBSD issue:
 * <https://gitlab.haskell.org/ghc/ghc/-/issues/19086#note_347076>.
 * His blog at <https://maskray.me/blog/2021-02-14-all-about-thread-local-storage>
 * may shed more light on these.
 *
 * Finally, the bad news.  This code only works when the target TLS variable is
 * defined by a preloaded shared object (.SO) that is known to the RTLD, has a
 * module id, and TLS data and bss segments from which the RTLD initialises
 * (perhaps lazily just-in-time) the per-thread TLS segments.  It is not
 * presently possible to support TLS variables from runtime loaded ".o" files,
 * These are not loaded via the RTLD, and don't get a new module id, and
 * __tls_get_addr() cannot return an appropriate thread-specific address for
 * these.
 *
 * The best solution is probably to deprecate runtime loading of ".o" files,
 * all runtime loaded objects should be shared objects, loaded via dlopen(),
 * in which case the RTLD will take of all the TLS relocation details!
 * Otherwise, packages with FFI code that uses the _Thread_local storage class
 * will not be runtime loadable in GHCi, Haskell-language-server, and similar
 * programs that use the GHC RTS runtime linker.  As the popularity of such
 * variables increases, we'll need have a more comprehensive approach to dealing
 * with them, not limited to just "external references" as supported here.
 *
 * A much more complex approach would be to filter calls to __tls_get_addr(),
 * using GHC-specific code to allocate per-thread storage for TLS variables in
 * code loaded via ".o" files, delegating just external TLS variables to the
 * RTLD.  It is far from clear how to do that, and likely unwise to even think
 * about going there.
 */

#include "linker/Elf.h"
#include "linker/SymbolExtras.h"
#include <link.h>
#include <string.h>

/*
 * Though for now we only get here for X86_64, also handle some other CPUs.
 */
#if defined(__mips__) || defined(__powerpc__) || defined(__powerpc64__)
#define OFFSUB 0x8000
#elif defined(__riscv__)
#define OFFSUB 0x800
#else
#define OFFSUB 0x0
#endif

static unsigned long
elfhash(const unsigned char *name)
{
    unsigned long h = 0, g;

    while (*name)
    {
        h = (h << 4) + *name++;
        if ((g = h & 0xf0000000) != 0)
            h ^= g >> 24;
        h &= ~g;
    }
    return h;
}

typedef struct tls_sym {
    ObjectCode   *tls_sym_object;
    const char   *tls_sym_name;
    unsigned long tls_sym_indx;
    unsigned long tls_sym_hash;
    StgInt64      tls_sym_reloc;
} tls_sym;

typedef struct dl_phdr_info dlpi;

static int
find_tls_sym(dlpi *info, size_t sz __attribute__((unused)), void *data)
{
    tls_sym *wanted = (tls_sym *)data;
    const Elf_Addr base = info->dlpi_addr;
    const Elf_Dyn *dyn = NULL;
    const Elf_Sym *dynsym = NULL;
    const Elf_Word *dynhash = 0;
    const char *dynstr = NULL;

    for (size_t i = 0; i < info->dlpi_phnum; i++) {
        const Elf_Phdr *phdr = &info->dlpi_phdr[i];

        if (phdr->p_type == PT_DYNAMIC) {
            dyn = (const Elf_Dyn *)(base + phdr->p_vaddr);
            break;
        }
    }
    if (dyn == NULL)
        return 0;

    for (size_t i = 0; dyn[i].d_tag != DT_NULL; ++i)
        switch (dyn[i].d_tag) {
        case DT_SYMTAB:
            dynsym = (const Elf_Sym *)(base + dyn[i].d_un.d_val);
            break;
        case DT_STRTAB:
            dynstr = (const char *)(base + dyn[i].d_un.d_val);
            break;
        case DT_HASH:
            dynhash = (const Elf_Word *)(base + dyn[i].d_un.d_val);
            break;
        default:
            break;
        }

    if (dynsym == NULL || dynstr == NULL || dynhash == NULL)
        return 0;

    unsigned long nbucket = (unsigned long)dynhash[0];
    // unsigned long nchain = (unsigned long)dynhash[1];
    const Elf_Word *bucket = &dynhash[2];
    const Elf_Word *chain = &dynhash[2+nbucket];
    unsigned long h = wanted->tls_sym_hash % nbucket;

    for (unsigned long i = bucket[h]; i != STN_UNDEF; i = chain[i]) {
        const Elf_Sym *sym = dynsym+i;
        const char *symname = dynstr + sym->st_name;

        /* Ignore undefined or non-TLS symbols */
        if (sym->st_value == 0 || ELF_ST_TYPE(sym->st_info) != STT_TLS)
            continue;

        if (strcmp(symname, wanted->tls_sym_name) == 0) {
            unsigned long target = sym->st_value - OFFSUB;
            /* Store the module id as GOT[0] in a new GOT entry */
            SymbolExtra *extra =
                makeSymbolExtra(wanted->tls_sym_object,
                                wanted->tls_sym_indx,
                                info->dlpi_tls_modid);
            /* Copy the target address to GOT[1] (a.k.a. jumpIsland) */
            memcpy(extra->jumpIsland, &target, sizeof(target));
            wanted->tls_sym_reloc = (StgInt64) extra;
            /* Signal success, no more modules will be tried */
            return 1;
        }
    }
    /* Try the next module if any */
    return 0;
}

StgInt64
lookupTlsgdSymbol(const char *symbol, unsigned long symnum, ObjectCode *oc)
{
    tls_sym t;

    t.tls_sym_object = oc;
    t.tls_sym_name = symbol;
    t.tls_sym_indx = symnum;
    t.tls_sym_hash = elfhash((unsigned char *)symbol);
    t.tls_sym_reloc = 0;

    dl_iterate_phdr(find_tls_sym, &t);

    return t.tls_sym_reloc;
}
#endif
