#include "Rts.h"

#if defined(x86_64_HOST_ARCH) && defined(freebsd_HOST_OS)

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
