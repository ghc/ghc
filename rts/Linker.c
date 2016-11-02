/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2012
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#if 0
#include "PosixSource.h"
#endif

#include "Rts.h"
#include "HsFFI.h"

#include "sm/Storage.h"
#include "Stats.h"
#include "Hash.h"
#include "LinkerInternals.h"
#include "RtsUtils.h"
#include "Trace.h"
#include "StgPrimFloat.h" // for __int_encodeFloat etc.
#include "Proftimer.h"
#include "GetEnv.h"
#include "Stable.h"
#include "RtsSymbols.h"
#include "RtsSymbolInfo.h"
#include "Profiling.h"
#include "sm/OSMem.h"
#include "linker/M32Alloc.h"
#include "linker/CacheFlush.h"
#include "linker/SymbolExtras.h"
#include "PathUtils.h"

#if !defined(mingw32_HOST_OS)
#include "posix/Signals.h"
#endif

// get protos for is*()
#include <ctype.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if defined(HAVE_DLFCN_H)
#include <dlfcn.h>
#endif

#if defined(linux_HOST_OS) || defined(solaris2_HOST_OS) || defined(freebsd_HOST_OS) || defined(kfreebsdgnu_HOST_OS) || defined(dragonfly_HOST_OS) || defined(netbsd_HOST_OS) || defined(openbsd_HOST_OS) || defined(gnu_HOST_OS)
#  define OBJFORMAT_ELF
#  include <regex.h>    // regex is already used by dlopen() so this is OK
                        // to use here without requiring an additional lib
#elif defined (mingw32_HOST_OS)
#  define OBJFORMAT_PEi386
#  include "linker/PEi386.h"
#  include <windows.h>
#elif defined(darwin_HOST_OS)
#  define OBJFORMAT_MACHO
#  include <regex.h>
#  include <mach/machine.h>
#  include <mach-o/fat.h>
#  include <mach-o/loader.h>
#  include <mach-o/nlist.h>
#  include <mach-o/reloc.h>
#if defined(powerpc_HOST_ARCH)
#  include <mach-o/ppc/reloc.h>
#endif
#if defined(x86_64_HOST_ARCH)
#  include <mach-o/x86_64/reloc.h>
#endif
#endif

#if defined(x86_64_HOST_ARCH) && defined(darwin_HOST_OS)
#define ALWAYS_PIC
#endif

#if defined(dragonfly_HOST_OS)
#include <sys/tls.h>
#endif

/* `symhash` is a Hash table mapping symbol names to RtsSymbolInfo.
   This hashtable will contain information on all symbols
   that we know of, however the .o they are in may not be loaded.

   Until the ObjectCode the symbol belongs to is actually
   loaded this symbol may be replaced. So do not rely on
   addresses of unloaded symbols.

   Note [runtime-linker-phases]
   --------------------------------------
   Broadly the behavior of the runtime linker can be
   split into the following four phases:

   - Indexing (e.g. ocVerifyImage and ocGetNames)
   - Initialization (e.g. ocResolve and ocRunInit)
   - Resolve (e.g. resolveObjs())
   - Lookup (e.g. lookupSymbol)

   This is to enable lazy loading of symbols. Eager loading is problematic
   as it means that all symbols must be available, even those which we will
   never use. This is especially painful of Windows, where the number of
   libraries required to link things like mingwex grows to be quite high.

   We proceed through these stages as follows,

   * During Indexing we verify and open the ObjectCode and
     perform a quick scan/indexing of the ObjectCode. All the work
     required to actually load the ObjectCode is done.

     All symbols from the ObjectCode is also inserted into
     `symhash`, where possible duplicates are handled via the semantics
     described in `ghciInsertSymbolTable`.

     This phase will produce ObjectCode with status `OBJECT_LOADED` or `OBJECT_NEEDED`
     depending on whether they are an archive members or not.

   * During initialization we load ObjectCode, perform relocations, execute
     static constructors etc. This phase may trigger other ObjectCodes to
     be loaded because of the calls to lookupSymbol.

     This phase will produce ObjectCode with status `OBJECT_NEEDED` if the
     previous status was `OBJECT_LOADED`.

   * During resolve we attempt to resolve all the symbols needed for the
     initial link. This essentially means, that for any ObjectCode given
     directly to the command-line we perform lookupSymbols on the required
     symbols. lookupSymbols may trigger the loading of additional ObjectCode
     if required.

     This phase will produce ObjectCode with status `OBJECT_RESOLVED` if
     the previous status was `OBJECT_NEEDED`.

   * Lookup symbols is used to lookup any symbols required, both during initial
     link and during statement and expression compilations in the REPL.
     Declaration of e.g. an foreign import, will eventually call lookupSymbol
     which will either fail (symbol unknown) or succeed (and possibly triggered a
     load).

     This phase may transition an ObjectCode from `OBJECT_LOADED` to `OBJECT_RESOLVED`

   When a new scope is introduced (e.g. a new module imported) GHCi does a full re-link
   by calling unloadObj and starting over.
   When a new declaration or statement is performed ultimately lookupSymbol is called
   without doing a re-link.

   The goal of these different phases is to allow the linker to be able to perform
   "lazy loading" of ObjectCode. The reason for this is that we want to only link
   in symbols that are actually required for the link. This reduces:

   1) Dependency chains, if A.o required a .o in libB but A.o isn't required to link
      then we don't need to load libB. This means the dependency chain for libraries
      such as mingw32 and mingwex can be broken down.

   2) The number of duplicate symbols, since now only symbols that are
      true duplicates will display the error.
 */
static /*Str*/HashTable *symhash;

/* List of currently loaded objects */
ObjectCode *objects = NULL;     /* initially empty */

/* List of objects that have been unloaded via unloadObj(), but are waiting
   to be actually freed via checkUnload() */
ObjectCode *unloaded_objects = NULL; /* initially empty */

#ifdef THREADED_RTS
/* This protects all the Linker's global state except unloaded_objects */
Mutex linker_mutex;
/*
 * This protects unloaded_objects.  We have a separate mutex for this, because
 * the GC needs to access unloaded_objects in checkUnload, while the linker only
 * needs to access unloaded_objects in unloadObj(), so this allows most linker
 * operations proceed concurrently with the GC.
 */
Mutex linker_unloaded_mutex;
#endif

static HsInt isAlreadyLoaded( pathchar *path );
static HsInt loadOc( ObjectCode* oc );
static ObjectCode* mkOc( pathchar *path, char *image, int imageSize,
                         rtsBool mapped, char *archiveMemberName,
                         int misalignment
                       );

/* Generic wrapper function to try and Resolve and RunInit oc files */
int ocTryLoad( ObjectCode* oc );

#if defined(OBJFORMAT_ELF)
static int ocVerifyImage_ELF    ( ObjectCode* oc );
static int ocGetNames_ELF       ( ObjectCode* oc );
static int ocResolve_ELF        ( ObjectCode* oc );
static int ocRunInit_ELF        ( ObjectCode* oc );
#if NEED_SYMBOL_EXTRAS
static int ocAllocateSymbolExtras_ELF ( ObjectCode* oc );
#endif

#elif defined(OBJFORMAT_MACHO)
static int ocVerifyImage_MachO    ( ObjectCode* oc );
static int ocGetNames_MachO       ( ObjectCode* oc );
static int ocResolve_MachO        ( ObjectCode* oc );
static int ocRunInit_MachO        ( ObjectCode* oc );
static int machoGetMisalignment( FILE * );
#if NEED_SYMBOL_EXTRAS
static int ocAllocateSymbolExtras_MachO ( ObjectCode* oc );
#endif
#ifdef powerpc_HOST_ARCH
static void machoInitSymbolsWithoutUnderscore( void );
#endif
#endif

/* on x86_64 we have a problem with relocating symbol references in
 * code that was compiled without -fPIC.  By default, the small memory
 * model is used, which assumes that symbol references can fit in a
 * 32-bit slot.  The system dynamic linker makes this work for
 * references to shared libraries by either (a) allocating a jump
 * table slot for code references, or (b) moving the symbol at load
 * time (and copying its contents, if necessary) for data references.
 *
 * We unfortunately can't tell whether symbol references are to code
 * or data.  So for now we assume they are code (the vast majority
 * are), and allocate jump-table slots.  Unfortunately this will
 * SILENTLY generate crashing code for data references.  This hack is
 * enabled by X86_64_ELF_NONPIC_HACK.
 *
 * One workaround is to use shared Haskell libraries.  This is
 * coming.  Another workaround is to keep the static libraries but
 * compile them with -fPIC, because that will generate PIC references
 * to data which can be relocated.  The PIC code is still too green to
 * do this systematically, though.
 *
 * See bug #781
 * See thread http://www.haskell.org/pipermail/cvs-ghc/2007-September/038458.html
 *
 * Naming Scheme for Symbol Macros
 *
 * SymI_*: symbol is internal to the RTS. It resides in an object
 *         file/library that is statically.
 * SymE_*: symbol is external to the RTS library. It might be linked
 *         dynamically.
 *
 * Sym*_HasProto  : the symbol prototype is imported in an include file
 *                  or defined explicitly
 * Sym*_NeedsProto: the symbol is undefined and we add a dummy
 *                  default proto extern void sym(void);
 */
#define X86_64_ELF_NONPIC_HACK 1

/* Link objects into the lower 2Gb on x86_64.  GHC assumes the
 * small memory model on this architecture (see gcc docs,
 * -mcmodel=small).
 *
 * MAP_32BIT not available on OpenBSD/amd64
 */
#if defined(x86_64_HOST_ARCH) && defined(MAP_32BIT)
#define TRY_MAP_32BIT MAP_32BIT
#else
#define TRY_MAP_32BIT 0
#endif

/*
  Note [The ARM/Thumb Story]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~

  Support for the ARM architecture is complicated by the fact that ARM has not
  one but several instruction encodings. The two relevant ones here are the original
  ARM encoding and Thumb, a more dense variant of ARM supporting only a subset
  of the instruction set.

  How the CPU decodes a particular instruction is determined by a mode bit. This
  mode bit is set on jump instructions, the value being determined by the low
  bit of the target address: An odd address means the target is a procedure
  encoded in the Thumb encoding whereas an even address means it's a traditional
  ARM procedure (the actual address jumped to is even regardless of the encoding bit).

  Interoperation between Thumb- and ARM-encoded object code (known as "interworking")
  is tricky. If the linker needs to link a call by an ARM object into Thumb code
  (or vice-versa) it will produce a jump island. This, however, is incompatible with
  GHC's tables-next-to-code. For this reason, it is critical that GHC emit
  exclusively ARM or Thumb objects for all Haskell code.

  We still do, however, need to worry about foreign code.
*/

/*
 * Due to the small memory model (see above), on x86_64 we have to map
 * all our non-PIC object files into the low 2Gb of the address space
 * (why 2Gb and not 4Gb?  Because all addresses must be reachable
 * using a 32-bit signed PC-relative offset). On Linux we can do this
 * using the MAP_32BIT flag to mmap(), however on other OSs
 * (e.g. *BSD, see #2063, and also on Linux inside Xen, see #2512), we
 * can't do this.  So on these systems, we have to pick a base address
 * in the low 2Gb of the address space and try to allocate memory from
 * there.
 *
 * We pick a default address based on the OS, but also make this
 * configurable via an RTS flag (+RTS -xm)
 */
#if !defined(ALWAYS_PIC) && defined(x86_64_HOST_ARCH)

#if defined(MAP_32BIT)
// Try to use MAP_32BIT
#define MMAP_32BIT_BASE_DEFAULT 0
#else
// A guess: 1Gb.
#define MMAP_32BIT_BASE_DEFAULT 0x40000000
#endif

static void *mmap_32bit_base = (void *)MMAP_32BIT_BASE_DEFAULT;
#endif

static void ghciRemoveSymbolTable(HashTable *table, const SymbolName* key,
    ObjectCode *owner)
{
    RtsSymbolInfo *pinfo = lookupStrHashTable(table, key);
    if (!pinfo || owner != pinfo->owner) return;
    removeStrHashTable(table, key, NULL);
    stgFree(pinfo);
}

/* -----------------------------------------------------------------------------
 * Insert symbols into hash tables, checking for duplicates.
 *
 * Returns: 0 on failure, nonzero on success
 */
/*
 Note [weak-symbols-support]
 -------------------------------------
 While ghciInsertSymbolTable does implement extensive
 logic for weak symbol support, weak symbols are not currently
 fully supported by the RTS. This code is mostly here for COMDAT
 support which uses the weak symbols support.

 Linking weak symbols defined purely in C code with other C code
 should also work, probably. Observing weak symbols in Haskell
 won't.

 Some test have been written for weak symbols but have been disabled
 mostly because it's unsure how the weak symbols support should look.
 See Trac #11223
 */
static int ghciInsertSymbolTable(
   pathchar* obj_name,
   HashTable *table,
   const SymbolName* key,
   SymbolAddr* data,
   HsBool weak,
   ObjectCode *owner)
{
   RtsSymbolInfo *pinfo = lookupStrHashTable(table, key);
   if (!pinfo) /* new entry */
   {
      pinfo = stgMallocBytes(sizeof (*pinfo), "ghciInsertToSymbolTable");
      pinfo->value = data;
      pinfo->owner = owner;
      pinfo->weak = weak;
      insertStrHashTable(table, key, pinfo);
      return 1;
   }
   else if (weak && data && pinfo->weak && !pinfo->value)
   {
       /* The existing symbol is weak with a zero value; replace it with the new symbol. */
       pinfo->value = data;
       pinfo->owner = owner;
       return 1;
   }
   else if (weak)
   {
       return 1; /* weak symbol, because the symbol is weak, data = 0 and we
                 already know of another copy throw this one away.

                 or both weak symbols have a nonzero value. Keep the existing one.

                 This also preserves the semantics of linking against
                 the first symbol we find. */
   }
   else if (pinfo->weak && !weak) /* weak symbol is in the table */
   {
      /* override the weak definition with the non-weak one */
      pinfo->value = data;
      pinfo->owner = owner;
      pinfo->weak = HS_BOOL_FALSE;
      return 1;
   }
   else if (  pinfo->owner
           && pinfo->owner->status != OBJECT_RESOLVED
           && pinfo->owner->status != OBJECT_NEEDED)
   {
        /* If the other symbol hasn't been loaded or will be loaded and we want to
           explicitly load the new one, we can just swap it out and load the one
           that has been requested. If not, just keep the first one encountered.

           Because the `symHash' table consists symbols we've also not loaded but
           found during the initial scan this is safe to do. If however the existing
           symbol has been loaded then it means we have a duplicate.

           This is essentially emulating the behavior of a linker wherein it will always
           link in object files that are .o file arguments, but only take object files
           from archives as needed. */
       if (owner && (owner->status == OBJECT_NEEDED || owner->status == OBJECT_RESOLVED)) {
           pinfo->value = data;
           pinfo->owner = owner;
           pinfo->weak  = weak;
       }

       return 1;
    }
    else if (pinfo->owner == owner)
    {
       /* If it's the same symbol, ignore. This makes ghciInsertSymbolTable idempotent */
       return 1;
    }
    else if (owner && owner->status == OBJECT_LOADED)
    {
        /* If the duplicate symbol is just in state OBJECT_LOADED it means we're in discovery of an
           member. It's not a real duplicate yet. If the Oc Becomes OBJECT_NEEDED then ocTryLoad will
           call this function again to trigger the duplicate error. */
        return 1;
    }

   pathchar* archiveName = NULL;
   debugBelch(
      "GHC runtime linker: fatal error: I found a duplicate definition for symbol\n"
      "   %s\n"
      "whilst processing object file\n"
      "   %" PATH_FMT "\n"
      "The symbol was previously defined in\n"
      "   %" PATH_FMT "\n"
      "This could be caused by:\n"
      "   * Loading two different object files which export the same symbol\n"
      "   * Specifying the same object file twice on the GHCi command line\n"
      "   * An incorrect `package.conf' entry, causing some object to be\n"
      "     loaded twice.\n",
      (char*)key,
      obj_name,
      pinfo->owner == NULL ? WSTR("(GHCi built-in symbols)") :
      pinfo->owner->archiveMemberName ? archiveName = mkPath(pinfo->owner->archiveMemberName)
      : pinfo->owner->fileName
   );

   if (archiveName)
   {
       stgFree(archiveName);
       archiveName = NULL;
   }
   return 0;
}

/* -----------------------------------------------------------------------------
* Looks up symbols into hash tables.
*
* Returns: 0 on failure and result is not set,
*          nonzero on success and result set to nonzero pointer
*/
HsBool ghciLookupSymbolInfo(HashTable *table,
    const SymbolName* key, RtsSymbolInfo **result)
{
    RtsSymbolInfo *pinfo = lookupStrHashTable(table, key);
    if (!pinfo) {
        *result = NULL;
        return HS_BOOL_FALSE;
    }
    if (pinfo->weak)
        IF_DEBUG(linker, debugBelch("lookupSymbolInfo: promoting %s\n", key));
    /* Once it's looked up, it can no longer be overridden */
    pinfo->weak = HS_BOOL_FALSE;

    *result = pinfo;
    return HS_BOOL_TRUE;
}

/* -----------------------------------------------------------------------------
 * initialize the object linker
 */


static int linker_init_done = 0 ;

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
static void *dl_prog_handle;
static regex_t re_invalid;
static regex_t re_realso;
#ifdef THREADED_RTS
static Mutex dl_mutex; // mutex to protect dlopen/dlerror critical section
#endif
#endif

void initLinker (void)
{
    // default to retaining CAFs for backwards compatibility.  Most
    // users will want initLinker_(0): otherwise unloadObj() will not
    // be able to unload object files when they contain CAFs.
    initLinker_(1);
}

void
initLinker_ (int retain_cafs)
{
    RtsSymbolVal *sym;
#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
    int compileResult;
#endif

    IF_DEBUG(linker, debugBelch("initLinker: start\n"));

    /* Make initLinker idempotent, so we can call it
       before every relevant operation; that means we
       don't need to initialise the linker separately */
    if (linker_init_done == 1) {
        IF_DEBUG(linker, debugBelch("initLinker: idempotent return\n"));
        return;
    } else {
        linker_init_done = 1;
    }

    objects = NULL;
    unloaded_objects = NULL;

#if defined(THREADED_RTS)
    initMutex(&linker_mutex);
    initMutex(&linker_unloaded_mutex);
#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
    initMutex(&dl_mutex);
#endif
#endif

    symhash = allocStrHashTable();

    /* populate the symbol table with stuff from the RTS */
    for (sym = rtsSyms; sym->lbl != NULL; sym++) {
        if (! ghciInsertSymbolTable(WSTR("(GHCi built-in symbols)"),
                                    symhash, sym->lbl, sym->addr, HS_BOOL_FALSE, NULL)) {
            barf("ghciInsertSymbolTable failed");
        }
        IF_DEBUG(linker, debugBelch("initLinker: inserting rts symbol %s, %p\n", sym->lbl, sym->addr));
    }
#   if defined(OBJFORMAT_MACHO) && defined(powerpc_HOST_ARCH)
    machoInitSymbolsWithoutUnderscore();
#   endif
    /* GCC defines a special symbol __dso_handle which is resolved to NULL if
       referenced from a statically linked module. We need to mimic this, but
       we cannot use NULL because we use it to mean nonexistent symbols. So we
       use an arbitrary (hopefully unique) address here.
    */
    if (! ghciInsertSymbolTable(WSTR("(GHCi special symbols)"),
                                symhash, "__dso_handle", (void *)0x12345687, HS_BOOL_FALSE, NULL)) {
        barf("ghciInsertSymbolTable failed");
    }

    // Redirect newCAF to newRetainedCAF if retain_cafs is true.
    if (! ghciInsertSymbolTable(WSTR("(GHCi built-in symbols)"), symhash,
                                MAYBE_LEADING_UNDERSCORE_STR("newCAF"),
                                retain_cafs ? newRetainedCAF : newGCdCAF,
                                HS_BOOL_FALSE, NULL)) {
        barf("ghciInsertSymbolTable failed");
    }

#   if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
#   if defined(RTLD_DEFAULT)
    dl_prog_handle = RTLD_DEFAULT;
#   else
    dl_prog_handle = dlopen(NULL, RTLD_LAZY);
#   endif /* RTLD_DEFAULT */

    compileResult = regcomp(&re_invalid,
           "(([^ \t()])+\\.so([^ \t:()])*):([ \t])*(invalid ELF header|file too short)",
           REG_EXTENDED);
    if (compileResult != 0) {
        barf("Compiling re_invalid failed");
    }
    compileResult = regcomp(&re_realso,
           "(GROUP|INPUT) *\\( *([^ )]+)",
           REG_EXTENDED);
    if (compileResult != 0) {
        barf("Compiling re_realso failed");
    }
#   endif

#if !defined(ALWAYS_PIC) && defined(x86_64_HOST_ARCH)
    if (RtsFlags.MiscFlags.linkerMemBase != 0) {
        // User-override for mmap_32bit_base
        mmap_32bit_base = (void*)RtsFlags.MiscFlags.linkerMemBase;
    }
#endif

    if (RTS_LINKER_USE_MMAP)
        m32_allocator_init();

#if defined(OBJFORMAT_PEi386)
    initLinker_PEi386();
#endif

    IF_DEBUG(linker, debugBelch("initLinker: done\n"));
    return;
}

void
exitLinker( void ) {
#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
   if (linker_init_done == 1) {
      regfree(&re_invalid);
      regfree(&re_realso);
#ifdef THREADED_RTS
      closeMutex(&dl_mutex);
#endif
   }
#endif
   if (linker_init_done == 1) {
       freeHashTable(symhash, free);
   }
#ifdef THREADED_RTS
   closeMutex(&linker_mutex);
#endif
}

/* -----------------------------------------------------------------------------
 *                  Loading DLL or .so dynamic libraries
 * -----------------------------------------------------------------------------
 *
 * Add a DLL from which symbols may be found.  In the ELF case, just
 * do RTLD_GLOBAL-style add, so no further messing around needs to
 * happen in order that symbols in the loaded .so are findable --
 * lookupSymbol() will subsequently see them by dlsym on the program's
 * dl-handle.  Returns NULL if success, otherwise ptr to an err msg.
 *
 * In the PEi386 case, open the DLLs and put handles to them in a
 * linked list.  When looking for a symbol, try all handles in the
 * list.  This means that we need to load even DLLs that are guaranteed
 * to be in the ghc.exe image already, just so we can get a handle
 * to give to loadSymbol, so that we can find the symbols.  For such
 * libraries, the LoadLibrary call should be a no-op except for returning
 * the handle.
 *
 */

#  if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)

/* Suppose in ghci we load a temporary SO for a module containing
       f = 1
   and then modify the module, recompile, and load another temporary
   SO with
       f = 2
   Then as we don't unload the first SO, dlsym will find the
       f = 1
   symbol whereas we want the
       f = 2
   symbol. We therefore need to keep our own SO handle list, and
   try SOs in the right order. */

typedef
   struct _OpenedSO {
      struct _OpenedSO* next;
      void *handle;
   }
   OpenedSO;

/* A list thereof. */
static OpenedSO* openedSOs = NULL;

static const char *
internal_dlopen(const char *dll_name)
{
   OpenedSO* o_so;
   void *hdl;
   const char *errmsg;
   char *errmsg_copy;

   // omitted: RTLD_NOW
   // see http://www.haskell.org/pipermail/cvs-ghc/2007-September/038570.html
   IF_DEBUG(linker,
      debugBelch("internal_dlopen: dll_name = '%s'\n", dll_name));

   //-------------- Begin critical section ------------------
   // This critical section is necessary because dlerror() is not
   // required to be reentrant (see POSIX -- IEEE Std 1003.1-2008)
   // Also, the error message returned must be copied to preserve it
   // (see POSIX also)

   ACQUIRE_LOCK(&dl_mutex);
   hdl = dlopen(dll_name, RTLD_LAZY|RTLD_LOCAL); /* see Note [RTLD_LOCAL] */

   errmsg = NULL;
   if (hdl == NULL) {
      /* dlopen failed; return a ptr to the error msg. */
      errmsg = dlerror();
      if (errmsg == NULL) errmsg = "addDLL: unknown error";
      errmsg_copy = stgMallocBytes(strlen(errmsg)+1, "addDLL");
      strcpy(errmsg_copy, errmsg);
      errmsg = errmsg_copy;
   } else {
      o_so = stgMallocBytes(sizeof(OpenedSO), "addDLL");
      o_so->handle = hdl;
      o_so->next   = openedSOs;
      openedSOs    = o_so;
   }

   RELEASE_LOCK(&dl_mutex);
   //--------------- End critical section -------------------

   return errmsg;
}

/*
  Note [RTLD_LOCAL]

  In GHCi we want to be able to override previous .so's with newly
  loaded .so's when we recompile something.  This further implies that
  when we look up a symbol in internal_dlsym() we have to iterate
  through the loaded libraries (in order from most recently loaded to
  oldest) looking up the symbol in each one until we find it.

  However, this can cause problems for some symbols that are copied
  by the linker into the executable image at runtime - see #8935 for a
  lengthy discussion.  To solve that problem we need to look up
  symbols in the main executable *first*, before attempting to look
  them up in the loaded .so's.  But in order to make that work, we
  have to always call dlopen with RTLD_LOCAL, so that the loaded
  libraries don't populate the global symbol table.
*/

static void *
internal_dlsym(const char *symbol) {
    OpenedSO* o_so;
    void *v;

    // We acquire dl_mutex as concurrent dl* calls may alter dlerror
    ACQUIRE_LOCK(&dl_mutex);
    dlerror();
    // look in program first
    v = dlsym(dl_prog_handle, symbol);
    if (dlerror() == NULL) {
        RELEASE_LOCK(&dl_mutex);
        return v;
    }

    for (o_so = openedSOs; o_so != NULL; o_so = o_so->next) {
        v = dlsym(o_so->handle, symbol);
        if (dlerror() == NULL) {
            RELEASE_LOCK(&dl_mutex);
            return v;
        }
    }
    RELEASE_LOCK(&dl_mutex);
    return v;
}
#  endif

const char *
addDLL( pathchar *dll_name )
{
#  if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
   /* ------------------- ELF DLL loader ------------------- */

#define NMATCH 5
   regmatch_t match[NMATCH];
   const char *errmsg;
   FILE* fp;
   size_t match_length;
#define MAXLINE 1000
   char line[MAXLINE];
   int result;

   IF_DEBUG(linker, debugBelch("addDLL: dll_name = '%s'\n", dll_name));
   errmsg = internal_dlopen(dll_name);

   if (errmsg == NULL) {
      return NULL;
   }

   // GHC Trac ticket #2615
   // On some systems (e.g., Gentoo Linux) dynamic files (e.g. libc.so)
   // contain linker scripts rather than ELF-format object code. This
   // code handles the situation by recognizing the real object code
   // file name given in the linker script.
   //
   // If an "invalid ELF header" error occurs, it is assumed that the
   // .so file contains a linker script instead of ELF object code.
   // In this case, the code looks for the GROUP ( ... ) linker
   // directive. If one is found, the first file name inside the
   // parentheses is treated as the name of a dynamic library and the
   // code attempts to dlopen that file. If this is also unsuccessful,
   // an error message is returned.

   // see if the error message is due to an invalid ELF header
   IF_DEBUG(linker, debugBelch("errmsg = '%s'\n", errmsg));
   result = regexec(&re_invalid, errmsg, (size_t) NMATCH, match, 0);
   IF_DEBUG(linker, debugBelch("result = %i\n", result));
   if (result == 0) {
      // success -- try to read the named file as a linker script
      match_length = (size_t) stg_min((match[1].rm_eo - match[1].rm_so),
                                 MAXLINE-1);
      strncpy(line, (errmsg+(match[1].rm_so)),match_length);
      line[match_length] = '\0'; // make sure string is null-terminated
      IF_DEBUG(linker, debugBelch ("file name = '%s'\n", line));
      if ((fp = fopen(line, "r")) == NULL) {
         return errmsg; // return original error if open fails
      }
      // try to find a GROUP or INPUT ( ... ) command
      while (fgets(line, MAXLINE, fp) != NULL) {
         IF_DEBUG(linker, debugBelch("input line = %s", line));
         if (regexec(&re_realso, line, (size_t) NMATCH, match, 0) == 0) {
            // success -- try to dlopen the first named file
            IF_DEBUG(linker, debugBelch("match%s\n",""));
            line[match[2].rm_eo] = '\0';
            stgFree((void*)errmsg); // Free old message before creating new one
            errmsg = internal_dlopen(line+match[2].rm_so);
            break;
         }
         // if control reaches here, no GROUP or INPUT ( ... ) directive
         // was found and the original error message is returned to the
         // caller
      }
      fclose(fp);
   }
   return errmsg;

#  elif defined(OBJFORMAT_PEi386)
   return addDLL_PEi386(dll_name);

#  else
   barf("addDLL: not implemented on this platform");
#  endif
}

/* -----------------------------------------------------------------------------
* Searches the system directories to determine if there is a system DLL that
* satisfies the given name. This prevent GHCi from linking against a static
* library if a DLL is available.
*
* Returns: NULL on failure or no DLL found, else the full path to the DLL
*          that can be loaded.
*/
pathchar* findSystemLibrary(pathchar* dll_name)
{
    IF_DEBUG(linker, debugBelch("\nfindSystemLibrary: dll_name = `%" PATH_FMT "'\n", dll_name));

#if defined(OBJFORMAT_PEi386)
    return findSystemLibrary_PEi386(dll_name);
#else
    (void)(dll_name); // Function not implemented for other platforms.
    return NULL;
#endif
}

/* -----------------------------------------------------------------------------
* Emits a warning determining that the system is missing a required security
* update that we need to get access to the proper APIs
*/
void warnMissingKBLibraryPaths( void )
{
    static HsBool missing_update_warn = HS_BOOL_FALSE;
    if (!missing_update_warn) {
        debugBelch("Warning: If linking fails, consider installing KB2533623.\n");
        missing_update_warn = HS_BOOL_TRUE;
    }
}

/* -----------------------------------------------------------------------------
* appends a directory to the process DLL Load path so LoadLibrary can find it
*
* Returns: NULL on failure, or pointer to be passed to removeLibrarySearchPath to
*          restore the search path to what it was before this call.
*/
HsPtr addLibrarySearchPath(pathchar* dll_path)
{
    IF_DEBUG(linker, debugBelch("\naddLibrarySearchPath: dll_path = `%" PATH_FMT "'\n", dll_path));

#if defined(OBJFORMAT_PEi386)
    return addLibrarySearchPath_PEi386(dll_path);
#else
    (void)(dll_path); // Function not implemented for other platforms.
    return NULL;
#endif
}

/* -----------------------------------------------------------------------------
* removes a directory from the process DLL Load path
*
* Returns: HS_BOOL_TRUE on success, otherwise HS_BOOL_FALSE
*/
HsBool removeLibrarySearchPath(HsPtr dll_path_index)
{
    IF_DEBUG(linker, debugBelch("\nremoveLibrarySearchPath: ptr = `%p'\n", dll_path_index));

#if defined(OBJFORMAT_PEi386)
    return removeLibrarySearchPath_PEi386(dll_path_index);
#else
    (void)(dll_path_index); // Function not implemented for other platforms.
    return HS_BOOL_FALSE;
#endif
}

/* -----------------------------------------------------------------------------
 * insert a symbol in the hash table
 *
 * Returns: 0 on failure, nozero on success
 */
HsInt insertSymbol(pathchar* obj_name, SymbolName* key, SymbolAddr* data)
{
    return ghciInsertSymbolTable(obj_name, symhash, key, data, HS_BOOL_FALSE, NULL);
}

/* -----------------------------------------------------------------------------
 * lookup a symbol in the hash table
 */
#if defined(OBJFORMAT_PEi386)
static SymbolAddr* lookupSymbol_ (SymbolName* lbl)
{
    return lookupSymbol_PEi386(lbl);
}

#else

static SymbolAddr* lookupSymbol_ (SymbolName* lbl)
{
    IF_DEBUG(linker, debugBelch("lookupSymbol: looking up %s\n", lbl));

    ASSERT(symhash != NULL);
    RtsSymbolInfo *pinfo;

    if (!ghciLookupSymbolInfo(symhash, lbl, &pinfo)) {
        IF_DEBUG(linker, debugBelch("lookupSymbol: symbol not found\n"));

#       if defined(OBJFORMAT_ELF)
        return internal_dlsym(lbl);
#       elif defined(OBJFORMAT_MACHO)

        /* HACK: On OS X, all symbols are prefixed with an underscore.
                 However, dlsym wants us to omit the leading underscore from the
                 symbol name -- the dlsym routine puts it back on before searching
                 for the symbol. For now, we simply strip it off here (and ONLY
                 here).
        */
        IF_DEBUG(linker, debugBelch("lookupSymbol: looking up %s with dlsym\n", lbl));
        ASSERT(lbl[0] == '_');
        return internal_dlsym(lbl + 1);

#       else
        ASSERT(2+2 == 5);
        return NULL;
#       endif
    } else {
        return loadSymbol(lbl, pinfo);
    }
}
#endif /* OBJFORMAT_PEi386 */

/*
 * Load and relocate the object code for a symbol as necessary.
 * Symbol name only used for diagnostics output.
 */
SymbolAddr* loadSymbol(SymbolName *lbl, RtsSymbolInfo *pinfo) {
    IF_DEBUG(linker, debugBelch("lookupSymbol: value of %s is %p\n", lbl, pinfo->value));
    ObjectCode* oc = pinfo->owner;

    /* Symbol can be found during linking, but hasn't been relocated. Do so now.
        See Note [runtime-linker-phases] */
    if (oc && oc->status == OBJECT_LOADED) {
        oc->status = OBJECT_NEEDED;
        IF_DEBUG(linker, debugBelch("lookupSymbol: on-demand loading symbol '%s'\n", lbl));
        int r = ocTryLoad(oc);
        if (!r) {
            errorBelch("Could not on-demand load symbol '%s'\n", lbl);
            return NULL;
        }

#ifdef PROFILING
        // collect any new cost centres & CCSs
        // that were defined during runInit
        initProfiling2();
#endif
    }

    return pinfo->value;
}

SymbolAddr* lookupSymbol( SymbolName* lbl )
{
    ACQUIRE_LOCK(&linker_mutex);
    SymbolAddr* r = lookupSymbol_(lbl);
    RELEASE_LOCK(&linker_mutex);
    return r;
}

/* -----------------------------------------------------------------------------
   Create a StablePtr for a foreign export.  This is normally called by
   a C function with __attribute__((constructor)), which is generated
   by GHC and linked into the module.

   If the object code is being loaded dynamically, then we remember
   which StablePtrs were allocated by the constructors and free them
   again in unloadObj().
   -------------------------------------------------------------------------- */

static ObjectCode *loading_obj = NULL;

StgStablePtr foreignExportStablePtr (StgPtr p)
{
    ForeignExportStablePtr *fe_sptr;
    StgStablePtr *sptr;

    sptr = getStablePtr(p);

    if (loading_obj != NULL) {
        fe_sptr = stgMallocBytes(sizeof(ForeignExportStablePtr),
                                 "foreignExportStablePtr");
        fe_sptr->stable_ptr = sptr;
        fe_sptr->next = loading_obj->stable_ptrs;
        loading_obj->stable_ptrs = fe_sptr;
    }

    return sptr;
}


/* -----------------------------------------------------------------------------
 * Debugging aid: look in GHCi's object symbol tables for symbols
 * within DELTA bytes of the specified address, and show their names.
 */
#ifdef DEBUG
void ghci_enquire ( SymbolAddr* addr );

void ghci_enquire(SymbolAddr* addr)
{
   int   i;
   SymbolName* sym;
   RtsSymbolInfo* a;
   const int DELTA = 64;
   ObjectCode* oc;

   for (oc = objects; oc; oc = oc->next) {
      for (i = 0; i < oc->n_symbols; i++) {
         sym = oc->symbols[i];
         if (sym == NULL) continue;
         a = NULL;
         if (a == NULL) {
             ghciLookupSymbolInfo(symhash, sym, &a);
         }
         if (a == NULL) {
             // debugBelch("ghci_enquire: can't find %s\n", sym);
         }
         else if (   a->value
                  && (char*)addr-DELTA <= (char*)a->value
                  && (char*)a->value <= (char*)addr+DELTA) {
             debugBelch("%p + %3d  ==  `%s'\n", addr, (int)((char*)a->value - (char*)addr), sym);
         }
      }
   }
}
#endif

#if RTS_LINKER_USE_MMAP
//
// Returns NULL on failure.
//
void *
mmapForLinker (size_t bytes, uint32_t flags, int fd, int offset)
{
   void *map_addr = NULL;
   void *result;
   size_t size;
   static uint32_t fixed = 0;

   IF_DEBUG(linker, debugBelch("mmapForLinker: start\n"));
   size = roundUpToPage(bytes);

#if !defined(ALWAYS_PIC) && defined(x86_64_HOST_ARCH)
mmap_again:

   if (mmap_32bit_base != 0) {
       map_addr = mmap_32bit_base;
   }
#endif

   IF_DEBUG(linker,
            debugBelch("mmapForLinker: \tprotection %#0x\n",
                       PROT_EXEC | PROT_READ | PROT_WRITE));
   IF_DEBUG(linker,
            debugBelch("mmapForLinker: \tflags      %#0x\n",
                       MAP_PRIVATE | TRY_MAP_32BIT | fixed | flags));

   result = mmap(map_addr, size,
                 PROT_EXEC|PROT_READ|PROT_WRITE,
                 MAP_PRIVATE|TRY_MAP_32BIT|fixed|flags, fd, offset);

   if (result == MAP_FAILED) {
       sysErrorBelch("mmap %" FMT_Word " bytes at %p",(W_)size,map_addr);
       errorBelch("Try specifying an address with +RTS -xm<addr> -RTS");
       return NULL;
   }

#if !defined(ALWAYS_PIC) && defined(x86_64_HOST_ARCH)
   if (mmap_32bit_base != 0) {
       if (result == map_addr) {
           mmap_32bit_base = (StgWord8*)map_addr + size;
       } else {
           if ((W_)result > 0x80000000) {
               // oops, we were given memory over 2Gb
               munmap(result,size);
#if defined(freebsd_HOST_OS)  || \
    defined(kfreebsdgnu_HOST_OS) || \
    defined(dragonfly_HOST_OS)
               // Some platforms require MAP_FIXED.  This is normally
               // a bad idea, because MAP_FIXED will overwrite
               // existing mappings.
               fixed = MAP_FIXED;
               goto mmap_again;
#else
               errorBelch("loadObj: failed to mmap() memory below 2Gb; "
                          "asked for %lu bytes at %p. "
                          "Try specifying an address with +RTS -xm<addr> -RTS",
                          size, map_addr);
               return NULL;
#endif
           } else {
               // hmm, we were given memory somewhere else, but it's
               // still under 2Gb so we can use it.  Next time, ask
               // for memory right after the place we just got some
               mmap_32bit_base = (StgWord8*)result + size;
           }
       }
   } else {
       if ((W_)result > 0x80000000) {
           // oops, we were given memory over 2Gb
           // ... try allocating memory somewhere else?;
           debugTrace(DEBUG_linker,
                      "MAP_32BIT didn't work; gave us %lu bytes at 0x%p",
                      bytes, result);
           munmap(result, size);

           // Set a base address and try again... (guess: 1Gb)
           mmap_32bit_base = (void*)0x40000000;
           goto mmap_again;
       }
   }
#endif

   IF_DEBUG(linker,
            debugBelch("mmapForLinker: mapped %" FMT_Word
                       " bytes starting at %p\n", (W_)size, result));
   IF_DEBUG(linker,
            debugBelch("mmapForLinker: done\n"));

   return result;
}
#endif

/*
 * Remove symbols from the symbol table, and free oc->symbols.
 * This operation is idempotent.
 */
static void removeOcSymbols (ObjectCode *oc)
{
    if (oc->symbols == NULL) return;

    // Remove all the mappings for the symbols within this object..
    int i;
    for (i = 0; i < oc->n_symbols; i++) {
        if (oc->symbols[i] != NULL) {
            ghciRemoveSymbolTable(symhash, oc->symbols[i], oc);
        }
    }

    stgFree(oc->symbols);
    oc->symbols = NULL;
}

/*
 * Release StablePtrs and free oc->stable_ptrs.
 * This operation is idempotent.
 */
static void freeOcStablePtrs (ObjectCode *oc)
{
    // Release any StablePtrs that were created when this
    // object module was initialized.
    ForeignExportStablePtr *fe_ptr, *next;

    for (fe_ptr = oc->stable_ptrs; fe_ptr != NULL; fe_ptr = next) {
        next = fe_ptr->next;
        freeStablePtr(fe_ptr->stable_ptr);
        stgFree(fe_ptr);
    }
    oc->stable_ptrs = NULL;
}

static void
freePreloadObjectFile (ObjectCode *oc)
{
#if defined(mingw32_HOST_OS)
    freePreloadObjectFile_PEi386(oc);
#else

    if (RTS_LINKER_USE_MMAP && oc->imageMapped) {
        munmap(oc->image, oc->fileSize);
    }
    else {
        stgFree(oc->image);
    }

#endif

    oc->image = NULL;
    oc->fileSize = 0;
}

/*
 * freeObjectCode() releases all the pieces of an ObjectCode.  It is called by
 * the GC when a previously unloaded ObjectCode has been determined to be
 * unused, and when an error occurs during loadObj().
 */
void freeObjectCode (ObjectCode *oc)
{
    freePreloadObjectFile(oc);

    if (oc->symbols != NULL) {
        stgFree(oc->symbols);
        oc->symbols = NULL;
    }

    if (oc->extraInfos != NULL) {
        freeHashTable(oc->extraInfos, NULL);
        oc->extraInfos = NULL;
    }

    if (oc->sections != NULL) {
        int i;
        for (i=0; i < oc->n_sections; i++) {
            if (oc->sections[i].start != NULL) {
                switch(oc->sections[i].alloc){
#if RTS_LINKER_USE_MMAP
                case SECTION_MMAP:
                    munmap(oc->sections[i].mapped_start,
                           oc->sections[i].mapped_size);
                    break;
                case SECTION_M32:
                    m32_free(oc->sections[i].start,
                             oc->sections[i].size);
                    break;
#endif
                case SECTION_MALLOC:
                    stgFree(oc->sections[i].start);
                    break;
                default:
                    break;
                }
            }
        }
        stgFree(oc->sections);
    }

    freeProddableBlocks(oc);

    /* Free symbol_extras.  On x86_64 Windows, symbol_extras are allocated
     * alongside the image, so we don't need to free. */
#if NEED_SYMBOL_EXTRAS && (!defined(x86_64_HOST_ARCH) || !defined(mingw32_HOST_OS))
    if (RTS_LINKER_USE_MMAP) {
        if (!USE_CONTIGUOUS_MMAP && oc->symbol_extras != NULL) {
            m32_free(oc->symbol_extras,
                    sizeof(SymbolExtra) * oc->n_symbol_extras);
        }
    }
    else {
        stgFree(oc->symbol_extras);
    }
#endif

    stgFree(oc->fileName);
    stgFree(oc->archiveMemberName);

    stgFree(oc);
}

/* -----------------------------------------------------------------------------
* Sets the initial status of a fresh ObjectCode
*/
static void setOcInitialStatus(ObjectCode* oc) {
    if (oc->archiveMemberName == NULL) {
        oc->status = OBJECT_NEEDED;
    } else {
        oc->status = OBJECT_LOADED;
    }
}

static ObjectCode*
mkOc( pathchar *path, char *image, int imageSize,
      rtsBool mapped, char *archiveMemberName, int misalignment ) {
   ObjectCode* oc;

   IF_DEBUG(linker, debugBelch("mkOc: start\n"));
   oc = stgMallocBytes(sizeof(ObjectCode), "mkOc(oc)");

#  if defined(OBJFORMAT_ELF)
   oc->formatName = "ELF";
#  elif defined(OBJFORMAT_PEi386)
   oc->formatName = "PEi386";
#  elif defined(OBJFORMAT_MACHO)
   oc->formatName = "Mach-O";
#  else
   stgFree(oc);
   barf("loadObj: not implemented on this platform");
#  endif

   oc->image = image;
   oc->fileName = pathdup(path);

   if (archiveMemberName) {
       oc->archiveMemberName = stgMallocBytes( strlen(archiveMemberName)+1, "loadObj" );
       strcpy(oc->archiveMemberName, archiveMemberName);
   } else {
       oc->archiveMemberName = NULL;
   }

   setOcInitialStatus( oc );

   oc->fileSize          = imageSize;
   oc->symbols           = NULL;
   oc->n_sections        = 0;
   oc->sections          = NULL;
   oc->proddables        = NULL;
   oc->stable_ptrs       = NULL;
#if NEED_SYMBOL_EXTRAS
   oc->symbol_extras     = NULL;
#endif
   oc->imageMapped       = mapped;

   oc->misalignment      = misalignment;
   oc->extraInfos        = NULL;

   /* chain it onto the list of objects */
   oc->next              = NULL;

   IF_DEBUG(linker, debugBelch("mkOc: done\n"));
   return oc;
}

/* -----------------------------------------------------------------------------
 * Check if an object or archive is already loaded.
 *
 * Returns: 1 if the path is already loaded, 0 otherwise.
 */
static HsInt
isAlreadyLoaded( pathchar *path )
{
    ObjectCode *o;
    for (o = objects; o; o = o->next) {
       if (0 == pathcmp(o->fileName, path)) {
           return 1; /* already loaded */
       }
    }
    return 0; /* not loaded yet */
}

static HsInt loadArchive_ (pathchar *path)
{
    ObjectCode* oc;
    char *image;
    int memberSize;
    FILE *f;
    int n;
    size_t thisFileNameSize;
    char *fileName;
    size_t fileNameSize;
    int isObject, isGnuIndex, isThin, isImportLib;
    char tmp[20];
    char *gnuFileIndex;
    int gnuFileIndexSize;
#if defined(darwin_HOST_OS)
    int i;
    uint32_t nfat_arch, nfat_offset, cputype, cpusubtype;
#if defined(i386_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_X86;
    const uint32_t mycpusubtype = CPU_SUBTYPE_X86_ALL;
#elif defined(x86_64_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_X86_64;
    const uint32_t mycpusubtype = CPU_SUBTYPE_X86_64_ALL;
#elif defined(powerpc_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_POWERPC;
    const uint32_t mycpusubtype = CPU_SUBTYPE_POWERPC_ALL;
#elif defined(powerpc64_HOST_ARCH)
    const uint32_t mycputype = CPU_TYPE_POWERPC64;
    const uint32_t mycpusubtype = CPU_SUBTYPE_POWERPC_ALL;
#else
#error Unknown Darwin architecture
#endif
#endif
    int misalignment = 0;

    /* TODO: don't call barf() on error, instead return an error code, freeing
     * all resources correctly.  This function is pretty complex, so it needs
     * to be refactored to make this practical. */

    IF_DEBUG(linker, debugBelch("loadArchive: start\n"));
    IF_DEBUG(linker, debugBelch("loadArchive: Loading archive `%" PATH_FMT" '\n", path));

    /* Check that we haven't already loaded this archive.
       Ignore requests to load multiple times */
    if (isAlreadyLoaded(path)) {
        IF_DEBUG(linker,
                 debugBelch("ignoring repeated load of %" PATH_FMT "\n", path));
        return 1; /* success */
    }

    gnuFileIndex = NULL;
    gnuFileIndexSize = 0;

    fileNameSize = 32;
    fileName = stgMallocBytes(fileNameSize, "loadArchive(fileName)");

    isThin = 0;
    isImportLib = 0;

    f = pathopen(path, WSTR("rb"));
    if (!f)
        barf("loadObj: can't read `%" PATH_FMT "'", path);

    /* Check if this is an archive by looking for the magic "!<arch>\n"
     * string.  Usually, if this fails, we barf and quit.  On Darwin however,
     * we may have a fat archive, which contains archives for more than
     * one architecture.  Fat archives start with the magic number 0xcafebabe,
     * always stored big endian.  If we find a fat_header, we scan through
     * the fat_arch structs, searching through for one for our host
     * architecture.  If a matching struct is found, we read the offset
     * of our archive data (nfat_offset) and seek forward nfat_offset bytes
     * from the start of the file.
     *
     * A subtlety is that all of the members of the fat_header and fat_arch
     * structs are stored big endian, so we need to call byte order
     * conversion functions.
     *
     * If we find the appropriate architecture in a fat archive, we gobble
     * its magic "!<arch>\n" string and continue processing just as if
     * we had a single architecture archive.
     */

    n = fread ( tmp, 1, 8, f );
    if (n != 8)
        barf("loadArchive: Failed reading header from `%" PATH_FMT "'", path);
    if (strncmp(tmp, "!<arch>\n", 8) == 0) {}
    /* Check if this is a thin archive by looking for the magic string "!<thin>\n"
     *
     * ar thin libraries have the exact same format as normal archives except they
     * have a different magic string and they don't copy the object files into the
     * archive.
     *
     * Instead each header entry points to the location of the object file on disk.
     * This is useful when a library is only created to satisfy a compile time dependency
     * instead of to be distributed. This saves the time required for copying.
     *
     * Thin archives are always flattened. They always only contain simple headers
     * pointing to the object file and so we need not allocate more memory than needed
     * to find the object file.
     *
     */
    else if (strncmp(tmp, "!<thin>\n", 8) == 0) {
        isThin = 1;
    }
#if defined(darwin_HOST_OS)
    /* Not a standard archive, look for a fat archive magic number: */
    else if (ntohl(*(uint32_t *)tmp) == FAT_MAGIC) {
        nfat_arch = ntohl(*(uint32_t *)(tmp + 4));
        IF_DEBUG(linker, debugBelch("loadArchive: found a fat archive containing %d architectures\n", nfat_arch));
        nfat_offset = 0;

        for (i = 0; i < (int)nfat_arch; i++) {
            /* search for the right arch */
            n = fread( tmp, 1, 20, f );
            if (n != 8)
                barf("loadArchive: Failed reading arch from `%s'", path);
            cputype = ntohl(*(uint32_t *)tmp);
            cpusubtype = ntohl(*(uint32_t *)(tmp + 4));

            if (cputype == mycputype && cpusubtype == mycpusubtype) {
                IF_DEBUG(linker, debugBelch("loadArchive: found my archive in a fat archive\n"));
                nfat_offset = ntohl(*(uint32_t *)(tmp + 8));
                break;
            }
        }

        if (nfat_offset == 0) {
           barf ("loadArchive: searched %d architectures, but no host arch found", (int)nfat_arch);
        }
        else {
            n = fseek( f, nfat_offset, SEEK_SET );
            if (n != 0)
                barf("loadArchive: Failed to seek to arch in `%s'", path);
            n = fread ( tmp, 1, 8, f );
            if (n != 8)
                barf("loadArchive: Failed reading header from `%s'", path);
            if (strncmp(tmp, "!<arch>\n", 8) != 0) {
                barf("loadArchive: couldn't find archive in `%s' at offset %d", path, nfat_offset);
            }
        }
    }
    else {
        barf("loadArchive: Neither an archive, nor a fat archive: `%s'", path);
    }
#else
    else {
        barf("loadArchive: Not an archive: `%" PATH_FMT "'", path);
    }
#endif

    IF_DEBUG(linker, debugBelch("loadArchive: loading archive contents\n"));

    while (1) {
        IF_DEBUG(linker, debugBelch("loadArchive: reading at %ld\n", ftell(f)));
        n = fread ( fileName, 1, 16, f );
        if (n != 16) {
            if (feof(f)) {
                IF_DEBUG(linker, debugBelch("loadArchive: EOF while reading from '%" PATH_FMT "'\n", path));
                break;
            }
            else {
                barf("loadArchive: Failed reading file name from `%" PATH_FMT "'", path);
            }
        }

#if defined(darwin_HOST_OS)
        if (strncmp(fileName, "!<arch>\n", 8) == 0) {
            IF_DEBUG(linker, debugBelch("loadArchive: found the start of another archive, breaking\n"));
            break;
        }
#endif

        n = fread ( tmp, 1, 12, f );
        if (n != 12)
            barf("loadArchive: Failed reading mod time from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            barf("loadArchive: Failed reading owner from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 6, f );
        if (n != 6)
            barf("loadArchive: Failed reading group from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 8, f );
        if (n != 8)
            barf("loadArchive: Failed reading mode from `%" PATH_FMT "'", path);
        n = fread ( tmp, 1, 10, f );
        if (n != 10)
            barf("loadArchive: Failed reading size from `%" PATH_FMT "'", path);
        tmp[10] = '\0';
        for (n = 0; isdigit(tmp[n]); n++);
        tmp[n] = '\0';
        memberSize = atoi(tmp);

        IF_DEBUG(linker, debugBelch("loadArchive: size of this archive member is %d\n", memberSize));
        n = fread ( tmp, 1, 2, f );
        if (n != 2)
            barf("loadArchive: Failed reading magic from `%" PATH_FMT "'", path);
        if (strncmp(tmp, "\x60\x0A", 2) != 0)
            barf("loadArchive: Failed reading magic from `%" PATH_FMT "' at %ld. Got %c%c",
                 path, ftell(f), tmp[0], tmp[1]);

        isGnuIndex = 0;
        /* Check for BSD-variant large filenames */
        if (0 == strncmp(fileName, "#1/", 3)) {
            fileName[16] = '\0';
            if (isdigit(fileName[3])) {
                for (n = 4; isdigit(fileName[n]); n++);
                fileName[n] = '\0';
                thisFileNameSize = atoi(fileName + 3);
                memberSize -= thisFileNameSize;
                if (thisFileNameSize >= fileNameSize) {
                    /* Double it to avoid potentially continually
                       increasing it by 1 */
                    fileNameSize = thisFileNameSize * 2;
                    fileName = stgReallocBytes(fileName, fileNameSize, "loadArchive(fileName)");
                }
                n = fread ( fileName, 1, thisFileNameSize, f );
                if (n != (int)thisFileNameSize) {
                    barf("loadArchive: Failed reading filename from `%" PATH_FMT "'",
                         path);
                }
                fileName[thisFileNameSize] = 0;

                /* On OS X at least, thisFileNameSize is the size of the
                   fileName field, not the length of the fileName
                   itself. */
                thisFileNameSize = strlen(fileName);
            }
            else {
                barf("loadArchive: BSD-variant filename size not found while reading filename from `%s'", path);
            }
        }
        /* Check for GNU file index file */
        else if (0 == strncmp(fileName, "//", 2)) {
            fileName[0] = '\0';
            thisFileNameSize = 0;
            isGnuIndex = 1;
        }
        /* Check for a file in the GNU file index */
        else if (fileName[0] == '/') {
            if (isdigit(fileName[1])) {
                int i;

                for (n = 2; isdigit(fileName[n]); n++);
                fileName[n] = '\0';
                n = atoi(fileName + 1);

                if (gnuFileIndex == NULL) {
                    barf("loadArchive: GNU-variant filename without an index while reading from `%s'", path);
                }
                if (n < 0 || n > gnuFileIndexSize) {
                    barf("loadArchive: GNU-variant filename offset %d out of range [0..%d] while reading filename from `%s'", n, gnuFileIndexSize, path);
                }
                if (n != 0 && gnuFileIndex[n - 1] != '\n') {
                    barf("loadArchive: GNU-variant filename offset %d invalid (range [0..%d]) while reading filename from `%s'", n, gnuFileIndexSize, path);
                }
                for (i = n; gnuFileIndex[i] != '\n'; i++);
                thisFileNameSize = i - n - 1;
                if (thisFileNameSize >= fileNameSize) {
                    /* Double it to avoid potentially continually
                       increasing it by 1 */
                    fileNameSize = thisFileNameSize * 2;
                    fileName = stgReallocBytes(fileName, fileNameSize, "loadArchive(fileName)");
                }
                memcpy(fileName, gnuFileIndex + n, thisFileNameSize);
                fileName[thisFileNameSize] = '\0';
            }
            else if (fileName[1] == ' ') {
                fileName[0] = '\0';
                thisFileNameSize = 0;
            }
            else {
                barf("loadArchive: GNU-variant filename offset not found while reading filename from `%s'", path);
            }
        }
        /* Finally, the case where the filename field actually contains
           the filename */
        else {
            /* GNU ar terminates filenames with a '/', this allowing
               spaces in filenames. So first look to see if there is a
               terminating '/'. */
            for (thisFileNameSize = 0;
                 thisFileNameSize < 16;
                 thisFileNameSize++) {
                if (fileName[thisFileNameSize] == '/') {
                    fileName[thisFileNameSize] = '\0';
                    break;
                }
            }
            /* If we didn't find a '/', then a space teminates the
               filename. Note that if we don't find one, then
               thisFileNameSize ends up as 16, and we already have the
               '\0' at the end. */
            if (thisFileNameSize == 16) {
                for (thisFileNameSize = 0;
                     thisFileNameSize < 16;
                     thisFileNameSize++) {
                    if (fileName[thisFileNameSize] == ' ') {
                        fileName[thisFileNameSize] = '\0';
                        break;
                    }
                }
            }
        }

        IF_DEBUG(linker,
                 debugBelch("loadArchive: Found member file `%s'\n", fileName));

        isObject = (thisFileNameSize >= 2 && strncmp(fileName + thisFileNameSize - 2, ".o"  , 2) == 0)
                || (thisFileNameSize >= 4 && strncmp(fileName + thisFileNameSize - 4, ".p_o", 4) == 0);

#if defined(OBJFORMAT_PEi386)
        /*
        * Note [MSVC import files (ext .lib)]
        * MSVC compilers store the object files in
        * the import libraries with extension .dll
        * so on Windows we should look for those too.
        * The PE COFF format doesn't specify any specific file name
        * for sections. So on windows, just try to load it all.
        *
        * Linker members (e.g. filename / are skipped since they are not needed)
        */
        isImportLib = thisFileNameSize >= 4 && strncmp(fileName + thisFileNameSize - 4, ".dll", 4) == 0;

        /*
         * Note [GCC import files (ext .dll.a)]
         * GCC stores import information in the same binary format
         * as the object file normally has. The only difference is that
         * all the information are put in .idata sections. The only real
         * way to tell if we're dealing with an import lib is by looking
         * at the file extension.
         */
        isImportLib = isImportLib || endsWithPath(path, WSTR(".dll.a"));
#endif // windows

        IF_DEBUG(linker, debugBelch("loadArchive: \tthisFileNameSize = %d\n", (int)thisFileNameSize));
        IF_DEBUG(linker, debugBelch("loadArchive: \tisObject = %d\n", isObject));

        if (isObject) {
            char *archiveMemberName;

            IF_DEBUG(linker, debugBelch("loadArchive: Member is an object file...loading...\n"));

#if defined(mingw32_HOST_OS)
            // TODO: We would like to use allocateExec here, but allocateExec
            //       cannot currently allocate blocks large enough.
            image = allocateImageAndTrampolines(path, fileName, f, memberSize,
                                                isThin);
#elif defined(darwin_HOST_OS)
            if (RTS_LINKER_USE_MMAP)
                image = mmapForLinker(memberSize, MAP_ANONYMOUS, -1, 0);
            else {
                /* See loadObj() */
                misalignment = machoGetMisalignment(f);
                image = stgMallocBytes(memberSize + misalignment,
                                        "loadArchive(image)");
                image += misalignment;
            }

#else // not windows or darwin
            image = stgMallocBytes(memberSize, "loadArchive(image)");
#endif
            if (isThin) {
                FILE *member;
                pathchar *pathCopy, *dirName, *memberPath, *objFileName;

                /* Allocate and setup the dirname of the archive.  We'll need
                    this to locate the thin member */
                pathCopy = pathdup(path); // Convert the char* to a pathchar*
                dirName  = pathdir(pathCopy);

                /* Append the relative member name to the dirname.  This should be
                   be the full path to the actual thin member. */
                int memberLen = pathlen(dirName) + 1 + strlen(fileName) + 1;
                memberPath    = stgMallocBytes(pathsize * memberLen, "loadArchive(file)");
                objFileName   = mkPath(fileName);
                pathprintf(memberPath, memberLen, WSTR("%" PATH_FMT "%" PATH_FMT), dirName, objFileName);
                stgFree(objFileName);
                stgFree(dirName);

                member = pathopen(memberPath, WSTR("rb"));
                if (!member)
                    barf("loadObj: can't read thin archive `%" PATH_FMT "'", memberPath);

                n = fread ( image, 1, memberSize, member );
                if (n != memberSize) {
                    barf("loadArchive: error whilst reading `%s'", fileName);
                }

                fclose(member);
                stgFree(memberPath);
                stgFree(pathCopy);
            }
            else
            {
                n = fread ( image, 1, memberSize, f );
                if (n != memberSize) {
                    barf("loadArchive: error whilst reading `%" PATH_FMT "'", path);
                }
            }

            archiveMemberName = stgMallocBytes(pathlen(path) + thisFileNameSize + 3,
                                               "loadArchive(file)");
            sprintf(archiveMemberName, "%" PATH_FMT "(%.*s)",
                    path, (int)thisFileNameSize, fileName);

            oc = mkOc(path, image, memberSize, rtsFalse, archiveMemberName
                     , misalignment);

            stgFree(archiveMemberName);

            if (0 == loadOc(oc)) {
                stgFree(fileName);
                fclose(f);
                return 0;
            } else {
#if defined(OBJFORMAT_PEi386)
                if (isImportLib)
                {
                    findAndLoadImportLibrary(oc);
                    stgFree(oc);
                    oc = NULL;
                    break;
                } else {
#endif
                    oc->next = objects;
                    objects = oc;
#if defined(OBJFORMAT_PEi386)
                }
#endif
            }
        }
        else if (isGnuIndex) {
            if (gnuFileIndex != NULL) {
                barf("loadArchive: GNU-variant index found, but already have an index, while reading filename from `%s'", path);
            }
            IF_DEBUG(linker, debugBelch("loadArchive: Found GNU-variant file index\n"));
#if RTS_LINKER_USE_MMAP
            gnuFileIndex = mmapForLinker(memberSize + 1, MAP_ANONYMOUS, -1, 0);
#else
            gnuFileIndex = stgMallocBytes(memberSize + 1, "loadArchive(image)");
#endif
            n = fread ( gnuFileIndex, 1, memberSize, f );
            if (n != memberSize) {
                barf("loadArchive: error whilst reading `%" PATH_FMT "'", path);
            }
            gnuFileIndex[memberSize] = '/';
            gnuFileIndexSize = memberSize;
        }
        else if (isImportLib) {
#if defined(OBJFORMAT_PEi386)
            if (checkAndLoadImportLibrary(path, fileName, f)) {
                IF_DEBUG(linker, debugBelch("loadArchive: Member is an import file section... Corresponding DLL has been loaded...\n"));
            }
            else {
                IF_DEBUG(linker, debugBelch("loadArchive: Member is not a valid import file section... Skipping...\n"));
                n = fseek(f, memberSize, SEEK_CUR);
                if (n != 0)
                    barf("loadArchive: error whilst seeking by %d in `%" PATH_FMT "'",
                    memberSize, path);
            }
#endif
        }
        else {
            IF_DEBUG(linker, debugBelch("loadArchive: '%s' does not appear to be an object file\n", fileName));
            if (!isThin || thisFileNameSize == 0) {
                n = fseek(f, memberSize, SEEK_CUR);
                if (n != 0)
                    barf("loadArchive: error whilst seeking by %d in `%" PATH_FMT "'",
                         memberSize, path);
            }
        }

        /* .ar files are 2-byte aligned */
        if (!(isThin && thisFileNameSize > 0) && memberSize % 2) {
            IF_DEBUG(linker, debugBelch("loadArchive: trying to read one pad byte\n"));
            n = fread ( tmp, 1, 1, f );
            if (n != 1) {
                if (feof(f)) {
                    IF_DEBUG(linker, debugBelch("loadArchive: found EOF while reading one pad byte\n"));
                    break;
                }
                else {
                    barf("loadArchive: Failed reading padding from `%" PATH_FMT "'", path);
                }
            }
            IF_DEBUG(linker, debugBelch("loadArchive: successfully read one pad byte\n"));
        }
        IF_DEBUG(linker, debugBelch("loadArchive: reached end of archive loading while loop\n"));
    }

    fclose(f);

    stgFree(fileName);
    if (gnuFileIndex != NULL) {
#if RTS_LINKER_USE_MMAP
        munmap(gnuFileIndex, gnuFileIndexSize + 1);
#else
        stgFree(gnuFileIndex);
#endif
    }

    if (RTS_LINKER_USE_MMAP)
        m32_allocator_flush();

    IF_DEBUG(linker, debugBelch("loadArchive: done\n"));
    return 1;
}

HsInt loadArchive (pathchar *path)
{
   ACQUIRE_LOCK(&linker_mutex);
   HsInt r = loadArchive_(path);
   RELEASE_LOCK(&linker_mutex);
   return r;
}

//
// Load the object file into memory.  This will not be its final resting place,
// as on 64-bit platforms we need to map its segments into the low 2Gb of the
// address space, properly aligned.
//
static ObjectCode *
preloadObjectFile (pathchar *path)
{
   int fileSize;
   struct_stat st;
   int r;
   void *image;
   ObjectCode *oc;
   int misalignment = 0;

   r = pathstat(path, &st);
   if (r == -1) {
       errorBelch("loadObj: %" PATH_FMT ": file doesn't exist", path);
       return NULL;
   }

   fileSize = st.st_size;

#if RTS_LINKER_USE_MMAP
   int fd;

   /* On many architectures malloc'd memory isn't executable, so we need to use
    * mmap. */

#if defined(openbsd_HOST_OS)
   fd = open(path, O_RDONLY, S_IRUSR);
#else
   fd = open(path, O_RDONLY);
#endif
   if (fd == -1) {
      errorBelch("loadObj: can't open %s", path);
      return NULL;
   }

   image = mmap(NULL, fileSize, PROT_READ|PROT_WRITE|PROT_EXEC,
                MAP_PRIVATE, fd, 0);
       // not 32-bit yet, we'll remap later
   close(fd);

#else /* !RTS_LINKER_USE_MMAP */
   FILE *f;

   /* load the image into memory */
   /* coverity[toctou] */
   f = pathopen(path, WSTR("rb"));
   if (!f) {
       errorBelch("loadObj: can't preload `%" PATH_FMT "'", path);
       return NULL;
   }

#  if defined(mingw32_HOST_OS)

        // TODO: We would like to use allocateExec here, but allocateExec
        //       cannot currently allocate blocks large enough.
    image = allocateImageAndTrampolines(path, "itself", f, fileSize,
                                        HS_BOOL_FALSE);
    if (image == NULL) {
        fclose(f);
        return NULL;
    }

#   elif defined(darwin_HOST_OS)

    // In a Mach-O .o file, all sections can and will be misaligned
    // if the total size of the headers is not a multiple of the
    // desired alignment. This is fine for .o files that only serve
    // as input for the static linker, but it's not fine for us,
    // as SSE (used by gcc for floating point) and Altivec require
    // 16-byte alignment.
    // We calculate the correct alignment from the header before
    // reading the file, and then we misalign image on purpose so
    // that the actual sections end up aligned again.
   misalignment = machoGetMisalignment(f);
   image = stgMallocBytes(fileSize + misalignment, "loadObj(image)");
   image += misalignment;

# else /* !defined(mingw32_HOST_OS) */

   image = stgMallocBytes(fileSize, "loadObj(image)");

#endif

   int n;
   n = fread ( image, 1, fileSize, f );
   fclose(f);
   if (n != fileSize) {
       errorBelch("loadObj: error whilst reading `%" PATH_FMT "'", path);
       stgFree(image);
       return NULL;
   }

#endif /* RTS_LINKER_USE_MMAP */

   oc = mkOc(path, image, fileSize, rtsTrue, NULL, misalignment);

   return oc;
}

/* -----------------------------------------------------------------------------
 * Load an obj (populate the global symbol table, but don't resolve yet)
 *
 * Returns: 1 if ok, 0 on error.
 */
static HsInt loadObj_ (pathchar *path)
{
   ObjectCode* oc;
   IF_DEBUG(linker, debugBelch("loadObj %" PATH_FMT "\n", path));

   /* debugBelch("loadObj %s\n", path ); */

   /* Check that we haven't already loaded this object.
      Ignore requests to load multiple times */

   if (isAlreadyLoaded(path)) {
       IF_DEBUG(linker,
                debugBelch("ignoring repeated load of %" PATH_FMT "\n", path));
       return 1; /* success */
   }

   oc = preloadObjectFile(path);
   if (oc == NULL) return 0;

   if (! loadOc(oc)) {
       // failed; free everything we've allocated
       removeOcSymbols(oc);
       // no need to freeOcStablePtrs, they aren't created until resolveObjs()
       freeObjectCode(oc);
       return 0;
   }

   oc->next = objects;
   objects = oc;
   return 1;
}

HsInt loadObj (pathchar *path)
{
   ACQUIRE_LOCK(&linker_mutex);
   HsInt r = loadObj_(path);
   RELEASE_LOCK(&linker_mutex);
   return r;
}

static HsInt loadOc (ObjectCode* oc)
{
   int r;

   IF_DEBUG(linker, debugBelch("loadOc: start\n"));

   /* verify the in-memory image */
#  if defined(OBJFORMAT_ELF)
   r = ocVerifyImage_ELF ( oc );
#  elif defined(OBJFORMAT_PEi386)
   r = ocVerifyImage_PEi386 ( oc );
#  elif defined(OBJFORMAT_MACHO)
   r = ocVerifyImage_MachO ( oc );
#  else
   barf("loadObj: no verify method");
#  endif
   if (!r) {
       IF_DEBUG(linker, debugBelch("loadOc: ocVerifyImage_* failed\n"));
       return r;
   }

#if NEED_SYMBOL_EXTRAS
#  if defined(OBJFORMAT_MACHO)
   r = ocAllocateSymbolExtras_MachO ( oc );
   if (!r) {
       IF_DEBUG(linker, debugBelch("loadOc: ocAllocateSymbolExtras_MachO failed\n"));
       return r;
   }
#  elif defined(OBJFORMAT_ELF)
   r = ocAllocateSymbolExtras_ELF ( oc );
   if (!r) {
       IF_DEBUG(linker, debugBelch("loadOc: ocAllocateSymbolExtras_ELF failed\n"));
       return r;
   }
#  elif defined(OBJFORMAT_PEi386)
   ocAllocateSymbolExtras_PEi386 ( oc );
#  endif
#endif

   /* build the symbol list for this image */
#  if defined(OBJFORMAT_ELF)
   r = ocGetNames_ELF ( oc );
#  elif defined(OBJFORMAT_PEi386)
   r = ocGetNames_PEi386 ( oc );
#  elif defined(OBJFORMAT_MACHO)
   r = ocGetNames_MachO ( oc );
#  else
   barf("loadObj: no getNames method");
#  endif
   if (!r) {
       IF_DEBUG(linker, debugBelch("loadOc: ocGetNames_* failed\n"));
       return r;
   }

   /* loaded, but not resolved yet, ensure the OC is in a consistent state */
   setOcInitialStatus( oc );
   IF_DEBUG(linker, debugBelch("loadOc: done.\n"));

   return 1;
}

/* -----------------------------------------------------------------------------
* try to load and initialize an ObjectCode into memory
*
* Returns: 1 if ok, 0 on error.
*/
int ocTryLoad (ObjectCode* oc) {
    int r;

    if (oc->status != OBJECT_NEEDED) {
        return 1;
    }

    /*  Check for duplicate symbols by looking into `symhash`.
        Duplicate symbols are any symbols which exist
        in different ObjectCodes that have both been loaded, or
        are to be loaded by this call.

        This call is intended to have no side-effects when a non-duplicate
        symbol is re-inserted.

        We set the Address to NULL since that is not used to distinguish
        symbols. Duplicate symbols are distinguished by name and oc.
    */
    int x;
    SymbolName* symbol;
    for (x = 0; x < oc->n_symbols; x++) {
        symbol = oc->symbols[x];
        if (   symbol
            && !ghciInsertSymbolTable(oc->fileName, symhash, symbol, NULL, isSymbolWeak(oc, symbol), oc)) {
            return 0;
        }
    }

#           if defined(OBJFORMAT_ELF)
        r = ocResolve_ELF ( oc );
#           elif defined(OBJFORMAT_PEi386)
        r = ocResolve_PEi386 ( oc );
#           elif defined(OBJFORMAT_MACHO)
        r = ocResolve_MachO ( oc );
#           else
    barf("ocTryLoad: not implemented on this platform");
#           endif
        if (!r) { return r; }

        // run init/init_array/ctors/mod_init_func

        loading_obj = oc; // tells foreignExportStablePtr what to do
#if defined(OBJFORMAT_ELF)
        r = ocRunInit_ELF ( oc );
#elif defined(OBJFORMAT_PEi386)
        r = ocRunInit_PEi386 ( oc );
#elif defined(OBJFORMAT_MACHO)
        r = ocRunInit_MachO ( oc );
#else
    barf("ocTryLoad: initializers not implemented on this platform");
#endif
        loading_obj = NULL;

        if (!r) { return r; }

    oc->status = OBJECT_RESOLVED;

    return 1;
}

/* -----------------------------------------------------------------------------
 * resolve all the currently unlinked objects in memory
 *
 * Returns: 1 if ok, 0 on error.
 */
static HsInt resolveObjs_ (void)
{
    ObjectCode *oc;
    int r;

    IF_DEBUG(linker, debugBelch("resolveObjs: start\n"));

    for (oc = objects; oc; oc = oc->next) {
        r = ocTryLoad(oc);
        if (!r)
        {
            return r;
        }
    }

#ifdef PROFILING
    // collect any new cost centres & CCSs that were defined during runInit
    initProfiling2();
#endif

    IF_DEBUG(linker, debugBelch("resolveObjs: done\n"));
    return 1;
}

HsInt resolveObjs (void)
{
    ACQUIRE_LOCK(&linker_mutex);
    HsInt r = resolveObjs_();
    RELEASE_LOCK(&linker_mutex);
    return r;
}

/* -----------------------------------------------------------------------------
 * delete an object from the pool
 */
static HsInt unloadObj_ (pathchar *path, rtsBool just_purge)
{
    ObjectCode *oc, *prev, *next;
    HsBool unloadedAnyObj = HS_BOOL_FALSE;

    ASSERT(symhash != NULL);
    ASSERT(objects != NULL);

    IF_DEBUG(linker, debugBelch("unloadObj: %" PATH_FMT "\n", path));

    prev = NULL;
    for (oc = objects; oc; oc = next) {
        next = oc->next; // oc might be freed

        if (!pathcmp(oc->fileName,path)) {

            // these are both idempotent, so in just_purge mode we can
            // later call unloadObj() to really unload the object.
            removeOcSymbols(oc);
            freeOcStablePtrs(oc);

            if (!just_purge) {
                if (prev == NULL) {
                    objects = oc->next;
                } else {
                    prev->next = oc->next;
                }
                ACQUIRE_LOCK(&linker_unloaded_mutex);
                oc->next = unloaded_objects;
                unloaded_objects = oc;
                oc->status = OBJECT_UNLOADED;
                RELEASE_LOCK(&linker_unloaded_mutex);
                // We do not own oc any more; it can be released at any time by
                // the GC in checkUnload().
            } else {
                prev = oc;
            }

            /* This could be a member of an archive so continue
             * unloading other members. */
            unloadedAnyObj = HS_BOOL_TRUE;
        } else {
            prev = oc;
        }
    }

    if (unloadedAnyObj) {
        return 1;
    }
    else {
        errorBelch("unloadObj: can't find `%" PATH_FMT "' to unload", path);
        return 0;
    }
}

HsInt unloadObj (pathchar *path)
{
    ACQUIRE_LOCK(&linker_mutex);
    HsInt r = unloadObj_(path, rtsFalse);
    RELEASE_LOCK(&linker_mutex);
    return r;
}

HsInt purgeObj (pathchar *path)
{
    ACQUIRE_LOCK(&linker_mutex);
    HsInt r = unloadObj_(path, rtsTrue);
    RELEASE_LOCK(&linker_mutex);
    return r;
}

/* -----------------------------------------------------------------------------
 * Sanity checking.  For each ObjectCode, maintain a list of address ranges
 * which may be prodded during relocation, and abort if we try and write
 * outside any of these.
 */
void
addProddableBlock ( ObjectCode* oc, void* start, int size )
{
   ProddableBlock* pb
      = stgMallocBytes(sizeof(ProddableBlock), "addProddableBlock");

   IF_DEBUG(linker, debugBelch("addProddableBlock: %p %p %d\n", oc, start, size));
   ASSERT(size > 0);
   pb->start      = start;
   pb->size       = size;
   pb->next       = oc->proddables;
   oc->proddables = pb;
}

void
checkProddableBlock (ObjectCode *oc, void *addr, size_t size )
{
   ProddableBlock* pb;

   for (pb = oc->proddables; pb != NULL; pb = pb->next) {
      char* s = (char*)(pb->start);
      char* e = s + pb->size;
      char* a = (char*)addr;
      if (a >= s && (a+size) <= e) return;
   }
   barf("checkProddableBlock: invalid fixup in runtime linker: %p", addr);
}

void freeProddableBlocks (ObjectCode *oc)
{
    ProddableBlock *pb, *next;

    for (pb = oc->proddables; pb != NULL; pb = next) {
        next = pb->next;
        stgFree(pb);
    }
    oc->proddables = NULL;
}

/* -----------------------------------------------------------------------------
 * Section management.
 */
void
addSection (Section *s, SectionKind kind, SectionAlloc alloc,
            void* start, StgWord size, StgWord mapped_offset,
            void* mapped_start, StgWord mapped_size)
{
   s->start        = start;     /* actual start of section in memory */
   s->size         = size;      /* actual size of section in memory */
   s->kind         = kind;
   s->alloc        = alloc;
   s->mapped_offset = mapped_offset; /* offset from the image of mapped_start */

   s->mapped_start = mapped_start; /* start of mmap() block */
   s->mapped_size  = mapped_size;  /* size of mmap() block */

   IF_DEBUG(linker,
            debugBelch("addSection: %p-%p (size %" FMT_Word "), kind %d\n",
                       start, (void*)((StgWord)start + size),
                       size, kind ));
}

/* --------------------------------------------------------------------------
 * ELF specifics
 * ------------------------------------------------------------------------*/

#if defined(OBJFORMAT_ELF)

#define FALSE 0
#define TRUE  1

#if defined(sparc_HOST_ARCH)
#  define ELF_TARGET_SPARC  /* Used inside <elf.h> */
#elif defined(i386_HOST_ARCH)
#  define ELF_TARGET_386    /* Used inside <elf.h> */
#elif defined(x86_64_HOST_ARCH)
#  define ELF_TARGET_X64_64
#  define ELF_64BIT
#  define ELF_TARGET_AMD64 /* Used inside <elf.h> on Solaris 11 */
#elif defined(powerpc64_HOST_ARCH) || defined(powerpc64le_HOST_ARCH)
#  define ELF_64BIT
#elif defined(ia64_HOST_ARCH)
#  define ELF_64BIT
#elif defined(aarch64_HOST_ARCH)
#  define ELF_64BIT
#endif

#if !defined(openbsd_HOST_OS)
#  include <elf.h>
#else
/* openbsd elf has things in different places, with diff names */
#  include <elf_abi.h>
#  include <machine/reloc.h>
#  define R_386_32    RELOC_32
#  define R_386_PC32  RELOC_PC32
#endif

/* If elf.h doesn't define it */
#  ifndef R_X86_64_PC64
#    define R_X86_64_PC64 24
#  endif

/*
 * Workaround for libc implementations (e.g. eglibc) with incomplete
 * relocation lists
 */
#ifndef R_ARM_THM_CALL
#  define R_ARM_THM_CALL      10
#endif
#ifndef R_ARM_CALL
#  define R_ARM_CALL      28
#endif
#ifndef R_ARM_JUMP24
#  define R_ARM_JUMP24      29
#endif
#ifndef R_ARM_THM_JUMP24
#  define R_ARM_THM_JUMP24      30
#endif
#ifndef R_ARM_TARGET1
#  define R_ARM_TARGET1      38
#endif
#ifndef R_ARM_MOVW_ABS_NC
#  define R_ARM_MOVW_ABS_NC      43
#endif
#ifndef R_ARM_MOVT_ABS
#  define R_ARM_MOVT_ABS      44
#endif
#ifndef R_ARM_THM_MOVW_ABS_NC
#  define R_ARM_THM_MOVW_ABS_NC   47
#endif
#ifndef R_ARM_THM_MOVT_ABS
#  define R_ARM_THM_MOVT_ABS      48
#endif
#ifndef R_ARM_THM_JUMP11
#  define R_ARM_THM_JUMP11      102
#endif
#ifndef R_ARM_THM_JUMP8
#  define R_ARM_THM_JUMP8      103
#endif

/*
 * Define a set of types which can be used for both ELF32 and ELF64
 */

#ifdef ELF_64BIT
#define ELFCLASS    ELFCLASS64
#define Elf_Addr    Elf64_Addr
#define Elf_Word    Elf64_Word
#define Elf_Sword   Elf64_Sword
#define Elf_Half    Elf64_Half
#define Elf_Ehdr    Elf64_Ehdr
#define Elf_Phdr    Elf64_Phdr
#define Elf_Shdr    Elf64_Shdr
#define Elf_Sym     Elf64_Sym
#define Elf_Rel     Elf64_Rel
#define Elf_Rela    Elf64_Rela
#ifndef ELF_ST_TYPE
#define ELF_ST_TYPE ELF64_ST_TYPE
#endif
#ifndef ELF_ST_BIND
#define ELF_ST_BIND ELF64_ST_BIND
#endif
#ifndef ELF_R_TYPE
#define ELF_R_TYPE  ELF64_R_TYPE
#endif
#ifndef ELF_R_SYM
#define ELF_R_SYM   ELF64_R_SYM
#endif
#else
#define ELFCLASS    ELFCLASS32
#define Elf_Addr    Elf32_Addr
#define Elf_Word    Elf32_Word
#define Elf_Sword   Elf32_Sword
#define Elf_Half    Elf32_Half
#define Elf_Ehdr    Elf32_Ehdr
#define Elf_Phdr    Elf32_Phdr
#define Elf_Shdr    Elf32_Shdr
#define Elf_Sym     Elf32_Sym
#define Elf_Rel     Elf32_Rel
#define Elf_Rela    Elf32_Rela
#ifndef ELF_ST_TYPE
#define ELF_ST_TYPE ELF32_ST_TYPE
#endif
#ifndef ELF_ST_BIND
#define ELF_ST_BIND ELF32_ST_BIND
#endif
#ifndef ELF_R_TYPE
#define ELF_R_TYPE  ELF32_R_TYPE
#endif
#ifndef ELF_R_SYM
#define ELF_R_SYM   ELF32_R_SYM
#endif
#endif


/*
 * Functions to allocate entries in dynamic sections.  Currently we simply
 * preallocate a large number, and we don't check if a entry for the given
 * target already exists (a linear search is too slow).  Ideally these
 * entries would be associated with symbols.
 */

/* These sizes sufficient to load HSbase + HShaskell98 + a few modules */
#define GOT_SIZE            0x20000
#define FUNCTION_TABLE_SIZE 0x10000
#define PLT_SIZE            0x08000

#ifdef ELF_NEED_GOT
static Elf_Addr got[GOT_SIZE];
static unsigned int gotIndex;
static Elf_Addr gp_val = (Elf_Addr)got;

static Elf_Addr
allocateGOTEntry(Elf_Addr target)
{
   Elf_Addr *entry;

   if (gotIndex >= GOT_SIZE)
      barf("Global offset table overflow");

   entry = &got[gotIndex++];
   *entry = target;
   return (Elf_Addr)entry;
}
#endif

#ifdef ELF_FUNCTION_DESC
typedef struct {
   Elf_Addr ip;
   Elf_Addr gp;
} FunctionDesc;

static FunctionDesc functionTable[FUNCTION_TABLE_SIZE];
static unsigned int functionTableIndex;

static Elf_Addr
allocateFunctionDesc(Elf_Addr target)
{
   FunctionDesc *entry;

   if (functionTableIndex >= FUNCTION_TABLE_SIZE)
      barf("Function table overflow");

   entry = &functionTable[functionTableIndex++];
   entry->ip = target;
   entry->gp = (Elf_Addr)gp_val;
   return (Elf_Addr)entry;
}

static Elf_Addr
copyFunctionDesc(Elf_Addr target)
{
   FunctionDesc *olddesc = (FunctionDesc *)target;
   FunctionDesc *newdesc;

   newdesc = (FunctionDesc *)allocateFunctionDesc(olddesc->ip);
   newdesc->gp = olddesc->gp;
   return (Elf_Addr)newdesc;
}
#endif

#ifdef ELF_NEED_PLT

typedef struct {
   unsigned char code[sizeof(plt_code)];
} PLTEntry;

static Elf_Addr
allocatePLTEntry(Elf_Addr target, ObjectCode *oc)
{
   PLTEntry *plt = (PLTEntry *)oc->plt;
   PLTEntry *entry;

   if (oc->pltIndex >= PLT_SIZE)
      barf("Procedure table overflow");

   entry = &plt[oc->pltIndex++];
   memcpy(entry->code, plt_code, sizeof(entry->code));
   PLT_RELOC(entry->code, target);
   return (Elf_Addr)entry;
}

static unsigned int
PLTSize(void)
{
   return (PLT_SIZE * sizeof(PLTEntry));
}
#endif

/*

   Note [Many ELF Sections]

   The normal section number fields in ELF are limited to 16 bits, which runs
   out of bits when you try to cram in more sections than that.

   To solve this, the fields e_shnum and e_shstrndx in the ELF header have an
   escape value (different for each case), and the actual section number is
   stashed into unused fields in the first section header.

   For symbols, there seems to have been no place in the actual symbol table
   for the extra bits, so the indexes have been moved into an auxilliary
   section instead.
   For symbols in sections beyond 0xff00, the symbol's st_shndx will be an
   escape value (SHN_XINDEX), and the actual 32-bit section number for symbol N
   is stored at index N in the SHT_SYMTAB_SHNDX table.

   These extensions seem to be undocumented in version 4.1 of the ABI and only
   appear in the drafts for the "next" version:
      https://refspecs.linuxfoundation.org/elf/gabi4+/contents.html

*/

static Elf_Word elf_shnum(Elf_Ehdr* ehdr)
{
   Elf_Shdr* shdr = (Elf_Shdr*) ((char*)ehdr + ehdr->e_shoff);
   Elf_Half shnum = ehdr->e_shnum;
   return shnum != SHN_UNDEF ? shnum : shdr[0].sh_size;
}

static Elf_Word elf_shstrndx(Elf_Ehdr* ehdr)
{
   Elf_Shdr* shdr = (Elf_Shdr*) ((char*)ehdr + ehdr->e_shoff);
   Elf_Half shstrndx = ehdr->e_shstrndx;
#if defined(SHN_XINDEX)
   return shstrndx != SHN_XINDEX ? shstrndx : shdr[0].sh_link;
#else
   // some OSes do not support SHN_XINDEX yet, let's revert to
   // old way
   return shstrndx;
#endif
}

#if defined(SHN_XINDEX)
static Elf_Word*
get_shndx_table(Elf_Ehdr* ehdr)
{
   Elf_Word  i;
   char*     ehdrC    = (char*)ehdr;
   Elf_Shdr* shdr     = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   const Elf_Word shnum = elf_shnum(ehdr);

   for (i = 0; i < shnum; i++) {
     if (shdr[i].sh_type == SHT_SYMTAB_SHNDX) {
       return (Elf32_Word*)(ehdrC + shdr[i].sh_offset);
     }
   }
   return NULL;
}
#endif

/*
 * Generic ELF functions
 */

static int
ocVerifyImage_ELF ( ObjectCode* oc )
{
   Elf_Shdr* shdr;
   Elf_Sym*  stab;
   int j, nent, nstrtab, nsymtabs;
   Elf_Word i, shnum, shstrndx;
   char* sh_strtab;

   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*)ehdrC;

   if (ehdr->e_ident[EI_MAG0] != ELFMAG0 ||
       ehdr->e_ident[EI_MAG1] != ELFMAG1 ||
       ehdr->e_ident[EI_MAG2] != ELFMAG2 ||
       ehdr->e_ident[EI_MAG3] != ELFMAG3) {
      errorBelch("%s: not an ELF object", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[EI_CLASS] != ELFCLASS) {
      errorBelch("%s: unsupported ELF format", oc->fileName);
      return 0;
   }

   if (ehdr->e_ident[EI_DATA] == ELFDATA2LSB) {
       IF_DEBUG(linker,debugBelch( "Is little-endian\n" ));
   } else
   if (ehdr->e_ident[EI_DATA] == ELFDATA2MSB) {
       IF_DEBUG(linker,debugBelch( "Is big-endian\n" ));
   } else {
       errorBelch("%s: unknown endiannness", oc->fileName);
       return 0;
   }

   if (ehdr->e_type != ET_REL) {
      errorBelch("%s: not a relocatable object (.o) file", oc->fileName);
      return 0;
   }
   IF_DEBUG(linker, debugBelch( "Is a relocatable object (.o) file\n" ));

   IF_DEBUG(linker,debugBelch( "Architecture is " ));
   switch (ehdr->e_machine) {
#ifdef EM_ARM
      case EM_ARM:   IF_DEBUG(linker,debugBelch( "arm" )); break;
#endif
      case EM_386:   IF_DEBUG(linker,debugBelch( "x86" )); break;
#ifdef EM_SPARC32PLUS
      case EM_SPARC32PLUS:
#endif
      case EM_SPARC: IF_DEBUG(linker,debugBelch( "sparc" )); break;
#ifdef EM_IA_64
      case EM_IA_64: IF_DEBUG(linker,debugBelch( "ia64" )); break;
#endif
      case EM_PPC:   IF_DEBUG(linker,debugBelch( "powerpc32" )); break;
#ifdef EM_X86_64
      case EM_X86_64: IF_DEBUG(linker,debugBelch( "x86_64" )); break;
#elif defined(EM_AMD64)
      case EM_AMD64: IF_DEBUG(linker,debugBelch( "amd64" )); break;
#endif
      default:       IF_DEBUG(linker,debugBelch( "unknown" ));
                     errorBelch("%s: unknown architecture (e_machine == %d)"
                                , oc->fileName, ehdr->e_machine);
                     return 0;
   }

   shnum = elf_shnum(ehdr);
   IF_DEBUG(linker,debugBelch(
             "\nSection header table: start %ld, n_entries %d, ent_size %d\n",
             (long)ehdr->e_shoff, shnum, ehdr->e_shentsize  ));

   ASSERT(ehdr->e_shentsize == sizeof(Elf_Shdr));

   shdr = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);

   shstrndx = elf_shstrndx(ehdr);
   if (shstrndx == SHN_UNDEF) {
      errorBelch("%s: no section header string table", oc->fileName);
      return 0;
   } else {
      IF_DEBUG(linker,debugBelch( "Section header string table is section %d\n",
                          shstrndx));
      sh_strtab = ehdrC + shdr[shstrndx].sh_offset;
   }

   for (i = 0; i < shnum; i++) {
      IF_DEBUG(linker,debugBelch("%2d:  ", i ));
      IF_DEBUG(linker,debugBelch("type=%2d  ", (int)shdr[i].sh_type ));
      IF_DEBUG(linker,debugBelch("size=%4d  ", (int)shdr[i].sh_size ));
      IF_DEBUG(linker,debugBelch("offs=%4d  ", (int)shdr[i].sh_offset ));
      IF_DEBUG(linker,debugBelch("  (%p .. %p)  ",
               ehdrC + shdr[i].sh_offset,
                      ehdrC + shdr[i].sh_offset + shdr[i].sh_size - 1));

#define SECTION_INDEX_VALID(ndx) (ndx > SHN_UNDEF && ndx < shnum)

      switch (shdr[i].sh_type) {

        case SHT_REL:
        case SHT_RELA:
          IF_DEBUG(linker,debugBelch( shdr[i].sh_type == SHT_REL ? "Rel  " : "RelA "));

          if (!SECTION_INDEX_VALID(shdr[i].sh_link)) {
            if (shdr[i].sh_link == SHN_UNDEF)
              errorBelch("\n%s: relocation section #%d has no symbol table\n"
                         "This object file has probably been fully striped. "
                         "Such files cannot be linked.\n",
                         oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);
            else
              errorBelch("\n%s: relocation section #%d has an invalid link field (%d)\n",
                         oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                         i, shdr[i].sh_link);
            return 0;
          }
          if (shdr[shdr[i].sh_link].sh_type != SHT_SYMTAB) {
            errorBelch("\n%s: relocation section #%d does not link to a symbol table\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);
            return 0;
          }
          if (!SECTION_INDEX_VALID(shdr[i].sh_info)) {
            errorBelch("\n%s: relocation section #%d has an invalid info field (%d)\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                       i, shdr[i].sh_info);
            return 0;
          }

          break;
        case SHT_SYMTAB:
          IF_DEBUG(linker,debugBelch("Sym  "));

          if (!SECTION_INDEX_VALID(shdr[i].sh_link)) {
            errorBelch("\n%s: symbol table section #%d has an invalid link field (%d)\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName,
                       i, shdr[i].sh_link);
            return 0;
          }
          if (shdr[shdr[i].sh_link].sh_type != SHT_STRTAB) {
            errorBelch("\n%s: symbol table section #%d does not link to a string table\n",
                       oc->archiveMemberName ? oc->archiveMemberName : oc->fileName, i);

            return 0;
          }
          break;
        case SHT_STRTAB: IF_DEBUG(linker,debugBelch("Str  ")); break;
        default:         IF_DEBUG(linker,debugBelch("     ")); break;
      }
      if (sh_strtab) {
          IF_DEBUG(linker,debugBelch("sname=%s\n", sh_strtab + shdr[i].sh_name ));
      }
   }

   IF_DEBUG(linker,debugBelch( "\nString tables\n" ));
   nstrtab = 0;
   for (i = 0; i < shnum; i++) {
      if (shdr[i].sh_type == SHT_STRTAB
          /* Ignore the section header's string table. */
          && i != shstrndx
          /* Ignore string tables named .stabstr, as they contain
             debugging info. */
          && 0 != memcmp(".stabstr", sh_strtab + shdr[i].sh_name, 8)
         ) {
         IF_DEBUG(linker,debugBelch("   section %d is a normal string table\n", i ));
         nstrtab++;
      }
   }
   if (nstrtab == 0) {
      IF_DEBUG(linker,debugBelch("   no normal string tables (potentially, but not necessarily a problem)\n"));
   }
#if defined(SHN_XINDEX)
   Elf_Word* shndxTable = get_shndx_table(ehdr);
#endif
   nsymtabs = 0;
   IF_DEBUG(linker,debugBelch( "Symbol tables\n" ));
   for (i = 0; i < shnum; i++) {
      if (shdr[i].sh_type != SHT_SYMTAB) continue;
      IF_DEBUG(linker,debugBelch( "section %d is a symbol table\n", i ));
      nsymtabs++;
      stab = (Elf_Sym*) (ehdrC + shdr[i].sh_offset);
      nent = shdr[i].sh_size / sizeof(Elf_Sym);
      IF_DEBUG(linker,debugBelch( "   number of entries is apparently %d (%ld rem)\n",
               nent,
               (long)shdr[i].sh_size % sizeof(Elf_Sym)
             ));
      if (0 != shdr[i].sh_size % sizeof(Elf_Sym)) {
         errorBelch("%s: non-integral number of symbol table entries", oc->fileName);
         return 0;
      }
      for (j = 0; j < nent; j++) {
         Elf_Word secno = stab[j].st_shndx;
#if defined(SHN_XINDEX)
         /* See Note [Many ELF Sections] */
         if (secno == SHN_XINDEX) {
            ASSERT(shndxTable);
            secno = shndxTable[j];
         }
#endif
         IF_DEBUG(linker,debugBelch("   %2d  ", j ));
         IF_DEBUG(linker,debugBelch("  sec=%-5d  size=%-3d  val=%5p  ",
                             (int)secno,
                             (int)stab[j].st_size,
                             (char*)stab[j].st_value ));

         IF_DEBUG(linker,debugBelch("type=" ));
         switch (ELF_ST_TYPE(stab[j].st_info)) {
            case STT_NOTYPE:  IF_DEBUG(linker,debugBelch("notype " )); break;
            case STT_OBJECT:  IF_DEBUG(linker,debugBelch("object " )); break;
            case STT_FUNC  :  IF_DEBUG(linker,debugBelch("func   " )); break;
            case STT_SECTION: IF_DEBUG(linker,debugBelch("section" )); break;
            case STT_FILE:    IF_DEBUG(linker,debugBelch("file   " )); break;
            default:          IF_DEBUG(linker,debugBelch("?      " )); break;
         }
         IF_DEBUG(linker,debugBelch("  " ));

         IF_DEBUG(linker,debugBelch("bind=" ));
         switch (ELF_ST_BIND(stab[j].st_info)) {
            case STB_LOCAL :  IF_DEBUG(linker,debugBelch("local " )); break;
            case STB_GLOBAL:  IF_DEBUG(linker,debugBelch("global" )); break;
            case STB_WEAK  :  IF_DEBUG(linker,debugBelch("weak  " )); break;
            default:          IF_DEBUG(linker,debugBelch("?     " )); break;
         }
         IF_DEBUG(linker,debugBelch("  " ));

         IF_DEBUG(linker,debugBelch("other=%2x ", stab[j].st_other ));
         IF_DEBUG(linker,debugBelch("name=%s [%x]\n",
                        ehdrC + shdr[shdr[i].sh_link].sh_offset
                              + stab[j].st_name, stab[j].st_name ));
      }
   }

   if (nsymtabs == 0) {
     // Not having a symbol table is not in principle a problem.
     // When an object file has no symbols then the 'strip' program
     // typically will remove the symbol table entirely.
     IF_DEBUG(linker,debugBelch("   no symbol tables (potentially, but not necessarily a problem)\n"));
   }

   return 1;
}

/* Figure out what kind of section it is.  Logic derived from
   Figure 1.14 ("Special Sections") of the ELF document
   ("Portable Formats Specification, Version 1.1"). */
static int getSectionKind_ELF( Elf_Shdr *hdr, int *is_bss )
{
    *is_bss = FALSE;

    if (hdr->sh_type == SHT_PROGBITS
        && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_EXECINSTR)) {
        /* .text-style section */
        return SECTIONKIND_CODE_OR_RODATA;
    }

    if (hdr->sh_type == SHT_PROGBITS
            && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_WRITE)) {
            /* .data-style section */
            return SECTIONKIND_RWDATA;
    }

    if (hdr->sh_type == SHT_PROGBITS
        && (hdr->sh_flags & SHF_ALLOC) && !(hdr->sh_flags & SHF_WRITE)) {
        /* .rodata-style section */
        return SECTIONKIND_CODE_OR_RODATA;
    }
#ifndef openbsd_HOST_OS
    if (hdr->sh_type == SHT_INIT_ARRAY
        && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_WRITE)) {
       /* .init_array section */
        return SECTIONKIND_INIT_ARRAY;
    }
#endif /* not OpenBSD */
    if (hdr->sh_type == SHT_NOBITS
        && (hdr->sh_flags & SHF_ALLOC) && (hdr->sh_flags & SHF_WRITE)) {
        /* .bss-style section */
        *is_bss = TRUE;
        return SECTIONKIND_RWDATA;
    }

    return SECTIONKIND_OTHER;
}

static void *
mapObjectFileSection (int fd, Elf_Word offset, Elf_Word size,
                      void **mapped_start, StgWord *mapped_size,
                      StgWord *mapped_offset)
{
    void *p;
    size_t pageOffset, pageSize;

    pageOffset = roundDownToPage(offset);
    pageSize = roundUpToPage(offset-pageOffset+size);
    p = mmapForLinker(pageSize, 0, fd, pageOffset);
    if (p == NULL) return NULL;
    *mapped_size = pageSize;
    *mapped_offset = pageOffset;
    *mapped_start = p;
    return (void*)((StgWord)p + offset - pageOffset);
}

static int
ocGetNames_ELF ( ObjectCode* oc )
{
   Elf_Word i;
   int j, nent, result, fd = -1;
   Elf_Sym* stab;

   char*     ehdrC    = (char*)(oc->image);
   Elf_Ehdr* ehdr     = (Elf_Ehdr*)ehdrC;
   char*     strtab;
   Elf_Shdr* shdr     = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   Section * sections;
#if defined(SHN_XINDEX)
   Elf_Word* shndxTable = get_shndx_table(ehdr);
#endif
   const Elf_Word shnum = elf_shnum(ehdr);

   ASSERT(symhash != NULL);

   sections = (Section*)stgCallocBytes(sizeof(Section), shnum,
                                       "ocGetNames_ELF(sections)");
   oc->sections = sections;
   oc->n_sections = shnum;


   if (oc->imageMapped) {
#if defined(openbsd_HOST_OS)
       fd = open(oc->fileName, O_RDONLY, S_IRUSR);
#else
       fd = open(oc->fileName, O_RDONLY);
#endif
       if (fd == -1) {
           errorBelch("loadObj: can't open %" PATH_FMT, oc->fileName);
           return 0;
       }
   }

   for (i = 0; i < shnum; i++) {
      int         is_bss = FALSE;
      SectionKind kind   = getSectionKind_ELF(&shdr[i], &is_bss);
      SectionAlloc alloc = SECTION_NOMEM;
      void *start = NULL, *mapped_start = NULL;
      StgWord mapped_size = 0, mapped_offset = 0;
      StgWord size = shdr[i].sh_size;
      StgWord offset = shdr[i].sh_offset;

      if (is_bss && size > 0) {
         /* This is a non-empty .bss section.  Allocate zeroed space for
            it, and set its .sh_offset field such that
            ehdrC + .sh_offset == addr_of_zeroed_space.  */
          alloc = SECTION_MALLOC;
          start = stgCallocBytes(1, size, "ocGetNames_ELF(BSS)");
          mapped_start = start;
         /*
         debugBelch("BSS section at 0x%x, size %d\n",
                         zspace, shdr[i].sh_size);
         */
      }

      else if (kind != SECTIONKIND_OTHER && size > 0) {
          if (USE_CONTIGUOUS_MMAP) {
              // already mapped.
              start = oc->image + offset;
              alloc = SECTION_NOMEM;
          }
          // use the m32 allocator if either the image is not mapped
          // (i.e. we cannot map the secions separately), or if the section
          // size is small.
          else if (!oc->imageMapped || size < getPageSize() / 3) {
              start = m32_alloc(size, 8);
              if (start == NULL) goto fail;
              memcpy(start, oc->image + offset, size);
              alloc = SECTION_M32;
          } else {
              start = mapObjectFileSection(fd, offset, size,
                                           &mapped_start, &mapped_size,
                                           &mapped_offset);
              if (start == NULL) goto fail;
              alloc = SECTION_MMAP;
          }
          addProddableBlock(oc, start, size);
      }

      addSection(&sections[i], kind, alloc, start, size,
                 mapped_offset, mapped_start, mapped_size);

      if (shdr[i].sh_type != SHT_SYMTAB) continue;

      /* copy stuff into this module's object symbol table */
      stab = (Elf_Sym*) (ehdrC + offset);
      strtab = ehdrC + shdr[shdr[i].sh_link].sh_offset;
      nent = shdr[i].sh_size / sizeof(Elf_Sym);

      oc->n_symbols = nent;
      oc->symbols = stgCallocBytes(oc->n_symbols, sizeof(SymbolName*),
                                   "ocGetNames_ELF(oc->symbols)");
      // Note calloc: if we fail partway through initializing symbols, we need
      // to undo the additions to the symbol table so far. We know which ones
      // have been added by whether the entry is NULL or not.

      //TODO: we ignore local symbols anyway right? So we can use the
      //      shdr[i].sh_info to get the index of the first non-local symbol
      // ie we should use j = shdr[i].sh_info
      for (j = 0; j < nent; j++) {

         char  isLocal  = FALSE; /* avoids uninit-var warning */
         HsBool isWeak  = HS_BOOL_FALSE;
         SymbolAddr* ad  = NULL;
         SymbolName* nm  = strtab + stab[j].st_name;
         unsigned short shndx = stab[j].st_shndx;
         Elf_Word secno;

         /* See Note [Many ELF Sections] */
         /* Note that future checks for special SHN_* numbers should check the
          * shndx variable, not the section number in secno. Sections with the
          * real number in the SHN_LORESERVE..HIRESERVE range will have shndx
          * SHN_XINDEX and a secno with one of the reserved values. */
         secno = shndx;
#if defined(SHN_XINDEX)
         if (shndx == SHN_XINDEX) {
            ASSERT(shndxTable);
            secno = shndxTable[j];
         }
#endif
         /* Figure out if we want to add it; if so, set ad to its
            address.  Otherwise leave ad == NULL. */

         if (shndx == SHN_COMMON) {
            isLocal = FALSE;
            ad = stgCallocBytes(1, stab[j].st_size, "ocGetNames_ELF(COMMON)");
            /*
            debugBelch("COMMON symbol, size %d name %s\n",
                            stab[j].st_size, nm);
            */
            /* Pointless to do addProddableBlock() for this area,
               since the linker should never poke around in it. */
         }
         else
         if ( ( ELF_ST_BIND(stab[j].st_info)==STB_GLOBAL
                || ELF_ST_BIND(stab[j].st_info)==STB_LOCAL
                || ELF_ST_BIND(stab[j].st_info)==STB_WEAK
              )
              /* and not an undefined symbol */
              && shndx != SHN_UNDEF
              /* and not in a "special section" */
              && (shndx < SHN_LORESERVE
#if defined(SHN_XINDEX)
                  || shndx == SHN_XINDEX
#endif
                 )
              &&
              /* and it's a not a section or string table or anything silly */
              ( ELF_ST_TYPE(stab[j].st_info)==STT_FUNC ||
                ELF_ST_TYPE(stab[j].st_info)==STT_OBJECT ||
                ELF_ST_TYPE(stab[j].st_info)==STT_NOTYPE
              )
            ) {
            /* Section 0 is the undefined section, hence > and not >=. */
            ASSERT(secno > 0 && secno < shnum);
            /*
            if (shdr[secno].sh_type == SHT_NOBITS) {
               debugBelch("   BSS symbol, size %d off %d name %s\n",
                               stab[j].st_size, stab[j].st_value, nm);
            }
            */
            ad = (SymbolAddr*)((intptr_t)sections[secno].start +
                         (intptr_t)stab[j].st_value);
            if (ELF_ST_BIND(stab[j].st_info)==STB_LOCAL) {
               isLocal = TRUE;
               isWeak = FALSE;
            } else { /* STB_GLOBAL or STB_WEAK */
#ifdef ELF_FUNCTION_DESC
               /* dlsym() and the initialisation table both give us function
                * descriptors, so to be consistent we store function descriptors
                * in the symbol table */
               if (ELF_ST_TYPE(stab[j].st_info) == STT_FUNC)
                   ad = (SymbolAddr*)allocateFunctionDesc((Elf_Addr)ad);
#endif
               IF_DEBUG(linker,debugBelch( "addOTabName(GLOB): %10p  %s %s\n",
                                      ad, oc->fileName, nm ));
               isLocal = FALSE;
               isWeak = (ELF_ST_BIND(stab[j].st_info)==STB_WEAK);
            }
         }

         /* And the decision is ... */

         oc->symbols[j] = nm;

         if (ad != NULL) {
            ASSERT(nm != NULL);
            /* Acquire! */
            if (isLocal) {
                /* Ignore entirely. */
                oc->symbols[j] = NULL;
            } else {

                if (isWeak == HS_BOOL_TRUE) {
                    setWeakSymbol(oc, nm);
                }

                if (! ghciInsertSymbolTable(oc->fileName, symhash,
                                            nm, ad, isWeak, oc)) {
                    goto fail;
                }
            }
         } else {
            /* Skip. */
            IF_DEBUG(linker,debugBelch( "skipping `%s'\n",
                                   nm ));

            /* We're skipping the symbol, but if we ever load this
               object file we'll want to skip it then too. */
            oc->symbols[j] = NULL;

            /*
            debugBelch(
                    "skipping   bind = %d,  type = %d,  secno = %d   `%s'\n",
                    (int)ELF_ST_BIND(stab[j].st_info),
                    (int)ELF_ST_TYPE(stab[j].st_info),
                    (int)secno,
                    nm
                   );
            */
         }

      }
   }

   result = 1;
   goto end;

fail:
   result = 0;
   goto end;

end:
   if (fd >= 0) close(fd);
   return result;
}

#ifdef arm_HOST_ARCH
// TODO: These likely belong in a library somewhere

// Signed extend a number to a 32-bit int.
static inline StgInt32 sign_extend32(uint32_t bits, StgWord32 x) {
    return ((StgInt32) (x << (32 - bits))) >> (32 - bits);
}

// Does the given signed integer fit into the given bit width?
static inline StgBool is_int(uint32_t bits, StgInt32 x) {
    return bits > 32 || (-(1 << (bits-1)) <= x
                         && x < (1 << (bits-1)));
}
#endif

/* Do ELF relocations which lack an explicit addend.  All x86-linux
   and arm-linux relocations appear to be of this form. */
static int
do_Elf_Rel_relocations ( ObjectCode* oc, char* ehdrC,
                         Elf_Shdr* shdr, int shnum )
{
   int j;
   SymbolName* symbol;
   Elf_Word* targ;
   Elf_Rel*  rtab = (Elf_Rel*) (ehdrC + shdr[shnum].sh_offset);
   Elf_Sym*  stab;
   char*     strtab;
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rel);
   int target_shndx = shdr[shnum].sh_info;
   int symtab_shndx = shdr[shnum].sh_link;
   int strtab_shndx = shdr[symtab_shndx].sh_link;
#if defined(SHN_XINDEX)
   Elf_Word* shndx_table = get_shndx_table((Elf_Ehdr*)ehdrC);
#endif

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   strtab= (char*)    (ehdrC + shdr[ strtab_shndx ].sh_offset);
   targ  = (Elf_Word*)oc->sections[target_shndx].start;
   IF_DEBUG(linker,debugBelch( "relocations for section %d using symtab %d and strtab %d\n",
                          target_shndx, symtab_shndx, strtab_shndx ));

   /* Skip sections that we're not interested in. */
   if (oc->sections[target_shndx].kind == SECTIONKIND_OTHER) {
           IF_DEBUG(linker,debugBelch( "skipping (target section not loaded)"));
           return 1;
   }

   for (j = 0; j < nent; j++) {
      Elf_Addr offset = rtab[j].r_offset;
      Elf_Addr info   = rtab[j].r_info;

      Elf_Addr  P  = ((Elf_Addr)targ) + offset;
      Elf_Word* pP = (Elf_Word*)P;
#if defined(i386_HOST_ARCH) || defined(DEBUG)
      Elf_Addr  A  = *pP;
#endif
      Elf_Addr  S;
      void*     S_tmp;
#ifdef i386_HOST_ARCH
      Elf_Addr  value;
#endif
#ifdef arm_HOST_ARCH
      int is_target_thm=0, T=0;
#endif

      IF_DEBUG(linker,debugBelch( "Rel entry %3d is raw(%6p %6p): ",
                             j, (void*)offset, (void*)info ));
      if (!info) {
         IF_DEBUG(linker,debugBelch( " ZERO" ));
         S = 0;
      } else {
         Elf_Sym sym = stab[ELF_R_SYM(info)];
         /* First see if it is a local symbol. */
         if (ELF_ST_BIND(sym.st_info) == STB_LOCAL) {
            /* Yes, so we can get the address directly from the ELF symbol
               table. */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            /* See Note [Many ELF Sections] */
            Elf_Word secno = sym.st_shndx;
#if defined(SHN_XINDEX)
            if (secno == SHN_XINDEX) {
               ASSERT(shndx_table);
               secno = shndx_table[ELF_R_SYM(info)];
            }
#endif
            S = (Elf_Addr)oc->sections[ secno ].start +
                stab[ELF_R_SYM(info)].st_value;
         } else {
            symbol = strtab + sym.st_name;
            S_tmp = lookupSymbol_( symbol );
            S = (Elf_Addr)S_tmp;
         }
         if (!S) {
            errorBelch("%s: unknown symbol `%s'", oc->fileName, symbol);
            return 0;
         }
         IF_DEBUG(linker,debugBelch( "`%s' resolves to %p\n", symbol, (void*)S ));

#ifdef arm_HOST_ARCH
         // Thumb instructions have bit 0 of symbol's st_value set
         is_target_thm = S & 0x1;

         T = sym.st_info & STT_FUNC && is_target_thm;

         // Make sure we clear bit 0. Strictly speaking we should have done
         // this to st_value above but I believe alignment requirements should
         // ensure that no instructions start on an odd address
         S &= ~1;
#endif
      }

      int reloc_type = ELF_R_TYPE(info);
      IF_DEBUG(linker,debugBelch( "Reloc: P = %p   S = %p   A = %p   type=%d\n",
                             (void*)P, (void*)S, (void*)A, reloc_type ));
      checkProddableBlock ( oc, pP, sizeof(Elf_Word) );

#ifdef i386_HOST_ARCH
      value = S + A;
#endif

      switch (reloc_type) {
#        ifdef i386_HOST_ARCH
         case R_386_32:   *pP = value;     break;
         case R_386_PC32: *pP = value - P; break;
#        endif

#        ifdef arm_HOST_ARCH
         case R_ARM_ABS32:
         case R_ARM_TARGET1:  // Specified by Linux ARM ABI to be equivalent to ABS32
            *(Elf32_Word *)P += S;
            *(Elf32_Word *)P |= T;
            break;

         case R_ARM_REL32:
            *(Elf32_Word *)P += S;
            *(Elf32_Word *)P |= T;
            *(Elf32_Word *)P -= P;
            break;

         case R_ARM_CALL:
         case R_ARM_JUMP24:
         {
            // N.B. LLVM's LLD linker's relocation implement is a fantastic
            // resource
            StgWord32 *word = (StgWord32 *)P;
            StgInt32 imm = (*word & ((1<<24)-1)) << 2;

            const StgBool is_blx = (*word & 0xf0000000) == 0xf0000000;
            const StgWord32 hBit = is_blx ? ((*word >> 24) & 1) : 0;
            imm |= hBit << 1;

            // Sign extend to 32 bits
            // I would have thought this would be 24 bits but LLD uses 26 here.
            // Hmm.
            imm = sign_extend32(26, imm);

            StgWord32 result = ((S + imm) | T) - P;

            const StgBool overflow = !is_int(26, (StgInt32) result);

            // Handle overflow and Thumb interworking
            const StgBool needs_veneer = (is_target_thm && ELF_R_TYPE(info) == R_ARM_JUMP24) || overflow;
            if (needs_veneer) {
               // Generate veneer
               // The +8 below is to undo the PC-bias compensation done by the object producer
               SymbolExtra *extra = makeArmSymbolExtra(oc, ELF_R_SYM(info), S+imm+8, 0, is_target_thm);
               // The -8 below is to compensate for PC bias
               result = (StgWord32) ((StgInt32) extra->jumpIsland - P - 8);
               result &= ~1; // Clear thumb indicator bit
               if (!is_int(26, (StgInt32) result)) {
                  errorBelch("Unable to fixup overflow'd R_ARM_CALL: jump island=%p, reloc=%p\n",
                             (void*) extra->jumpIsland, (void*) P);
                  return 0;
               }
            }

            // Update the branch target
            const StgWord32 imm24 = (result & 0x03fffffc) >> 2;
            *word = (*word & ~0x00ffffff)
                  | (imm24 & 0x00ffffff);

            // Change the relocated branch into a BLX if necessary
            const StgBool switch_mode = is_target_thm && (reloc_type == R_ARM_CALL);
            if (!needs_veneer && switch_mode) {
               const StgWord32 hBit = (result & 0x2) >> 1;
               // Change instruction to BLX
               *word = (*word & ~0xFF000000) | ((0xfa | hBit) << 24);
               IF_DEBUG(linker, debugBelch("Changed BL to BLX at %p\n", word));
            }
            break;
         }

         case R_ARM_MOVT_ABS:
         case R_ARM_MOVW_ABS_NC:
         {
            StgWord32 *word = (StgWord32 *)P;
            StgWord32 imm12 = *word & 0xfff;
            StgWord32 imm4 = (*word >> 16) & 0xf;
            StgInt32 offset = imm4 << 12 | imm12;
            StgWord32 result = (S + offset) | T;

            if (reloc_type == R_ARM_MOVT_ABS)
                result = (result & 0xffff0000) >> 16;

            StgWord32 result12 = result & 0xfff;
            StgWord32 result4 = (result >> 12) & 0xf;
            *word = (*word & ~0xf0fff) | (result4 << 16) | result12;
            break;
         }

         case R_ARM_THM_CALL:
         case R_ARM_THM_JUMP24:
         {
            StgWord16 *upper = (StgWord16 *)P;
            StgWord16 *lower = (StgWord16 *)(P + 2);

            int overflow;
            int to_thm = (*lower >> 12) & 1;
            int sign = (*upper >> 10) & 1;
            int j1, j2, i1, i2;

            // Decode immediate value
            j1 = (*lower >> 13) & 1; i1 = ~(j1 ^ sign) & 1;
            j2 = (*lower >> 11) & 1; i2 = ~(j2 ^ sign) & 1;
            StgInt32 imm = (sign << 24)
                         | (i1 << 23)
                         | (i2 << 22)
                         | ((*upper & 0x03ff) << 12)
                         | ((*lower & 0x07ff) << 1);

            // Sign extend 25 to 32 bits
            if (imm & 0x01000000)
               imm -= 0x02000000;

            offset = ((imm + S) | T) - P;
            overflow = offset <= (StgWord32)0xff000000 || offset >= (StgWord32)0x01000000;

            if ((!is_target_thm && ELF_R_TYPE(info) == R_ARM_THM_JUMP24) || overflow) {
               // Generate veneer
               SymbolExtra *extra = makeArmSymbolExtra(oc, ELF_R_SYM(info), S+imm+4, 1, is_target_thm);
               offset = (StgWord32) &extra->jumpIsland - P - 4;
               sign = offset >> 31;
               to_thm = 1;
            } else if (!is_target_thm && ELF_R_TYPE(info) == R_ARM_THM_CALL) {
               offset &= ~0x3;
               to_thm = 0;
            }

            // Reencode instruction
            i1 = ~(offset >> 23) & 1; j1 = sign ^ i1;
            i2 = ~(offset >> 22) & 1; j2 = sign ^ i2;
            *upper = ( (*upper & 0xf800)
                   | (sign << 10)
                   | ((offset >> 12) & 0x03ff) );
            *lower = ( (*lower & 0xd000)
                   | (j1 << 13)
                   | (to_thm << 12)
                   | (j2 << 11)
                   | ((offset >> 1) & 0x07ff) );
            break;
         }

         case R_ARM_THM_MOVT_ABS:
         case R_ARM_THM_MOVW_ABS_NC:
         {
            StgWord16 *upper = (StgWord16 *)P;
            StgWord16 *lower = (StgWord16 *)(P + 2);
            StgInt32 offset = ((*upper & 0x000f) << 12)
                            | ((*upper & 0x0400) << 1)
                            | ((*lower & 0x7000) >> 4)
                            | (*lower & 0x00ff);

            offset = (offset ^ 0x8000) - 0x8000; // Sign extend
            offset += S;
            if (ELF_R_TYPE(info) == R_ARM_THM_MOVW_ABS_NC)
                   offset |= T;
            else if (ELF_R_TYPE(info) == R_ARM_THM_MOVT_ABS)
                   offset >>= 16;

            *upper = ( (*upper & 0xfbf0)
                   | ((offset & 0xf000) >> 12)
                   | ((offset & 0x0800) >> 1) );
            *lower = ( (*lower & 0x8f00)
                   | ((offset & 0x0700) << 4)
                   | (offset & 0x00ff) );
            break;
         }

         case R_ARM_THM_JUMP8:
         {
            StgWord16 *word = (StgWord16 *)P;
            StgWord offset = *word & 0x01fe;
            offset += S - P;
            if (!is_target_thm) {
               errorBelch("%s: Thumb to ARM transition with JUMP8 relocation not supported\n",
                     oc->fileName);
               return 0;
            }

            *word = (*word & ~0x01fe)
                  | (offset & 0x01fe);
            break;
         }

         case R_ARM_THM_JUMP11:
         {
            StgWord16 *word = (StgWord16 *)P;
            StgWord offset = *word & 0x0ffe;
            offset += S - P;
            if (!is_target_thm) {
               errorBelch("%s: Thumb to ARM transition with JUMP11 relocation not supported\n",
                     oc->fileName);
               return 0;
            }

            *word = (*word & ~0x0ffe)
                  | (offset & 0x0ffe);
            break;
         }

#        endif // arm_HOST_ARCH

         default:
            errorBelch("%s: unhandled ELF relocation(Rel) type %" FMT_Word "\n",
                  oc->fileName, (W_)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

/* Do ELF relocations for which explicit addends are supplied.
   sparc-solaris relocations appear to be of this form. */
static int
do_Elf_Rela_relocations ( ObjectCode* oc, char* ehdrC,
                          Elf_Shdr* shdr, int shnum )
{
   int j;
   SymbolName* symbol = NULL;
   Elf_Rela* rtab = (Elf_Rela*) (ehdrC + shdr[shnum].sh_offset);
   Elf_Sym*  stab;
   char*     strtab;
   int         nent = shdr[shnum].sh_size / sizeof(Elf_Rela);
   int symtab_shndx = shdr[shnum].sh_link;
   int strtab_shndx = shdr[symtab_shndx].sh_link;
   int target_shndx = shdr[shnum].sh_info;
#if defined(SHN_XINDEX)
   Elf_Word* shndx_table = get_shndx_table((Elf_Ehdr*)ehdrC);
#endif
#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
   /* This #ifdef only serves to avoid unused-var warnings. */
   Elf_Addr targ = (Elf_Addr) oc->sections[target_shndx].start;
#endif

   stab  = (Elf_Sym*) (ehdrC + shdr[ symtab_shndx ].sh_offset);
   strtab= (char*)    (ehdrC + shdr[ strtab_shndx ].sh_offset);

   IF_DEBUG(linker,debugBelch( "relocations for section %d using symtab %d\n",
                          target_shndx, symtab_shndx ));

   /* Skip sections that we're not interested in. */
   if (oc->sections[target_shndx].kind == SECTIONKIND_OTHER) {
           IF_DEBUG(linker,debugBelch( "skipping (target section not loaded)"));
           return 1;
   }

   for (j = 0; j < nent; j++) {
#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
      /* This #ifdef only serves to avoid unused-var warnings. */
      Elf_Addr  offset = rtab[j].r_offset;
      Elf_Addr  P      = targ + offset;
      Elf_Addr  A      = rtab[j].r_addend;
#endif
#if defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
      Elf_Addr  value;
#endif
      Elf_Addr  info   = rtab[j].r_info;
      Elf_Addr  S;
      void*     S_tmp;
#     if defined(sparc_HOST_ARCH)
      Elf_Word* pP = (Elf_Word*)P;
      Elf_Word  w1, w2;
#     elif defined(powerpc_HOST_ARCH)
      Elf_Sword delta;
#     endif

      IF_DEBUG(linker,debugBelch( "Rel entry %3d is raw(%6p %6p %6p)   ",
                             j, (void*)offset, (void*)info,
                                (void*)A ));
      if (!info) {
         IF_DEBUG(linker,debugBelch( " ZERO" ));
         S = 0;
      } else {
         Elf_Sym sym = stab[ELF_R_SYM(info)];
         /* First see if it is a local symbol. */
         if (ELF_ST_BIND(sym.st_info) == STB_LOCAL) {
            /* Yes, so we can get the address directly from the ELF symbol
               table. */
            symbol = sym.st_name==0 ? "(noname)" : strtab+sym.st_name;
            /* See Note [Many ELF Sections] */
            Elf_Word secno = sym.st_shndx;
#if defined(SHN_XINDEX)
            if (secno == SHN_XINDEX) {
              secno = shndx_table[ELF_R_SYM(info)];
            }
#endif
            S = (Elf_Addr)oc->sections[secno].start
                + stab[ELF_R_SYM(info)].st_value;
#ifdef ELF_FUNCTION_DESC
            /* Make a function descriptor for this function */
            if (S && ELF_ST_TYPE(sym.st_info) == STT_FUNC) {
               S = allocateFunctionDesc(S + A);
               A = 0;
            }
#endif
         } else {
            /* No, so look up the name in our global table. */
            symbol = strtab + sym.st_name;
            S_tmp = lookupSymbol_( symbol );
            S = (Elf_Addr)S_tmp;

#ifdef ELF_FUNCTION_DESC
            /* If a function, already a function descriptor - we would
               have to copy it to add an offset. */
            if (S && (ELF_ST_TYPE(sym.st_info) == STT_FUNC) && (A != 0))
               errorBelch("%s: function %s with addend %p", oc->fileName, symbol, (void *)A);
#endif
         }
         if (!S) {
           errorBelch("%s: unknown symbol `%s'", oc->fileName, symbol);
           return 0;
         }
         IF_DEBUG(linker,debugBelch( "`%s' resolves to %p\n", symbol, (void*)S ));
      }

#if defined(DEBUG) || defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) \
    || defined(x86_64_HOST_ARCH)
      IF_DEBUG(linker,debugBelch("Reloc: P = %p   S = %p   A = %p\n",
                                        (void*)P, (void*)S, (void*)A ));
      checkProddableBlock(oc, (void*)P, sizeof(Elf_Word));
#endif

#if defined(sparc_HOST_ARCH) || defined(powerpc_HOST_ARCH) || defined(x86_64_HOST_ARCH)
      value = S + A;
#endif

      switch (ELF_R_TYPE(info)) {
#        if defined(sparc_HOST_ARCH)
         case R_SPARC_WDISP30:
            w1 = *pP & 0xC0000000;
            w2 = (Elf_Word)((value - P) >> 2);
            ASSERT((w2 & 0xC0000000) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         case R_SPARC_HI22:
            w1 = *pP & 0xFFC00000;
            w2 = (Elf_Word)(value >> 10);
            ASSERT((w2 & 0xFFC00000) == 0);
            w1 |= w2;
            *pP = w1;
            break;
         case R_SPARC_LO10:
            w1 = *pP & ~0x3FF;
            w2 = (Elf_Word)(value & 0x3FF);
            ASSERT((w2 & ~0x3FF) == 0);
            w1 |= w2;
            *pP = w1;
            break;

         /* According to the Sun documentation:
            R_SPARC_UA32
            This relocation type resembles R_SPARC_32, except it refers to an
            unaligned word. That is, the word to be relocated must be treated
            as four separate bytes with arbitrary alignment, not as a word
            aligned according to the architecture requirements.
         */
         case R_SPARC_UA32:
            w2  = (Elf_Word)value;

            // SPARC doesn't do misaligned writes of 32 bit words,
            //       so we have to do this one byte-at-a-time.
            char *pPc   = (char*)pP;
            pPc[0]      = (char) ((Elf_Word)(w2 & 0xff000000) >> 24);
            pPc[1]      = (char) ((Elf_Word)(w2 & 0x00ff0000) >> 16);
            pPc[2]      = (char) ((Elf_Word)(w2 & 0x0000ff00) >> 8);
            pPc[3]      = (char) ((Elf_Word)(w2 & 0x000000ff));
            break;

         case R_SPARC_32:
            w2 = (Elf_Word)value;
            *pP = w2;
            break;
#        elif defined(powerpc_HOST_ARCH)
         case R_PPC_ADDR16_LO:
            *(Elf32_Half*) P = value;
            break;

         case R_PPC_ADDR16_HI:
            *(Elf32_Half*) P = value >> 16;
            break;

         case R_PPC_ADDR16_HA:
            *(Elf32_Half*) P = (value + 0x8000) >> 16;
            break;

         case R_PPC_ADDR32:
            *(Elf32_Word *) P = value;
            break;

         case R_PPC_REL32:
            *(Elf32_Word *) P = value - P;
            break;

         case R_PPC_PLTREL24:
            value -= 0x8000; /* See Note [.LCTOC1 in PPC PIC code] */
            /* fallthrough */
         case R_PPC_REL24:
            delta = value - P;

            if( delta << 6 >> 6 != delta )
            {
               value = (Elf_Addr) (&makeSymbolExtra( oc, ELF_R_SYM(info), value )
                                        ->jumpIsland);
               delta = value - P;

               if( value == 0 || delta << 6 >> 6 != delta )
               {
                  barf( "Unable to make SymbolExtra for #%d",
                        ELF_R_SYM(info) );
                  return 0;
               }
            }

            *(Elf_Word *) P = (*(Elf_Word *) P & 0xfc000003)
                                          | (delta & 0x3fffffc);
            break;

         case R_PPC_REL16_LO:
            *(Elf32_Half*) P = value - P;
            break;

         case R_PPC_REL16_HI:
            *(Elf32_Half*) P = (value - P) >> 16;
            break;

         case R_PPC_REL16_HA:
            *(Elf32_Half*) P = (value + 0x8000 - P) >> 16;
            break;
#        endif

#if x86_64_HOST_ARCH
      case R_X86_64_64:
          *(Elf64_Xword *)P = value;
          break;

      case R_X86_64_PC32:
      {
#if defined(ALWAYS_PIC)
          barf("R_X86_64_PC32 relocation, but ALWAYS_PIC.");
#else
          StgInt64 off = value - P;
          if (off >= 0x7fffffffL || off < -0x80000000L) {
              if (X86_64_ELF_NONPIC_HACK) {
                  StgInt64 pltAddress =
                      (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                -> jumpIsland;
                  off = pltAddress + A - P;
              } else {
                  errorBelch("R_X86_64_PC32 relocation out of range: %s = %"
                             PRId64 "d\nRecompile %s with -fPIC.",
                             symbol, off, oc->fileName );
                  return 0;
              }
          }
          *(Elf64_Word *)P = (Elf64_Word)off;
#endif
          break;
      }

      case R_X86_64_PC64:
      {
          StgInt64 off = value - P;
          *(Elf64_Word *)P = (Elf64_Word)off;
          break;
      }

      case R_X86_64_32:
#if defined(ALWAYS_PIC)
          barf("R_X86_64_32 relocation, but ALWAYS_PIC.");
#else
          if (value >= 0x7fffffffL) {
              if (X86_64_ELF_NONPIC_HACK) {
                  StgInt64 pltAddress =
                      (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                -> jumpIsland;
                  value = pltAddress + A;
              } else {
                  errorBelch("R_X86_64_32 relocation out of range: %s = %"
                         PRId64 "d\nRecompile %s with -fPIC.",
                         symbol, value, oc->fileName );
                  return 0;
              }
          }
          *(Elf64_Word *)P = (Elf64_Word)value;
#endif
          break;

      case R_X86_64_32S:
#if defined(ALWAYS_PIC)
          barf("R_X86_64_32S relocation, but ALWAYS_PIC.");
#else
          if ((StgInt64)value > 0x7fffffffL || (StgInt64)value < -0x80000000L) {
              if (X86_64_ELF_NONPIC_HACK) {
                  StgInt64 pltAddress =
                      (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                -> jumpIsland;
                  value = pltAddress + A;
              } else {
                  errorBelch("R_X86_64_32S relocation out of range: %s = %"
                         PRId64 "d\nRecompile %s with -fPIC.",
                         symbol, value, oc->fileName );
                  return 0;
              }
          }
          *(Elf64_Sword *)P = (Elf64_Sword)value;
#endif
          break;
/* These two relocations were introduced in glibc 2.23 and binutils 2.26.
    But in order to use them the system which compiles the bindist for GHC needs
    to have glibc >= 2.23. So only use them if they're defined. */
#if defined(R_X86_64_REX_GOTPCRELX) && defined(R_X86_64_GOTPCRELX)
      case R_X86_64_REX_GOTPCRELX:
      case R_X86_64_GOTPCRELX:
#endif
      case R_X86_64_GOTPCREL:
      {
          StgInt64 gotAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)->addr;
          StgInt64 off = gotAddress + A - P;
          *(Elf64_Word *)P = (Elf64_Word)off;
          break;
      }
#if defined(dragonfly_HOST_OS)
      case R_X86_64_GOTTPOFF:
      {
#if defined(ALWAYS_PIC)
          barf("R_X86_64_GOTTPOFF relocation, but ALWAYS_PIC.");
#else
        /* determine the offset of S to the current thread's tls
           area
           XXX: Move this to the beginning of function */
          struct tls_info ti;
          get_tls_area(0, &ti, sizeof(ti));
          /* make entry in GOT that contains said offset */
          StgInt64 gotEntry = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info),
                                         (S - (Elf64_Addr)(ti.base)))->addr;
          *(Elf64_Word *)P = gotEntry + A - P;
#endif
          break;
      }
#endif



      case R_X86_64_PLT32:
      {
#if defined(ALWAYS_PIC)
          barf("R_X86_64_PLT32 relocation, but ALWAYS_PIC.");
#else
          StgInt64 off = value - P;
          if (off >= 0x7fffffffL || off < -0x80000000L) {
              StgInt64 pltAddress = (StgInt64) &makeSymbolExtra(oc, ELF_R_SYM(info), S)
                                                    -> jumpIsland;
              off = pltAddress + A - P;
          }
          *(Elf64_Word *)P = (Elf64_Word)off;
#endif
          break;
      }
#endif

         default:
            errorBelch("%s: unhandled ELF relocation(RelA) type %" FMT_Word "\n",
                  oc->fileName, (W_)ELF_R_TYPE(info));
            return 0;
      }

   }
   return 1;
}

static int
ocResolve_ELF ( ObjectCode* oc )
{
   int       ok;
   Elf_Word  i;
   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*) ehdrC;
   Elf_Shdr* shdr  = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   const Elf_Word shnum = elf_shnum(ehdr);

   /* Process the relocation sections. */
   for (i = 0; i < shnum; i++) {
      if (shdr[i].sh_type == SHT_REL) {
         ok = do_Elf_Rel_relocations ( oc, ehdrC, shdr, i );
         if (!ok) return ok;
      }
      else
      if (shdr[i].sh_type == SHT_RELA) {
         ok = do_Elf_Rela_relocations ( oc, ehdrC, shdr, i );
         if (!ok) return ok;
      }
   }

#if defined(powerpc_HOST_ARCH) || defined(arm_HOST_ARCH)
   ocFlushInstructionCache( oc );
#endif

   return 1;
}

static int ocRunInit_ELF( ObjectCode *oc )
{
   Elf_Word i;
   char*     ehdrC = (char*)(oc->image);
   Elf_Ehdr* ehdr  = (Elf_Ehdr*) ehdrC;
   Elf_Shdr* shdr  = (Elf_Shdr*) (ehdrC + ehdr->e_shoff);
   char* sh_strtab = ehdrC + shdr[elf_shstrndx(ehdr)].sh_offset;
   int argc, envc;
   char **argv, **envv;

   getProgArgv(&argc, &argv);
   getProgEnvv(&envc, &envv);

   // XXX Apparently in some archs .init may be something
   // special!  See DL_DT_INIT_ADDRESS macro in glibc
   // as well as ELF_FUNCTION_PTR_IS_SPECIAL.  We've not handled
   // it here, please file a bug report if it affects you.
   for (i = 0; i < elf_shnum(ehdr); i++) {
      init_t *init_start, *init_end, *init;
      int is_bss = FALSE;
      SectionKind kind = getSectionKind_ELF(&shdr[i], &is_bss);
      if (kind == SECTIONKIND_CODE_OR_RODATA
       && 0 == memcmp(".init", sh_strtab + shdr[i].sh_name, 5)) {
          init_t init_f = (init_t)(oc->sections[i].start);
          init_f(argc, argv, envv);
      }

      if (kind == SECTIONKIND_INIT_ARRAY) {
          char *init_startC = oc->sections[i].start;
         init_start = (init_t*)init_startC;
         init_end = (init_t*)(init_startC + shdr[i].sh_size);
         for (init = init_start; init < init_end; init++) {
            (*init)(argc, argv, envv);
         }
      }

      // XXX could be more strict and assert that it's
      // SECTIONKIND_RWDATA; but allowing RODATA seems harmless enough.
      if ((kind == SECTIONKIND_RWDATA || kind == SECTIONKIND_CODE_OR_RODATA)
       && 0 == memcmp(".ctors", sh_strtab + shdr[i].sh_name, 6)) {
          char *init_startC = oc->sections[i].start;
         init_start = (init_t*)init_startC;
         init_end = (init_t*)(init_startC + shdr[i].sh_size);
         // ctors run in reverse
         for (init = init_end - 1; init >= init_start; init--) {
            (*init)(argc, argv, envv);
         }
      }
   }

   freeProgEnvv(envc, envv);
   return 1;
}

/*
 * PowerPC & X86_64 ELF specifics
 */

#if NEED_SYMBOL_EXTRAS

static int ocAllocateSymbolExtras_ELF( ObjectCode *oc )
{
  Elf_Ehdr *ehdr;
  Elf_Shdr* shdr;
  Elf_Word i, shnum;

  ehdr = (Elf_Ehdr *) oc->image;
  shdr = (Elf_Shdr *) ( ((char *)oc->image) + ehdr->e_shoff );

  shnum = elf_shnum(ehdr);

  for( i = 0; i < shnum; i++ )
    if( shdr[i].sh_type == SHT_SYMTAB )
      break;

  if( i == shnum )
  {
    // Not having a symbol table is not in principle a problem.
    // When an object file has no symbols then the 'strip' program
    // typically will remove the symbol table entirely.
    IF_DEBUG(linker, debugBelch( "The ELF file %s contains no symtab\n",
             oc->archiveMemberName ? oc->archiveMemberName : oc->fileName ));
    return 1;
  }

  if( shdr[i].sh_entsize != sizeof( Elf_Sym ) )
  {
    errorBelch( "The entry size (%d) of the symtab isn't %d\n",
      (int) shdr[i].sh_entsize, (int) sizeof( Elf_Sym ) );

    return 0;
  }

  return ocAllocateSymbolExtras( oc, shdr[i].sh_size / sizeof( Elf_Sym ), 0 );
}

#endif /* NEED_SYMBOL_EXTRAS */

#endif /* ELF */

/* --------------------------------------------------------------------------
 * Mach-O specifics
 * ------------------------------------------------------------------------*/

#if defined(OBJFORMAT_MACHO)

/*
  Support for MachO linking on Darwin/MacOS X
  by Wolfgang Thaller (wolfgang.thaller@gmx.net)

  I hereby formally apologize for the hackish nature of this code.
  Things that need to be done:
  *) implement ocVerifyImage_MachO
  *) add still more sanity checks.
*/

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
#define mach_header mach_header_64
#define segment_command segment_command_64
#define section section_64
#define nlist nlist_64
#endif

#ifdef powerpc_HOST_ARCH
static int
ocAllocateSymbolExtras_MachO(ObjectCode* oc)
{
    struct mach_header *header = (struct mach_header *) oc->image;
    struct load_command *lc = (struct load_command *) (header + 1);
    unsigned i;

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: start\n"));

    for (i = 0; i < header->ncmds; i++) {
        if (lc->cmd == LC_SYMTAB) {

                // Find out the first and last undefined external
                // symbol, so we don't have to allocate too many
            // jump islands/GOT entries.

            struct symtab_command *symLC = (struct symtab_command *) lc;
            unsigned min = symLC->nsyms, max = 0;
            struct nlist *nlist =
                symLC ? (struct nlist*) ((char*) oc->image + symLC->symoff)
                      : NULL;

            for (i = 0; i < symLC->nsyms; i++) {

                if (nlist[i].n_type & N_STAB) {
                    ;
                } else if (nlist[i].n_type & N_EXT) {

                    if((nlist[i].n_type & N_TYPE) == N_UNDF
                        && (nlist[i].n_value == 0)) {

                        if (i < min) {
                            min = i;
                        }

                        if (i > max) {
                            max = i;
                    }
                }
            }
            }

            if (max >= min) {
                return ocAllocateSymbolExtras(oc, max - min + 1, min);
            }

            break;
        }

        lc = (struct load_command *) ( ((char *)lc) + lc->cmdsize );
    }

    return ocAllocateSymbolExtras(oc,0,0);
}

#endif
#ifdef x86_64_HOST_ARCH
static int
ocAllocateSymbolExtras_MachO(ObjectCode* oc)
{
    struct mach_header *header = (struct mach_header *) oc->image;
    struct load_command *lc = (struct load_command *) (header + 1);
    unsigned i;

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: start\n"));

    for (i = 0; i < header->ncmds; i++) {
        if (lc->cmd == LC_SYMTAB) {

                // Just allocate one entry for every symbol
            struct symtab_command *symLC = (struct symtab_command *) lc;

            IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: allocate %d symbols\n", symLC->nsyms));
            IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: done\n"));
            return ocAllocateSymbolExtras(oc, symLC->nsyms, 0);
        }

        lc = (struct load_command *) ( ((char *)lc) + lc->cmdsize );
    }

    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: allocated no symbols\n"));
    IF_DEBUG(linker, debugBelch("ocAllocateSymbolExtras_MachO: done\n"));
    return ocAllocateSymbolExtras(oc,0,0);
}
#endif

static int
ocVerifyImage_MachO(ObjectCode * oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;

    IF_DEBUG(linker, debugBelch("ocVerifyImage_MachO: start\n"));

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
    if(header->magic != MH_MAGIC_64) {
        errorBelch("Could not load image %s: bad magic!\n"
                   "  Expected %08x (64bit), got %08x%s\n",
                   oc->fileName, MH_MAGIC_64, header->magic,
                   header->magic == MH_MAGIC ? " (32bit)." : ".");
        return 0;
    }
#else
    if(header->magic != MH_MAGIC) {
        errorBelch("Could not load image %s: bad magic!\n"
                   "  Expected %08x (32bit), got %08x%s\n",
                   oc->fileName, MH_MAGIC, header->magic,
                   header->magic == MH_MAGIC_64 ? " (64bit)." : ".");
        return 0;
    }
#endif

    // FIXME: do some more verifying here
    IF_DEBUG(linker, debugBelch("ocVerifyImage_MachO: done\n"));
    return 1;
}

static int
resolveImports(
    ObjectCode* oc,
    char *image,
    struct symtab_command *symLC,
    struct section *sect,    // ptr to lazy or non-lazy symbol pointer section
    unsigned long *indirectSyms,
    struct nlist *nlist)
{
    unsigned i;
    size_t itemSize = 4;

    IF_DEBUG(linker, debugBelch("resolveImports: start\n"));

#if i386_HOST_ARCH
    int isJumpTable = 0;

    if (strcmp(sect->sectname,"__jump_table") == 0) {
        isJumpTable = 1;
        itemSize = 5;
        ASSERT(sect->reserved2 == itemSize);
    }

#endif

    for(i = 0; i * itemSize < sect->size; i++)
    {
        // according to otool, reserved1 contains the first index into the indirect symbol table
        struct nlist *symbol = &nlist[indirectSyms[sect->reserved1+i]];
        SymbolName* nm = image + symLC->stroff + symbol->n_un.n_strx;
        SymbolAddr* addr = NULL;

        IF_DEBUG(linker, debugBelch("resolveImports: resolving %s\n", nm));

        if ((symbol->n_type & N_TYPE) == N_UNDF
            && (symbol->n_type & N_EXT) && (symbol->n_value != 0)) {
            addr = (SymbolAddr*) (symbol->n_value);
            IF_DEBUG(linker, debugBelch("resolveImports: undefined external %s has value %p\n", nm, addr));
        } else {
            addr = lookupSymbol_(nm);
            IF_DEBUG(linker, debugBelch("resolveImports: looking up %s, %p\n", nm, addr));
        }

        if (addr == NULL)
        {
            errorBelch("\nlookupSymbol failed in resolveImports\n"
                       "%s: unknown symbol `%s'", oc->fileName, nm);
            return 0;
        }
        ASSERT(addr);

#if i386_HOST_ARCH
        if (isJumpTable) {
            checkProddableBlock(oc,image + sect->offset + i*itemSize, 5);

            *(image + sect->offset + i * itemSize) = 0xe9; // jmp opcode
            *(unsigned*)(image + sect->offset + i*itemSize + 1)
                = (SymbolAddr*)addr - (image + sect->offset + i*itemSize + 5);
        }
        else
#endif
        {
            checkProddableBlock(oc,
                                ((void**)(image + sect->offset)) + i,
                                sizeof(void *));
            ((void**)(image + sect->offset))[i] = addr;
        }
    }

    IF_DEBUG(linker, debugBelch("resolveImports: done\n"));
    return 1;
}

static unsigned long
relocateAddress(
    ObjectCode* oc,
    int nSections,
    struct section* sections,
    unsigned long address)
{
    int i;
    IF_DEBUG(linker, debugBelch("relocateAddress: start\n"));
    for (i = 0; i < nSections; i++)
    {
            IF_DEBUG(linker, debugBelch("    relocating address in section %d\n", i));
        if (sections[i].addr <= address
            && address < sections[i].addr + sections[i].size)
        {
            return (unsigned long)oc->image
                    + sections[i].offset + address - sections[i].addr;
        }
    }
    barf("Invalid Mach-O file:"
         "Address out of bounds while relocating object file");
    return 0;
}

static int
relocateSection(
    ObjectCode* oc,
    char *image,
    struct symtab_command *symLC, struct nlist *nlist,
    int nSections, struct section* sections, struct section *sect)
{
    struct relocation_info *relocs;
    int i, n;

    IF_DEBUG(linker, debugBelch("relocateSection: start\n"));

    if(!strcmp(sect->sectname,"__la_symbol_ptr"))
        return 1;
    else if(!strcmp(sect->sectname,"__nl_symbol_ptr"))
        return 1;
    else if(!strcmp(sect->sectname,"__la_sym_ptr2"))
        return 1;
    else if(!strcmp(sect->sectname,"__la_sym_ptr3"))
        return 1;

    n = sect->nreloc;
    IF_DEBUG(linker, debugBelch("relocateSection: number of relocations: %d\n", n));

    relocs = (struct relocation_info*) (image + sect->reloff);

    for(i = 0; i < n; i++)
    {
#ifdef x86_64_HOST_ARCH
        struct relocation_info *reloc = &relocs[i];

        char    *thingPtr = image + sect->offset + reloc->r_address;
        uint64_t thing;
        /* We shouldn't need to initialise this, but gcc on OS X 64 bit
           complains that it may be used uninitialized if we don't */
        uint64_t value = 0;
        uint64_t baseValue;
        int type = reloc->r_type;

        IF_DEBUG(linker, debugBelch("relocateSection: relocation %d\n", i));
        IF_DEBUG(linker, debugBelch("               : type      = %d\n", reloc->r_type));
        IF_DEBUG(linker, debugBelch("               : address   = %d\n", reloc->r_address));
        IF_DEBUG(linker, debugBelch("               : symbolnum = %u\n", reloc->r_symbolnum));
        IF_DEBUG(linker, debugBelch("               : pcrel     = %d\n", reloc->r_pcrel));
        IF_DEBUG(linker, debugBelch("               : length    = %d\n", reloc->r_length));
        IF_DEBUG(linker, debugBelch("               : extern    = %d\n", reloc->r_extern));
        IF_DEBUG(linker, debugBelch("               : type      = %d\n", reloc->r_type));

        switch(reloc->r_length)
        {
            case 0:
                checkProddableBlock(oc,thingPtr,1);
                thing = *(uint8_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 1;
                break;
            case 1:
                checkProddableBlock(oc,thingPtr,2);
                thing = *(uint16_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 2;
                break;
            case 2:
                checkProddableBlock(oc,thingPtr,4);
                thing = *(uint32_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 4;
                break;
            case 3:
                checkProddableBlock(oc,thingPtr,8);
                thing = *(uint64_t*)thingPtr;
                baseValue = (uint64_t)thingPtr + 8;
                break;
            default:
                barf("Unknown size.");
        }

        IF_DEBUG(linker,
                 debugBelch("relocateSection: length = %d, thing = %" PRId64 ", baseValue = %p\n",
                            reloc->r_length, thing, (char *)baseValue));

        if (type == X86_64_RELOC_GOT
         || type == X86_64_RELOC_GOT_LOAD)
        {
            struct nlist *symbol = &nlist[reloc->r_symbolnum];
            SymbolName* nm = image + symLC->stroff + symbol->n_un.n_strx;
            SymbolAddr* addr = NULL;

            IF_DEBUG(linker, debugBelch("relocateSection: making jump island for %s, extern = %d, X86_64_RELOC_GOT\n", nm, reloc->r_extern));

            ASSERT(reloc->r_extern);
            if (reloc->r_extern == 0) {
                    errorBelch("\nrelocateSection: global offset table relocation for symbol with r_extern == 0\n");
            }

            if (symbol->n_type & N_EXT) {
                    // The external bit is set, meaning the symbol is exported,
                    // and therefore can be looked up in this object module's
                    // symtab, or it is undefined, meaning dlsym must be used
                    // to resolve it.

                    addr = lookupSymbol_(nm);
                    IF_DEBUG(linker, debugBelch("relocateSection: looked up %s, "
                                                "external X86_64_RELOC_GOT or X86_64_RELOC_GOT_LOAD\n", nm));
                    IF_DEBUG(linker, debugBelch("               : addr = %p\n", addr));

                    if (addr == NULL) {
                            errorBelch("\nlookupSymbol failed in relocateSection (RELOC_GOT)\n"
                                       "%s: unknown symbol `%s'", oc->fileName, nm);
                            return 0;
                    }
            } else {
                    IF_DEBUG(linker, debugBelch("relocateSection: %s is not an exported symbol\n", nm));

                    // The symbol is not exported, or defined in another
                    // module, so it must be in the current object module,
                    // at the location given by the section index and
                    // symbol address (symbol->n_value)

                    if ((symbol->n_type & N_TYPE) == N_SECT) {
                            addr = (void *)relocateAddress(oc, nSections, sections, symbol->n_value);
                            IF_DEBUG(linker, debugBelch("relocateSection: calculated relocation %p of "
                                                        "non-external X86_64_RELOC_GOT or X86_64_RELOC_GOT_LOAD\n",
                                                        (void *)symbol->n_value));
                            IF_DEBUG(linker, debugBelch("               : addr = %p\n", addr));
                    } else {
                            errorBelch("\nrelocateSection: %s is not exported,"
                                       " and should be defined in a section, but isn't!\n", nm);
                    }
            }

            value = (uint64_t) &makeSymbolExtra(oc, reloc->r_symbolnum, (unsigned long)addr)->addr;

            type = X86_64_RELOC_SIGNED;
        }
        else if (reloc->r_extern)
        {
            struct nlist *symbol = &nlist[reloc->r_symbolnum];
            SymbolName* nm = image + symLC->stroff + symbol->n_un.n_strx;
            SymbolAddr* addr = NULL;

            IF_DEBUG(linker, debugBelch("relocateSection: looking up external symbol %s\n", nm));
            IF_DEBUG(linker, debugBelch("               : type  = %d\n", symbol->n_type));
            IF_DEBUG(linker, debugBelch("               : sect  = %d\n", symbol->n_sect));
            IF_DEBUG(linker, debugBelch("               : desc  = %d\n", symbol->n_desc));
            IF_DEBUG(linker, debugBelch("               : value = %p\n", (void *)symbol->n_value));

            if ((symbol->n_type & N_TYPE) == N_SECT) {
                value = relocateAddress(oc, nSections, sections,
                                        symbol->n_value);
                IF_DEBUG(linker, debugBelch("relocateSection, defined external symbol %s, relocated address %p\n", nm, (void *)value));
            }
            else {
                addr = lookupSymbol_(nm);
                if (addr == NULL)
                {
                     errorBelch("\nlookupSymbol failed in relocateSection (relocate external)\n"
                                "%s: unknown symbol `%s'", oc->fileName, nm);
                     return 0;
                }

                value = (uint64_t) addr;
                IF_DEBUG(linker, debugBelch("relocateSection: external symbol %s, address %p\n", nm, (void *)value));
            }
        }
        else
        {
            // If the relocation is not through the global offset table
            // or external, then set the value to the baseValue.  This
            // will leave displacements into the __const section
            // unchanged (as they ought to be).

            value = baseValue;
        }

        IF_DEBUG(linker, debugBelch("relocateSection: value = %p\n", (void *)value));

        if (type == X86_64_RELOC_BRANCH)
        {
            if((int32_t)(value - baseValue) != (int64_t)(value - baseValue))
            {
                ASSERT(reloc->r_extern);
                value = (uint64_t) &makeSymbolExtra(oc, reloc->r_symbolnum, value)
                                        -> jumpIsland;
            }
            ASSERT((int32_t)(value - baseValue) == (int64_t)(value - baseValue));
            type = X86_64_RELOC_SIGNED;
        }

        switch(type)
        {
            case X86_64_RELOC_UNSIGNED:
                ASSERT(!reloc->r_pcrel);
                thing += value;
                break;
            case X86_64_RELOC_SIGNED:
            case X86_64_RELOC_SIGNED_1:
            case X86_64_RELOC_SIGNED_2:
            case X86_64_RELOC_SIGNED_4:
                ASSERT(reloc->r_pcrel);
                thing += value - baseValue;
                break;
            case X86_64_RELOC_SUBTRACTOR:
                ASSERT(!reloc->r_pcrel);
                thing -= value;
                break;
            default:
                barf("unkown relocation");
        }

        switch(reloc->r_length)
        {
            case 0:
                *(uint8_t*)thingPtr = thing;
                break;
            case 1:
                *(uint16_t*)thingPtr = thing;
                break;
            case 2:
                *(uint32_t*)thingPtr = thing;
                break;
            case 3:
                *(uint64_t*)thingPtr = thing;
                break;
        }
#else
        if(relocs[i].r_address & R_SCATTERED)
        {
            struct scattered_relocation_info *scat =
                (struct scattered_relocation_info*) &relocs[i];

            if(!scat->r_pcrel)
            {
                if(scat->r_length == 2)
                {
                    unsigned long word = 0;
                    unsigned long* wordPtr = (unsigned long*) (image + sect->offset + scat->r_address);

                    /* In this check we assume that sizeof(unsigned long) = 2 * sizeof(unsigned short)
                       on powerpc_HOST_ARCH */
                    checkProddableBlock(oc,wordPtr,sizeof(unsigned long));

                    // Note on relocation types:
                    // i386 uses the GENERIC_RELOC_* types,
                    // while ppc uses special PPC_RELOC_* types.
                    // *_RELOC_VANILLA and *_RELOC_PAIR have the same value
                    // in both cases, all others are different.
                    // Therefore, we use GENERIC_RELOC_VANILLA
                    // and GENERIC_RELOC_PAIR instead of the PPC variants,
                    // and use #ifdefs for the other types.

                    // Step 1: Figure out what the relocated value should be
                    if (scat->r_type == GENERIC_RELOC_VANILLA) {
                        word = *wordPtr
                             + (unsigned long) relocateAddress(oc,
                                                                nSections,
                                                                sections,
                                                                scat->r_value)
                                        - scat->r_value;
                    }
#ifdef powerpc_HOST_ARCH
                    else if(scat->r_type == PPC_RELOC_SECTDIFF
                        || scat->r_type == PPC_RELOC_LO16_SECTDIFF
                        || scat->r_type == PPC_RELOC_HI16_SECTDIFF
                        || scat->r_type == PPC_RELOC_HA16_SECTDIFF
                        || scat->r_type == PPC_RELOC_LOCAL_SECTDIFF)
#else
                    else if(scat->r_type == GENERIC_RELOC_SECTDIFF
                        || scat->r_type == GENERIC_RELOC_LOCAL_SECTDIFF)
#endif
                    {
                        struct scattered_relocation_info *pair =
                                (struct scattered_relocation_info*) &relocs[i+1];

                        if (!pair->r_scattered || pair->r_type != GENERIC_RELOC_PAIR) {
                            barf("Invalid Mach-O file: "
                                 "RELOC_*_SECTDIFF not followed by RELOC_PAIR");
                        }

                        word = (unsigned long)
                               (relocateAddress(oc, nSections, sections, scat->r_value)
                              - relocateAddress(oc, nSections, sections, pair->r_value));
                        i++;
                    }
#ifdef powerpc_HOST_ARCH
                    else if(scat->r_type == PPC_RELOC_HI16
                         || scat->r_type == PPC_RELOC_LO16
                         || scat->r_type == PPC_RELOC_HA16
                         || scat->r_type == PPC_RELOC_LO14)
                    {   // these are generated by label+offset things
                        struct relocation_info *pair = &relocs[i+1];

                        if ((pair->r_address & R_SCATTERED) || pair->r_type != PPC_RELOC_PAIR) {
                            barf("Invalid Mach-O file: "
                                 "PPC_RELOC_* not followed by PPC_RELOC_PAIR");
                        }

                        if(scat->r_type == PPC_RELOC_LO16)
                        {
                            word = ((unsigned short*) wordPtr)[1];
                            word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
                        }
                        else if(scat->r_type == PPC_RELOC_LO14)
                        {
                            barf("Unsupported Relocation: PPC_RELOC_LO14");
                            word = ((unsigned short*) wordPtr)[1] & 0xFFFC;
                            word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
                        }
                        else if(scat->r_type == PPC_RELOC_HI16)
                        {
                            word = ((unsigned short*) wordPtr)[1] << 16;
                            word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF);
                        }
                        else if(scat->r_type == PPC_RELOC_HA16)
                        {
                            word = ((unsigned short*) wordPtr)[1] << 16;
                            word += ((short)relocs[i+1].r_address & (short)0xFFFF);
                        }


                        word += (unsigned long) relocateAddress(oc, nSections, sections, scat->r_value)
                                                - scat->r_value;

                        i++;
                    }
 #endif
                    else {
                        barf ("Don't know how to handle this Mach-O "
                              "scattered relocation entry: "
                              "object file %s; entry type %ld; "
                              "address %#lx\n",
                              OC_INFORMATIVE_FILENAME(oc),
                              scat->r_type,
                              scat->r_address);
                        return 0;
                     }

#ifdef powerpc_HOST_ARCH
                    if(scat->r_type == GENERIC_RELOC_VANILLA
                        || scat->r_type == PPC_RELOC_SECTDIFF)
#else
                    if(scat->r_type == GENERIC_RELOC_VANILLA
                        || scat->r_type == GENERIC_RELOC_SECTDIFF
                        || scat->r_type == GENERIC_RELOC_LOCAL_SECTDIFF)
#endif
                    {
                        *wordPtr = word;
                    }
#ifdef powerpc_HOST_ARCH
                    else if (scat->r_type == PPC_RELOC_LO16_SECTDIFF
                          || scat->r_type == PPC_RELOC_LO16)
                    {
                        ((unsigned short*) wordPtr)[1] = word & 0xFFFF;
                    }
                    else if (scat->r_type == PPC_RELOC_HI16_SECTDIFF
                          || scat->r_type == PPC_RELOC_HI16)
                    {
                        ((unsigned short*) wordPtr)[1] = (word >> 16) & 0xFFFF;
                    }
                    else if (scat->r_type == PPC_RELOC_HA16_SECTDIFF
                          || scat->r_type == PPC_RELOC_HA16)
                    {
                        ((unsigned short*) wordPtr)[1] = ((word >> 16) & 0xFFFF)
                            + ((word & (1<<15)) ? 1 : 0);
                    }
#endif
                }
                else
                {
                    barf("Can't handle Mach-O scattered relocation entry "
                         "with this r_length tag: "
                         "object file %s; entry type %ld; "
                         "r_length tag %ld; address %#lx\n",
                         OC_INFORMATIVE_FILENAME(oc),
                         scat->r_type,
                         scat->r_length,
                         scat->r_address);
                    return 0;
                }
            }
            else /* scat->r_pcrel */
            {
                barf("Don't know how to handle *PC-relative* Mach-O "
                     "scattered relocation entry: "
                     "object file %s; entry type %ld; address %#lx\n",
                     OC_INFORMATIVE_FILENAME(oc),
                     scat->r_type,
                     scat->r_address);
               return 0;
            }

        }
        else /* !(relocs[i].r_address & R_SCATTERED) */
        {
            struct relocation_info *reloc = &relocs[i];
            if (reloc->r_pcrel && !reloc->r_extern) {
                IF_DEBUG(linker, debugBelch("relocateSection: pc relative but not external, skipping\n"));
                continue;
            }

            if (reloc->r_length == 2) {
                unsigned long word = 0;
#ifdef powerpc_HOST_ARCH
                unsigned long jumpIsland = 0;
                long offsetToJumpIsland = 0xBADBAD42; // initialise to bad value
                                                      // to avoid warning and to catch
                                                      // bugs.
#endif

                unsigned long* wordPtr = (unsigned long*) (image + sect->offset + reloc->r_address);

                /* In this check we assume that sizeof(unsigned long) = 2 * sizeof(unsigned short)
                   on powerpc_HOST_ARCH */
                checkProddableBlock(oc,wordPtr, sizeof(unsigned long));

                if (reloc->r_type == GENERIC_RELOC_VANILLA) {
                    word = *wordPtr;
                }
#ifdef powerpc_HOST_ARCH
                else if (reloc->r_type == PPC_RELOC_LO16) {
                    word = ((unsigned short*) wordPtr)[1];
                    word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF) << 16;
                }
                else if (reloc->r_type == PPC_RELOC_HI16) {
                    word = ((unsigned short*) wordPtr)[1] << 16;
                    word |= ((unsigned long) relocs[i+1].r_address & 0xFFFF);
                }
                else if (reloc->r_type == PPC_RELOC_HA16) {
                    word = ((unsigned short*) wordPtr)[1] << 16;
                    word += ((short)relocs[i+1].r_address & (short)0xFFFF);
                }
                else if (reloc->r_type == PPC_RELOC_BR24) {
                    word = *wordPtr;
                    word = (word & 0x03FFFFFC) | ((word & 0x02000000) ? 0xFC000000 : 0);
                }
#endif
                else {
                    barf("Can't handle this Mach-O relocation entry "
                         "(not scattered): "
                         "object file %s; entry type %ld; address %#lx\n",
                         OC_INFORMATIVE_FILENAME(oc),
                         reloc->r_type,
                         reloc->r_address);
                    return 0;
                }

                if (!reloc->r_extern) {
                    long delta = sections[reloc->r_symbolnum-1].offset
                        - sections[reloc->r_symbolnum-1].addr
                        + ((long) image);

                    word += delta;
                }
                else {
                    struct nlist *symbol = &nlist[reloc->r_symbolnum];
                    char *nm = image + symLC->stroff + symbol->n_un.n_strx;
                    void *symbolAddress = lookupSymbol_(nm);

                    if (!symbolAddress) {
                        errorBelch("\nunknown symbol `%s'", nm);
                        return 0;
                    }

                    if (reloc->r_pcrel) {
#ifdef powerpc_HOST_ARCH
                            // In the .o file, this should be a relative jump to NULL
                            // and we'll change it to a relative jump to the symbol
                        ASSERT(word + reloc->r_address == 0);
                        jumpIsland = (unsigned long)
                                        &makeSymbolExtra(oc,
                                                         reloc->r_symbolnum,
                                                         (unsigned long) symbolAddress)
                                         -> jumpIsland;
                        if (jumpIsland != 0) {
                            offsetToJumpIsland = word + jumpIsland
                                - (((long)image) + sect->offset - sect->addr);
                        }
#endif
                        word += (unsigned long) symbolAddress
                                - (((long)image) + sect->offset - sect->addr);
                    }
                    else {
                        word += (unsigned long) symbolAddress;
                    }
                }

                if (reloc->r_type == GENERIC_RELOC_VANILLA) {
                    *wordPtr = word;
                    continue;
                }
#ifdef powerpc_HOST_ARCH
                else if(reloc->r_type == PPC_RELOC_LO16)
                {
                    ((unsigned short*) wordPtr)[1] = word & 0xFFFF;
                    i++;
                    continue;
                }
                else if(reloc->r_type == PPC_RELOC_HI16)
                {
                    ((unsigned short*) wordPtr)[1] = (word >> 16) & 0xFFFF;
                    i++;
                    continue;
                }
                else if(reloc->r_type == PPC_RELOC_HA16)
                {
                    ((unsigned short*) wordPtr)[1] = ((word >> 16) & 0xFFFF)
                        + ((word & (1<<15)) ? 1 : 0);
                    i++;
                    continue;
                }
                else if(reloc->r_type == PPC_RELOC_BR24)
                {
                    if ((word & 0x03) != 0) {
                        barf("%s: unconditional relative branch with a displacement "
                             "which isn't a multiple of 4 bytes: %#lx",
                             OC_INFORMATIVE_FILENAME(oc),
                             word);
                    }

                    if((word & 0xFE000000) != 0xFE000000 &&
                        (word & 0xFE000000) != 0x00000000) {
                        // The branch offset is too large.
                        // Therefore, we try to use a jump island.
                        if (jumpIsland == 0) {
                            barf("%s: unconditional relative branch out of range: "
                                 "no jump island available: %#lx",
                                 OC_INFORMATIVE_FILENAME(oc),
                                 word);
                        }

                        word = offsetToJumpIsland;

                        if((word & 0xFE000000) != 0xFE000000 &&
                            (word & 0xFE000000) != 0x00000000) {
                            barf("%s: unconditional relative branch out of range: "
                                 "jump island out of range: %#lx",
                                 OC_INFORMATIVE_FILENAME(oc),
                                 word);
                    }
                    }
                    *wordPtr = (*wordPtr & 0xFC000003) | (word & 0x03FFFFFC);
                    continue;
                }
#endif
            }
            else
            {
                 barf("Can't handle Mach-O relocation entry (not scattered) "
                      "with this r_length tag: "
                      "object file %s; entry type %ld; "
                      "r_length tag %ld; address %#lx\n",
                      OC_INFORMATIVE_FILENAME(oc),
                      reloc->r_type,
                      reloc->r_length,
                      reloc->r_address);
                 return 0;
            }
        }
#endif
    }

    IF_DEBUG(linker, debugBelch("relocateSection: done\n"));
    return 1;
}

static int
ocGetNames_MachO(ObjectCode* oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    unsigned i,curSymbol = 0;
    struct segment_command *segLC = NULL;
    struct section *sections;
    struct symtab_command *symLC = NULL;
    struct nlist *nlist;
    unsigned long commonSize = 0;
    SymbolAddr* commonStorage = NULL;
    unsigned long commonCounter;

    IF_DEBUG(linker,debugBelch("ocGetNames_MachO: start\n"));

    for(i=0;i<header->ncmds;i++)
    {
        if (lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64) {
            segLC = (struct segment_command*) lc;
        }
        else if (lc->cmd == LC_SYMTAB) {
            symLC = (struct symtab_command*) lc;
        }

        lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }

    sections = (struct section*) (segLC+1);
    nlist = symLC ? (struct nlist*) (image + symLC->symoff)
                  : NULL;

    if (!segLC) {
        barf("ocGetNames_MachO: no segment load command");
    }

    Section *secArray;
    secArray = (Section*)stgCallocBytes(
         sizeof(Section),
         segLC->nsects,
         "ocGetNames_MachO(sections)");
   oc->sections = secArray;
   oc->n_sections = segLC->nsects;

    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: will load %d sections\n", segLC->nsects));
    for(i=0;i<segLC->nsects;i++)
    {
        IF_DEBUG(linker, debugBelch("ocGetNames_MachO: section %d\n", i));

        if (sections[i].size == 0) {
            IF_DEBUG(linker, debugBelch("ocGetNames_MachO: found a zero length section, skipping\n"));
            continue;
        }

        if((sections[i].flags & SECTION_TYPE) == S_ZEROFILL) {
            char * zeroFillArea;
            if (RTS_LINKER_USE_MMAP) {
                zeroFillArea = mmapForLinker(sections[i].size, MAP_ANONYMOUS,
                                            -1, 0);
                if (zeroFillArea == NULL) return 0;
                memset(zeroFillArea, 0, sections[i].size);
            }
            else {
                zeroFillArea = stgCallocBytes(1,sections[i].size,
                                      "ocGetNames_MachO(common symbols)");
            }
            sections[i].offset = zeroFillArea - image;
        }

        SectionKind kind = SECTIONKIND_OTHER;

        if (0==strcmp(sections[i].sectname,"__text")) {
            kind = SECTIONKIND_CODE_OR_RODATA;
        }
        else if (0==strcmp(sections[i].sectname,"__const") ||
                 0==strcmp(sections[i].sectname,"__data") ||
                 0==strcmp(sections[i].sectname,"__bss") ||
                 0==strcmp(sections[i].sectname,"__common") ||
                 0==strcmp(sections[i].sectname,"__mod_init_func")) {
            kind = SECTIONKIND_RWDATA;
        }

        addSection(&secArray[i], kind, SECTION_NOMEM,
                   (void *)(image + sections[i].offset),
                   sections[i].size,
                   0, 0, 0);

        addProddableBlock(oc,
                          (void *) (image + sections[i].offset),
                                        sections[i].size);
    }

        // count external symbols defined here
    oc->n_symbols = 0;
    if (symLC) {
        for (i = 0; i < symLC->nsyms; i++) {
            if (nlist[i].n_type & N_STAB) {
                ;
            }
            else if(nlist[i].n_type & N_EXT)
            {
                if((nlist[i].n_type & N_TYPE) == N_UNDF
                    && (nlist[i].n_value != 0))
                {
                    commonSize += nlist[i].n_value;
                    oc->n_symbols++;
                }
                else if((nlist[i].n_type & N_TYPE) == N_SECT)
                    oc->n_symbols++;
            }
        }
    }
    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: %d external symbols\n", oc->n_symbols));
    oc->symbols = stgMallocBytes(oc->n_symbols * sizeof(SymbolName*),
                                   "ocGetNames_MachO(oc->symbols)");

    if(symLC)
    {
        for(i=0;i<symLC->nsyms;i++)
        {
            if(nlist[i].n_type & N_STAB)
                ;
            else if((nlist[i].n_type & N_TYPE) == N_SECT)
            {
                if(nlist[i].n_type & N_EXT)
                {
                    SymbolName* nm = image + symLC->stroff + nlist[i].n_un.n_strx;
                    if ((nlist[i].n_desc & N_WEAK_DEF) && lookupSymbol_(nm)) {
                        // weak definition, and we already have a definition
                        IF_DEBUG(linker, debugBelch("    weak: %s\n", nm));
                    }
                    else
                    {
                            IF_DEBUG(linker, debugBelch("ocGetNames_MachO: inserting %s\n", nm));
                            SymbolAddr* addr = image
                                       + sections[nlist[i].n_sect - 1].offset
                                       - sections[nlist[i].n_sect - 1].addr
                                       + nlist[i].n_value;

                            ghciInsertSymbolTable( oc->fileName
                                                 , symhash
                                                 , nm
                                                 , addr
                                                 , HS_BOOL_FALSE
                                                 , oc);

                            oc->symbols[curSymbol] = nm;
                            curSymbol++;
                    }
                }
                else
                {
                    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: \t...not external, skipping\n"));
                }
            }
            else
            {
                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: \t...not defined in this section, skipping\n"));
            }
        }
    }

    commonStorage = stgCallocBytes(1,commonSize,"ocGetNames_MachO(common symbols)");
    commonCounter = (unsigned long)commonStorage;

    if (symLC) {
        for (i = 0; i < symLC->nsyms; i++) {
            if((nlist[i].n_type & N_TYPE) == N_UNDF
             && (nlist[i].n_type & N_EXT)
             && (nlist[i].n_value != 0)) {

                SymbolName* nm = image + symLC->stroff + nlist[i].n_un.n_strx;
                unsigned long sz = nlist[i].n_value;

                nlist[i].n_value = commonCounter;

                IF_DEBUG(linker, debugBelch("ocGetNames_MachO: inserting common symbol: %s\n", nm));
                ghciInsertSymbolTable(oc->fileName, symhash, nm,
                                       (void*)commonCounter, HS_BOOL_FALSE, oc);
                oc->symbols[curSymbol] = nm;
                curSymbol++;

                commonCounter += sz;
            }
        }
    }

    IF_DEBUG(linker, debugBelch("ocGetNames_MachO: done\n"));
    return 1;
}

static int
ocResolve_MachO(ObjectCode* oc)
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    unsigned i;
    struct segment_command *segLC = NULL;
    struct section *sections;
    struct symtab_command *symLC = NULL;
    struct dysymtab_command *dsymLC = NULL;
    struct nlist *nlist;

    IF_DEBUG(linker, debugBelch("ocResolve_MachO: start\n"));
    for (i = 0; i < header->ncmds; i++)
    {
        if (lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64) {
            segLC = (struct segment_command*) lc;
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: found a 32 or 64 bit segment load command\n"));
        }
        else if (lc->cmd == LC_SYMTAB) {
            symLC = (struct symtab_command*) lc;
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: found a symbol table load command\n"));
        }
        else if (lc->cmd == LC_DYSYMTAB) {
            dsymLC = (struct dysymtab_command*) lc;
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: found a dynamic symbol table load command\n"));
        }

        lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }

    sections = (struct section*) (segLC+1);
    nlist = symLC ? (struct nlist*) (image + symLC->symoff)
                  : NULL;

    if(dsymLC)
    {
        unsigned long *indirectSyms
            = (unsigned long*) (image + dsymLC->indirectsymoff);

        IF_DEBUG(linker, debugBelch("ocResolve_MachO: resolving dsymLC\n"));
        for (i = 0; i < segLC->nsects; i++)
        {
            if(    !strcmp(sections[i].sectname,"__la_symbol_ptr")
                || !strcmp(sections[i].sectname,"__la_sym_ptr2")
                || !strcmp(sections[i].sectname,"__la_sym_ptr3"))
            {
                if(!resolveImports(oc,image,symLC,&sections[i],indirectSyms,nlist))
                    return 0;
            }
            else if(!strcmp(sections[i].sectname,"__nl_symbol_ptr")
                ||  !strcmp(sections[i].sectname,"__pointers"))
            {
                if(!resolveImports(oc,image,symLC,&sections[i],indirectSyms,nlist))
                    return 0;
            }
            else if(!strcmp(sections[i].sectname,"__jump_table"))
            {
                if(!resolveImports(oc,image,symLC,&sections[i],indirectSyms,nlist))
                    return 0;
            }
            else
            {
                IF_DEBUG(linker, debugBelch("ocResolve_MachO: unknown section\n"));
            }
        }
    }

    for(i=0;i<segLC->nsects;i++)
    {
            IF_DEBUG(linker, debugBelch("ocResolve_MachO: relocating section %d\n", i));

        if (!relocateSection(oc,image,symLC,nlist,segLC->nsects,sections,&sections[i]))
            return 0;
    }

#if defined (powerpc_HOST_ARCH)
    ocFlushInstructionCache( oc );
#endif

    return 1;
}

static int ocRunInit_MachO ( ObjectCode *oc )
{
    char *image = (char*) oc->image;
    struct mach_header *header = (struct mach_header*) image;
    struct load_command *lc = (struct load_command*) (image + sizeof(struct mach_header));
    struct segment_command *segLC = NULL;
    struct section *sections;
    uint32_t i;

    for (i = 0; i < header->ncmds; i++) {
        if (lc->cmd == LC_SEGMENT || lc->cmd == LC_SEGMENT_64) {
            segLC = (struct segment_command*) lc;
        }
        lc = (struct load_command *) ( ((char*)lc) + lc->cmdsize );
    }
    if (!segLC) {
        barf("ocRunInit_MachO: no segment load command");
    }
    sections = (struct section*) (segLC+1);

    int argc, envc;
    char **argv, **envv;

    getProgArgv(&argc, &argv);
    getProgEnvv(&envc, &envv);

    for (i = 0; i < segLC->nsects; i++) {
        // ToDo: replace this with a proper check for the S_MOD_INIT_FUNC_POINTERS
        // flag.  We should do this elsewhere in the Mach-O linker code
        // too.  Note that the system linker will *refuse* to honor
        // sections which don't have this flag, so this could cause
        // weird behavior divergence (albeit reproduceable).
        if (0 == strcmp(sections[i].sectname,"__mod_init_func")) {
            char *init_startC = image + sections[i].offset;
            init_t *init = (init_t*)init_startC;
            init_t *init_end = (init_t*)(init_startC + sections[i].size);
            for (; init < init_end; init++) {
                (*init)(argc, argv, envv);
            }
        }
    }

    freeProgEnvv(envc, envv);
    return 1;
}

#ifdef powerpc_HOST_ARCH
/*
 * The Mach-O object format uses leading underscores. But not everywhere.
 * There is a small number of runtime support functions defined in
 * libcc_dynamic.a whose name does not have a leading underscore.
 * As a consequence, we can't get their address from C code.
 * We have to use inline assembler just to take the address of a function.
 * Yuck.
 */

extern void* symbolsWithoutUnderscore[];

static void
machoInitSymbolsWithoutUnderscore(void)
{
    void **p = symbolsWithoutUnderscore;
    __asm__ volatile(".globl _symbolsWithoutUnderscore\n.data\n_symbolsWithoutUnderscore:");

#undef SymI_NeedsProto
#undef SymI_NeedsDataProto

#define SymI_NeedsProto(x)  \
    __asm__ volatile(".long " # x);

#define SymI_NeedsDataProto(x) \
    SymI_NeedsProto(x)

    RTS_MACHO_NOUNDERLINE_SYMBOLS

    __asm__ volatile(".text");

#undef SymI_NeedsProto
#undef SymI_NeedsDataProto

#define SymI_NeedsProto(x)  \
    ghciInsertSymbolTable("(GHCi built-in symbols)", symhash, #x, *p++, HS_BOOL_FALSE, NULL);

#define SymI_NeedsDataProto(x) \
    SymI_NeedsProto(x)

    RTS_MACHO_NOUNDERLINE_SYMBOLS

#undef SymI_NeedsProto
#undef SymI_NeedsDataProto
}
#endif

#if defined(OBJFORMAT_MACHO)
/*
 * Figure out by how much to shift the entire Mach-O file in memory
 * when loading so that its single segment ends up 16-byte-aligned
 */
static int
machoGetMisalignment( FILE * f )
{
    struct mach_header header;
    int misalignment;

    {
        int n = fread(&header, sizeof(header), 1, f);
        if (n != 1) {
            barf("machoGetMisalignment: can't read the Mach-O header");
        }
    }
    fseek(f, -sizeof(header), SEEK_CUR);

#if x86_64_HOST_ARCH || powerpc64_HOST_ARCH
    if(header.magic != MH_MAGIC_64) {
        barf("Bad magic. Expected: %08x, got: %08x.",
             MH_MAGIC_64, header.magic);
    }
#else
    if(header.magic != MH_MAGIC) {
        barf("Bad magic. Expected: %08x, got: %08x.",
             MH_MAGIC, header.magic);
    }
#endif

    misalignment = (header.sizeofcmds + sizeof(header))
                    & 0xF;

    return misalignment ? (16 - misalignment) : 0;
}
#endif

#endif
