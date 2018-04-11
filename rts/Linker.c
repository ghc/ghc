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

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <fs_rts.h>

#if defined(HAVE_SYS_STAT_H)
#include <sys/stat.h>
#endif

#if defined(HAVE_DLFCN_H)
#include <dlfcn.h>
#endif

#if defined(OBJFORMAT_ELF)
#  include "linker/Elf.h"
#  include <regex.h>    // regex is already used by dlopen() so this is OK
                        // to use here without requiring an additional lib
#elif defined(OBJFORMAT_PEi386)
#  include "linker/PEi386.h"
#  include <windows.h>
#elif defined(OBJFORMAT_MACHO)
#  include "linker/MachO.h"
#  include <regex.h>
#  include <mach/machine.h>
#  include <mach-o/fat.h>
#endif

#if defined(x86_64_HOST_ARCH) && defined(darwin_HOST_OS)
#define ALWAYS_PIC
#endif

#if defined(dragonfly_HOST_OS)
#include <sys/tls.h>
#endif
/*
   Note [runtime-linker-support]
   -----------------------------
   When adding support for a new platform to the runtime linker please
   update `$TOP/configure.ac` under heading `Does target have runtime
   linker support?`.
 */
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
   never use. This is especially painful on Windows, where the number of
   libraries required to link things like mingwex grows to be quite high.

   We proceed through these stages as follows,

   * During Indexing we verify and open the ObjectCode and
     perform a quick scan/indexing of the ObjectCode. All the work
     required to actually load the ObjectCode is done.

     All symbols from the ObjectCode are also inserted into
     `symhash`, where possible duplicates are handled via the semantics
     described in `ghciInsertSymbolTable`.

     This phase will produce ObjectCode with status `OBJECT_LOADED` or `OBJECT_NEEDED`
     depending on whether they are an archive member or not.

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

   * lookupSymbols is used to lookup any symbols required, both during initial
     link and during statement and expression compilations in the REPL.
     Declaration of e.g. a foreign import, will eventually call lookupSymbol
     which will either fail (symbol unknown) or succeed (and possibly trigger a
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
/*Str*/HashTable *symhash;

/* List of currently loaded objects */
ObjectCode *objects = NULL;     /* initially empty */

/* List of objects that have been unloaded via unloadObj(), but are waiting
   to be actually freed via checkUnload() */
ObjectCode *unloaded_objects = NULL; /* initially empty */

#if defined(THREADED_RTS)
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

/* Generic wrapper function to try and Resolve and RunInit oc files */
int ocTryLoad( ObjectCode* oc );

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
    if (isSymbolImport (owner, key))
      stgFree(pinfo->value);

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
int ghciInsertSymbolTable(
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
#if defined(THREADED_RTS)
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
                                    symhash, sym->lbl, sym->addr,
                                    sym->weak, NULL)) {
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
           "(([^ \t()])+\\.so([^ \t:()])*):([ \t])*(invalid ELF header|file too short|invalid file format)",
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
#if defined(THREADED_RTS)
      closeMutex(&dl_mutex);
#endif
   }
#endif
   if (linker_init_done == 1) {
       freeHashTable(symhash, free);
   }
#if defined(THREADED_RTS)
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
      if ((fp = __rts_fopen(line, "r")) == NULL) {
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
   return addDLL_PEi386(dll_name, NULL);

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
    IF_DEBUG(linker, debugBelch("\nfindSystemLibrary: dll_name = `%"
                                PATH_FMT "'\n", dll_name));

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
    IF_DEBUG(linker, debugBelch("\naddLibrarySearchPath: dll_path = `%"
                                PATH_FMT "'\n", dll_path));

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
    IF_DEBUG(linker, debugBelch("\nremoveLibrarySearchPath: ptr = `%p'\n",
                                dll_path_index));

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
    return ghciInsertSymbolTable(obj_name, symhash, key, data, HS_BOOL_FALSE,
                                 NULL);
}

/* -----------------------------------------------------------------------------
 * lookup a symbol in the hash table
 */
#if defined(OBJFORMAT_PEi386)
SymbolAddr* lookupSymbol_ (SymbolName* lbl)
{
    return lookupSymbol_PEi386(lbl);
}

#else

SymbolAddr* lookupSymbol_ (SymbolName* lbl)
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
                 symbol name -- the dlsym routine puts it back on before
                 searching for the symbol. For now, we simply strip it off here
                 (and ONLY here).
        */
        IF_DEBUG(linker, debugBelch("lookupSymbol: looking up %s with dlsym\n",
                                    lbl));
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
    IF_DEBUG(linker, debugBelch("lookupSymbol: value of %s is %p\n", lbl,
                                pinfo->value));
    ObjectCode* oc = pinfo->owner;

    /* Symbol can be found during linking, but hasn't been relocated. Do so now.
        See Note [runtime-linker-phases] */
    if (oc && lbl && oc->status == OBJECT_LOADED) {
        oc->status = OBJECT_NEEDED;
        IF_DEBUG(linker, debugBelch("lookupSymbol: on-demand "
                                    "loading symbol '%s'\n", lbl));
        int r = ocTryLoad(oc);
        if (!r) {
            return NULL;
        }

#if defined(PROFILING)
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
    if (!r) {
        errorBelch("^^ Could not load '%s', dependency unresolved. "
                   "See top entry above.\n", lbl);
        fflush(stderr);
    }
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
#if defined(DEBUG)
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
             debugBelch("%p + %3d  ==  `%s'\n", addr,
                        (int)((char*)a->value - (char*)addr), sym);
         }
      }
   }
}
#endif

pathchar*
resolveSymbolAddr (pathchar* buffer, int size,
                   SymbolAddr* symbol, uintptr_t* top)
{
#if defined(OBJFORMAT_PEi386)
  return resolveSymbolAddr_PEi386 (buffer, size, symbol, top);
#else /* OBJFORMAT_PEi386 */
  (void)buffer;
  (void)size;
  (void)symbol;
  (void)top;
  return NULL;
#endif /* OBJFORMAT_PEi386 */
}

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
            if (oc->sections[i].info) {
                stgFree(oc->sections[i].info);
            }
        }
        stgFree(oc->sections);
    }

    freeProddableBlocks(oc);

    /* Free symbol_extras.  On x86_64 Windows, symbol_extras are allocated
     * alongside the image, so we don't need to free. */
#if defined(NEED_SYMBOL_EXTRAS) && (!defined(x86_64_HOST_ARCH) \
                                    || !defined(mingw32_HOST_OS))
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

#if defined(OBJECTFORMAT_MACHO)
    ocDeinit_MachO(oc);
#endif
#if defined(OBJFORMAT_ELF)
    ocDeinit_ELF(oc);
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

ObjectCode*
mkOc( pathchar *path, char *image, int imageSize,
      bool mapped, char *archiveMemberName, int misalignment ) {
   ObjectCode* oc;

   IF_DEBUG(linker, debugBelch("mkOc: start\n"));
   oc = stgMallocBytes(sizeof(ObjectCode), "mkOc(oc)");

   oc->info = NULL;

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
       oc->archiveMemberName = stgMallocBytes( strlen(archiveMemberName)+1,
                                               "loadObj" );
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
#if defined(NEED_SYMBOL_EXTRAS)
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
HsInt
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

   /* iOS does not permit to mmap with r+w+x, however while the comment for
    * this function says this is not the final resting place, for some
    * architectures / hosts (at least mach-o non-iOS -- see ocGetNames_MachO)
    * the image mmaped here in fact ends up being the final resting place for
    * the sections. And hence we need to leave r+w+x here for other hosts
    * until all hosts have been made aware of the initial image being r+w only.
    *
    * See also the misalignment logic for darwin below.
    */
#if defined(ios_HOST_OS)
   image = mmap(NULL, fileSize, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);
#else
   image = mmap(NULL, fileSize, PROT_READ|PROT_WRITE|PROT_EXEC,
                MAP_PRIVATE, fd, 0);
#endif

   if (image == MAP_FAILED) {
       errorBelch("mmap: failed. errno = %d", errno);
   }
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

   oc = mkOc(path, image, fileSize, true, NULL, misalignment);

#if defined(OBJFORMAT_MACHO)
   if (ocVerifyImage_MachO( oc ))
       ocInit_MachO( oc );
#endif
#if defined(OBJFORMAT_ELF)
   if(ocVerifyImage_ELF( oc ))
       ocInit_ELF( oc );
#endif
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

HsInt loadOc (ObjectCode* oc)
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

#if defined(NEED_SYMBOL_EXTRAS)
#  if defined(OBJFORMAT_MACHO)
   r = ocAllocateSymbolExtras_MachO ( oc );
   if (!r) {
       IF_DEBUG(linker,
                debugBelch("loadOc: ocAllocateSymbolExtras_MachO failed\n"));
       return r;
   }
#  elif defined(OBJFORMAT_ELF)
   r = ocAllocateSymbolExtras_ELF ( oc );
   if (!r) {
       IF_DEBUG(linker,
                debugBelch("loadOc: ocAllocateSymbolExtras_ELF failed\n"));
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
            && !ghciInsertSymbolTable(oc->fileName, symhash, symbol, NULL,
                                      isSymbolWeak(oc, symbol), oc)) {
            return 0;
        }
    }

#   if defined(OBJFORMAT_ELF)
    r = ocResolve_ELF ( oc );
#   elif defined(OBJFORMAT_PEi386)
    r = ocResolve_PEi386 ( oc );
#   elif defined(OBJFORMAT_MACHO)
    r = ocResolve_MachO ( oc );
#   else
    barf("ocTryLoad: not implemented on this platform");
#   endif
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

#if defined(PROFILING)
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
static HsInt unloadObj_ (pathchar *path, bool just_purge)
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
    HsInt r = unloadObj_(path, false);
    RELEASE_LOCK(&linker_mutex);
    return r;
}

HsInt purgeObj (pathchar *path)
{
    ACQUIRE_LOCK(&linker_mutex);
    HsInt r = unloadObj_(path, true);
    RELEASE_LOCK(&linker_mutex);
    return r;
}

static OStatus getObjectLoadStatus_ (pathchar *path)
{
    ObjectCode *o;
    for (o = objects; o; o = o->next) {
       if (0 == pathcmp(o->fileName, path)) {
           return o->status;
       }
    }
    for (o = unloaded_objects; o; o = o->next) {
       if (0 == pathcmp(o->fileName, path)) {
           return o->status;
       }
    }
    return OBJECT_NOT_LOADED;
}

OStatus getObjectLoadStatus (pathchar *path)
{
    ACQUIRE_LOCK(&linker_mutex);
    OStatus r = getObjectLoadStatus_(path);
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

   s->info = (struct SectionFormatInfo*)stgCallocBytes(1, sizeof *s->info,
                                            "addSection(SectionFormatInfo)");

   IF_DEBUG(linker,
            debugBelch("addSection: %p-%p (size %" FMT_Word "), kind %d\n",
                       start, (void*)((StgWord)start + size),
                       size, kind ));
}

