/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2012
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#if 0
#include "rts/PosixSource.h"
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
#include "StablePtr.h"
#include "RtsSymbols.h"
#include "RtsSymbolInfo.h"
#include "Profiling.h"
#include "ForeignExports.h"
#include "sm/OSMem.h"
#include "linker/M32Alloc.h"
#include "linker/CacheFlush.h"
#include "linker/SymbolExtras.h"
#include "linker/MMap.h"
#include "PathUtils.h"
#include "CheckUnload.h" // createOCSectionIndices
#include "ReportMemoryMap.h"

#if !defined(mingw32_HOST_OS) && defined(HAVE_SIGNAL_H)
#include "posix/Signals.h"
#endif

// get protos for is*()
#include <ctype.h>

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#include <fcntl.h>
#include <unistd.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
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

#if defined(dragonfly_HOST_OS)
#include <sys/tls.h>
#endif

/*
 * Note [iconv and FreeBSD]
 * ~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * On FreeBSD libc.so provides an implementation of the iconv_* family of
 * functions. However, due to their implementation, these symbols cannot be
 * resolved via dlsym(); rather, they can only be resolved using the
 * explicitly-versioned dlvsym().
 *
 * This is problematic for the RTS linker since we may be asked to load
 * an object that depends upon iconv. To handle this we include a set of
 * fallback cases for these functions, allowing us to resolve them to the
 * symbols provided by the libc against which the RTS is linked.
 *
 * See #20354.
 */

#if defined(freebsd_HOST_OS)
extern void iconvctl();
extern void iconv_open_into();
extern void iconv_open();
extern void iconv_close();
extern void iconv_canonicalize();
extern void iconv();
#endif

/*
   Note [runtime-linker-support]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Broadly the behavior of the runtime linker can be
   split into the following five phases:

   - Indexing (e.g. ocVerifyImage and ocGetNames)
   - Initialization (e.g. ocResolve)
   - RunInit (e.g. ocRunInit)
   - Lookup (e.g. lookupSymbol)

   This is to enable lazy loading of symbols. Eager loading is problematic
   as it means that all symbols must be available, even those which we will
   never use. This is especially painful on Windows, where the number of
   libraries required to link things like QT or WxWidgets grows to be quite high.

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
     directly to the command-line we perform lookupSymbol on the required
     symbols. lookupSymbol may trigger the loading of additional ObjectCode
     if required. After resolving an object we mark its text as executable and
     not writable.

     This phase will produce ObjectCode with status `OBJECT_RESOLVED` if
     the previous status was `OBJECT_NEEDED`.

   * During RunInit we run the initializers ("constructors") of the objects
     that are in `OBJECT_RESOLVED` state and move them to `OBJECT_READY` state.
     This must be in a separate phase since we must ensure that all needed
     objects have been fully resolved before we can run their initializers.
     This is particularly tricky in the presence of cyclic dependencies (see
     #21253).

   * lookupSymbol is used to lookup any symbols required, both during initial
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
      such as ucrt can be broken down.

   2) The number of duplicate symbols, since now only symbols that are
      true duplicates will display the error.
 */
StrHashTable *symhash;

#if defined(THREADED_RTS)
/* This protects all the Linker's global state */
Mutex linker_mutex;
#endif

/* Generic wrapper function to try and resolve oc files */
static int ocTryLoad( ObjectCode* oc );
/* Run initializers */
static int ocRunInit( ObjectCode* oc );
static int runPendingInitializers (void);

static void ghciRemoveSymbolTable(StrHashTable *table, const SymbolName* key,
    ObjectCode *owner)
{
    RtsSymbolInfo *pinfo = lookupStrHashTable(table, key);
    if (!pinfo || owner != pinfo->owner) return;
    removeStrHashTable(table, key, NULL);
    if (isSymbolImport (owner, key))
      stgFree(pinfo->value);

    stgFree(pinfo);
}

static const char *
symbolTypeString (SymType type)
{
    switch (type & ~SYM_TYPE_DUP_DISCARD) {
        case SYM_TYPE_CODE: return "code";
        case SYM_TYPE_DATA: return "data";
        case SYM_TYPE_INDIRECT_DATA: return "indirect-data";
        default: barf("symbolTypeString: unknown symbol type");
    }
}

/* -----------------------------------------------------------------------------
 * Insert symbols into hash tables, checking for duplicates.
 *
 * Returns: 0 on failure, nonzero on success
 */
/*
 Note [weak-symbols-support]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
 While ghciInsertSymbolTable does implement extensive
 logic for weak symbol support, weak symbols are not currently
 fully supported by the RTS. This code is mostly here for COMDAT
 support which uses the weak symbols support.

 Linking weak symbols defined purely in C code with other C code
 should also work, probably. Observing weak symbols in Haskell
 won't.

 Some test have been written for weak symbols but have been disabled
 mostly because it's unsure how the weak symbols support should look.
 See #11223
 */
int ghciInsertSymbolTable(
   pathchar* obj_name,
   StrHashTable *table,
   const SymbolName* key,
   SymbolAddr* data,
   SymStrength strength,
   SymType type,
   ObjectCode *owner)
{
   RtsSymbolInfo *pinfo = lookupStrHashTable(table, key);
   if (!pinfo) /* new entry */
   {
      pinfo = stgMallocBytes(sizeof (*pinfo), "ghciInsertToSymbolTable");
      pinfo->value = data;
      pinfo->owner = owner;
      pinfo->strength = strength;
      pinfo->type = type;
      insertStrHashTable(table, key, pinfo);
      return 1;
   }
   else if (pinfo->type ^ type)
   {
       /* We were asked to discard the symbol on duplicates, do so quietly.  */
       if (!(type & SYM_TYPE_DUP_DISCARD))
       {
         debugBelch("Symbol type mismatch.\n");
         debugBelch("Symbol %s was defined by %" PATH_FMT " to be a %s symbol.\n",
                    key, obj_name, symbolTypeString(type));
         debugBelch("      yet was defined by %" PATH_FMT " to be a %s symbol.\n",
                    pinfo->owner ? pinfo->owner->fileName : WSTR("<builtin>"),
                    symbolTypeString(pinfo->type));
       }
       return 1;
   }
   else if (pinfo->strength == STRENGTH_STRONG)
   {
       /* The existing symbol is strong meaning we must never override it */
       IF_DEBUG(linker, debugBelch("%s is already defined as a strong symbol; ignoring redefinition...", key));
       return 1;
   }
   else if (strength == STRENGTH_WEAK &&
            data &&
            pinfo->strength == STRENGTH_WEAK &&
            !pinfo->value)
   {
       /* The existing symbol is weak with a zero value; replace it with the new symbol. */
       pinfo->value = data;
       pinfo->owner = owner;
       return 1;
   }
   else if (strength == STRENGTH_WEAK)
   {
       return 1; /* weak symbol, because the symbol is weak, data = 0 and we
                 already know of another copy throw this one away.

                 or both weak symbols have a nonzero value. Keep the existing one.

                 This also preserves the semantics of linking against
                 the first symbol we find. */
   }
   else if (pinfo->strength == STRENGTH_WEAK && strength != STRENGTH_WEAK) /* weak symbol is in the table */
   {
      /* override the weak definition with the non-weak one */
      pinfo->value = data;
      pinfo->owner = owner;
      pinfo->strength = strength;
      return 1;
   }
   else if (  pinfo->owner
           && pinfo->owner->status != OBJECT_READY
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
       if (owner && (owner->status == OBJECT_NEEDED
                     || owner->status == OBJECT_RESOLVED
                     || owner->status == OBJECT_READY)) {
           pinfo->value = data;
           pinfo->owner = owner;
           pinfo->strength = strength;
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
      pinfo->owner->archiveMemberName ? pinfo->owner->archiveMemberName
      : pinfo->owner->fileName
   );

   return 0;
}

/* -----------------------------------------------------------------------------
* Looks up symbols into hash tables.
*
* Returns: 0 on failure and result is not set,
*          nonzero on success and result set to nonzero pointer
*/
HsBool ghciLookupSymbolInfo(StrHashTable *table,
    const SymbolName* key, RtsSymbolInfo **result)
{
    RtsSymbolInfo *pinfo = lookupStrHashTable(table, key);
    if (!pinfo) {
        *result = NULL;
        return HS_BOOL_FALSE;
    }
    if (pinfo->strength == STRENGTH_WEAK) {
        IF_DEBUG(linker, debugBelch("lookupSymbolInfo: promoting %s\n", key));
        /* Once it's looked up, it can no longer be overridden */
        pinfo->strength = STRENGTH_NORMAL;
    }

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
Mutex dl_mutex; // mutex to protect dlopen/dlerror critical section
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

    initUnloadCheck();

#if defined(THREADED_RTS)
    initMutex(&linker_mutex);
#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
    initMutex(&dl_mutex);
#endif
#endif

    symhash = allocStrHashTable();

    /* populate the symbol table with stuff from the RTS */
    for (const RtsSymbolVal *sym = rtsSyms; sym->lbl != NULL; sym++) {
        if (! ghciInsertSymbolTable(WSTR("(GHCi built-in symbols)"),
                                    symhash, sym->lbl, sym->addr,
                                    sym->strength, sym->type, NULL)) {
            barf("ghciInsertSymbolTable failed");
        }
        IF_DEBUG(linker, debugBelch("initLinker: inserting rts symbol %s, %p\n", sym->lbl, sym->addr));
    }

    // Redirect newCAF to newRetainedCAF if retain_cafs is true.
    if (! ghciInsertSymbolTable(WSTR("(GHCi built-in symbols)"), symhash,
                                MAYBE_LEADING_UNDERSCORE_STR("newCAF"),
                                retain_cafs ? newRetainedCAF : newGCdCAF,
                                HS_BOOL_FALSE, SYM_TYPE_CODE, NULL)) {
        barf("ghciInsertSymbolTable failed");
    }

#   if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)
#   if defined(RTLD_DEFAULT)
    dl_prog_handle = RTLD_DEFAULT;
#   else
    dl_prog_handle = dlopen(NULL, RTLD_LAZY);
#   endif /* RTLD_DEFAULT */

    compileResult = regcomp(&re_invalid,
           "(([^ \t()])+\\.so([^ \t:()])*):([ \t])*(invalid ELF header|file too short|invalid file format|Exec format error)",
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

    if (RtsFlags.MiscFlags.linkerMemBase != 0) {
        // User-override for mmap_32bit_base
        mmap_32bit_base = (void*)RtsFlags.MiscFlags.linkerMemBase;
    }

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
       freeStrHashTable(symhash, free);
       exitUnloadCheck();
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
  ~~~~~~~~~~~~~~~~~
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

    // clears dlerror
    dlerror();

    // look in program first
    v = dlsym(dl_prog_handle, symbol);
    if (dlerror() == NULL) {
        RELEASE_LOCK(&dl_mutex);
        IF_DEBUG(linker, debugBelch("internal_dlsym: found symbol '%s' in program\n", symbol));
        return v;
    }

    for (o_so = openedSOs; o_so != NULL; o_so = o_so->next) {
        v = dlsym(o_so->handle, symbol);
        if (dlerror() == NULL) {
            IF_DEBUG(linker, debugBelch("internal_dlsym: found symbol '%s' in shared object\n", symbol));
            RELEASE_LOCK(&dl_mutex);
            return v;
        }
    }
    RELEASE_LOCK(&dl_mutex);

    IF_DEBUG(linker, debugBelch("internal_dlsym: looking for symbol '%s' in special cases\n", symbol));
#   define SPECIAL_SYMBOL(sym) \
      if (strcmp(symbol, #sym) == 0) return (void*)&sym;

#   if defined(HAVE_SYS_STAT_H) && defined(linux_HOST_OS) && defined(__GLIBC__)
    // HACK: GLIBC implements these functions with a great deal of trickery where
    //       they are either inlined at compile time to their corresponding
    //       __xxxx(SYS_VER, ...) function or direct syscalls, or resolved at
    //       link time via libc_nonshared.a.
    //
    //       We borrow the approach that the LLVM JIT uses to resolve these
    //       symbols. See http://llvm.org/PR274 and #7072 for more info.

    SPECIAL_SYMBOL(stat);
    SPECIAL_SYMBOL(fstat);
    SPECIAL_SYMBOL(lstat);
    SPECIAL_SYMBOL(stat64);
    SPECIAL_SYMBOL(fstat64);
    SPECIAL_SYMBOL(lstat64);
    SPECIAL_SYMBOL(atexit);
    SPECIAL_SYMBOL(mknod);
#   endif

    // See Note [iconv and FreeBSD]
#   if defined(freebsd_HOST_OS)
    SPECIAL_SYMBOL(iconvctl);
    SPECIAL_SYMBOL(iconv_open_into);
    SPECIAL_SYMBOL(iconv_open);
    SPECIAL_SYMBOL(iconv_close);
    SPECIAL_SYMBOL(iconv_canonicalize);
    SPECIAL_SYMBOL(iconv);
#   endif

#undef SPECIAL_SYMBOL

    // we failed to find the symbol
    return NULL;
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

   // GHC #2615
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
      IF_DEBUG(linker, debugBelch("file name = '%s'\n", line));
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
 * insert a code symbol in the hash table
 *
 * Returns: 0 on failure, nonzero on success
 */
HsInt insertSymbol(pathchar* obj_name, SymbolName* key, SymbolAddr* data)
{
    return ghciInsertSymbolTable(obj_name, symhash, key, data, HS_BOOL_FALSE,
                                 SYM_TYPE_CODE, NULL);
}

/* -----------------------------------------------------------------------------
 * Lookup a symbol in the hash table
 *
 * When 'dependent' is not NULL, adds it as a dependent to the owner of the
 * symbol.
 */
#if defined(OBJFORMAT_PEi386)
SymbolAddr* lookupDependentSymbol (SymbolName* lbl, ObjectCode *dependent, SymType *type)
{
    ASSERT_LOCK_HELD(&linker_mutex);
    return lookupSymbol_PEi386(lbl, dependent, type);
}

#else

SymbolAddr* lookupDependentSymbol (SymbolName* lbl, ObjectCode *dependent, SymType *type)
{
    ASSERT_LOCK_HELD(&linker_mutex);
    IF_DEBUG(linker_verbose, debugBelch("lookupSymbol: looking up '%s'\n", lbl));

    ASSERT(symhash != NULL);
    RtsSymbolInfo *pinfo;

    /* See Note [Resolving __dso_handle] */
    if (strcmp(lbl, MAYBE_LEADING_UNDERSCORE_STR("__dso_handle")) == 0) {
        if (dependent) {
            return dependent->image;
        } else {
            // In the case that we don't know which object the reference lives
            // in we return a random symbol from the executable image.
            return &lookupDependentSymbol;
        }
    }
    if (strcmp(lbl, MAYBE_LEADING_UNDERSCORE_STR("__cxa_atexit")) == 0 && dependent) {
        dependent->cxa_finalize = (cxa_finalize_fn) lookupDependentSymbol(
                MAYBE_LEADING_UNDERSCORE_STR("__cxa_finalize"),
                dependent,
                NULL);
    }

#if defined(OBJFORMAT_ELF)
    // Resolve references to the GOT if we know the origin object
    if (dependent && strncmp(lbl, "_GLOBAL_OFFSET_TABLE_", 21) == 0) {
        return dependent->info->got_start;
    }
#endif

    if (!ghciLookupSymbolInfo(symhash, lbl, &pinfo)) {
        IF_DEBUG(linker_verbose, debugBelch("lookupSymbol: symbol '%s' not found, trying dlsym\n", lbl));

#       if defined(OBJFORMAT_ELF)
        SymbolAddr *ret = internal_dlsym(lbl);
        if (type) {
            // We assume that the symbol is code since this is usually the case
            // and dlsym doesn't tell us.
            *type = SYM_TYPE_CODE;
        }

        // Generally the dynamic linker would define _DYNAMIC, which is
        // supposed to point to various bits of dynamic linker state (see
        // [1]). However, if dynamic linking isn't supported (e.g. in the case
        // of musl) then we can safely declare that it is NULL.
        //
        // [1] https://wiki.gentoo.org/wiki/Hardened/Introduction_to_Position_Independent_Code
        if (ret == NULL && strcmp(lbl, "_DYNAMIC") == 0) {
            static void *RTS_DYNAMIC = NULL;
            ret = (SymbolAddr *) &RTS_DYNAMIC;
            if (type) {
                *type = SYM_TYPE_DATA;
            }
        }
        return ret;
#       elif defined(OBJFORMAT_MACHO)

        /* HACK: On OS X, all symbols are prefixed with an underscore.
                 However, dlsym wants us to omit the leading underscore from the
                 symbol name -- the dlsym routine puts it back on before
                 searching for the symbol. For now, we simply strip it off here
                 (and ONLY here).
        */
        IF_DEBUG(linker, debugBelch("lookupSymbol: looking up %s with dlsym\n",
                                    lbl));
        CHECK(lbl[0] == '_');
        if (type) {
            // We assume that the symbol is code since this is usually the case
            // and dlsym doesn't tell us.
            *type = SYM_TYPE_CODE;
        }
        return internal_dlsym(lbl + 1);

#       elif defined(OBJFORMAT_WASM32)
        return NULL;
#       else
#       error No OBJFORMAT_* macro set
#       endif
    } else {
        static void *RTS_NO_FINI = NULL;
        if (strcmp(lbl, "__fini_array_end") == 0) { return (SymbolAddr *) &RTS_NO_FINI; }
        if (strcmp(lbl, "__fini_array_start") == 0) { return (SymbolAddr *) &RTS_NO_FINI; }
        if (type) {
            // This is an assumption
            *type = pinfo->type;
        }

        if (dependent) {
            // Add dependent as symbol's owner's dependency
            ObjectCode *owner = pinfo->owner;
            if (owner) {
                // TODO: what does it mean for a symbol to not have an owner?
                insertHashSet(dependent->dependencies, (W_)owner);
            }
        }
        return loadSymbol(lbl, pinfo);
    }
}
#endif /* OBJFORMAT_PEi386 */

/* Note [Resolving __dso_handle]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * This symbol, which is defined by the C++ ABI, would typically be defined by
 * the system's dynamic linker to act as a "handle", identifying a particular
 * loaded dynamic object to the C++ standard library for the purpose of running
 * destructors on unload. Here we behave the same way that the dynamic linker
 * would, using some address (here the start address) of the loaded object as
 * its handle.
 *
 * Note that references to __dso_handle may be relocated using
 * relocations of bounded displacement and therefore __dso_handle must not be
 * too far from the loaded object's code (hence using its start address).
 *
 * Finally, when we see a reference to __cxa_atexit in an object we take care
 * to lookup and record the address of __cxa_finalize (largely to ensure that
 * the symbol dependency is recorded) and call it with the appropriate handle
 * when the object is unloaded.
 *
 * See #20493.
 * See section 3.3.5 of the Itanium C++ ABI, version 1.83.
 */

/*
 * Load and relocate the object code for a symbol as necessary.
 * Symbol name only used for diagnostics output.
 */
SymbolAddr* loadSymbol(SymbolName *lbl, RtsSymbolInfo *pinfo) {
    IF_DEBUG(linker_verbose,
             debugBelch("lookupSymbol: value of %s is %p, owned by %" PATH_FMT "\n", lbl,
                        pinfo->value,
                        pinfo->owner ? OC_INFORMATIVE_FILENAME(pinfo->owner) : WSTR("No owner, probably built-in.")));
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
        refreshProfilingCCSs();
#endif
    }

    return pinfo->value;
}

void
printLoadedObjects(void) {
    ObjectCode* oc;
    for (oc = objects; oc; oc = oc->next) {
        if (oc->sections != NULL) {
            int i;
            printf("%" PATH_FMT "\n", OC_INFORMATIVE_FILENAME(oc));
            for (i=0; i < oc->n_sections; i++) {
                if(oc->sections[i].mapped_start != NULL || oc->sections[i].start != NULL) {
                    printf("\tsec %2d[alloc: %d; kind: %d]: %p - %p; mmaped: %p - %p\n",
                        i, oc->sections[i].alloc, oc->sections[i].kind,
                        oc->sections[i].start,
                        (void*)((uintptr_t)(oc->sections[i].start) + oc->sections[i].size),
                        oc->sections[i].mapped_start,
                        (void*)((uintptr_t)(oc->sections[i].mapped_start) + oc->sections[i].mapped_size));
                }
            }
        }
   }
}

SymbolAddr* lookupSymbol( SymbolName* lbl )
{
    ACQUIRE_LOCK(&linker_mutex);
    // NULL for "don't add dependent". When adding a dependency we call
    // lookupDependentSymbol directly.
    SymbolAddr* r = lookupDependentSymbol(lbl, NULL, NULL);
    if (!r) {
        errorBelch("^^ Could not load '%s', dependency unresolved. "
                   "See top entry above.\n", lbl);
        IF_DEBUG(linker, printLoadedObjects());
        fflush(stderr);
    }

    if (!runPendingInitializers()) {
        errorBelch("lookupSymbol: Failed to run initializers.");
    }
    RELEASE_LOCK(&linker_mutex);
    return r;
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
         sym = oc->symbols[i].name;
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
        if (oc->symbols[i].name != NULL) {
            ghciRemoveSymbolTable(symhash, oc->symbols[i].name, oc);
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
    struct ForeignExportsList *exports, *next;

    for (exports = oc->foreign_exports; exports != NULL; exports = next) {
        next = exports->next;
        for (int i = 0; i < exports->n_entries; i++) {
            freeStablePtr(exports->stable_ptrs[i]);
        }
        stgFree(exports->stable_ptrs);
        exports->stable_ptrs = NULL;
        exports->next = NULL;
    }
    oc->foreign_exports = NULL;
}

static void
freePreloadObjectFile (ObjectCode *oc)
{
#if defined(mingw32_HOST_OS)
    freePreloadObjectFile_PEi386(oc);
#else

    if (RTS_LINKER_USE_MMAP && oc->imageMapped) {
        munmapForLinker(oc->image, oc->fileSize, "freePreloadObjectFile");
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
    IF_DEBUG(linker, ocDebugBelch(oc, "start\n"));

    // Run finalizers
    if (oc->type == STATIC_OBJECT &&
            (oc->status == OBJECT_READY || oc->status == OBJECT_UNLOADED)) {
        // Only run finalizers if the initializers have also been run, which
        // happens when we resolve the object.
#if defined(OBJFORMAT_ELF)
        ocRunFini_ELF(oc);
#elif defined(OBJFORMAT_PEi386)
        ocRunFini_PEi386(oc);
#elif defined(OBJFORMAT_MACHO)
        ocRunFini_MachO(oc);
#endif
    }

    // See Note [Resolving __dso_handle]
    if (oc->cxa_finalize) {
        oc->cxa_finalize(oc->image);
    }

    if (oc->type == DYNAMIC_OBJECT) {
#if defined(OBJFORMAT_ELF)
        ACQUIRE_LOCK(&dl_mutex);
        freeNativeCode_ELF(oc);
        RELEASE_LOCK(&dl_mutex);
#else
        barf("freeObjectCode: This shouldn't happen");
#endif
    }

    freePreloadObjectFile(oc);

    if (oc->symbols != NULL) {
        stgFree(oc->symbols);
        oc->symbols = NULL;
    }

    if (oc->extraInfos != NULL) {
        freeStrHashTable(oc->extraInfos, NULL);
        oc->extraInfos = NULL;
    }

    if (oc->sections != NULL) {
        int i;
        for (i=0; i < oc->n_sections; i++) {
            if (oc->sections[i].start != NULL) {
                switch(oc->sections[i].alloc){
#if RTS_LINKER_USE_MMAP
                case SECTION_MMAP:
                    munmapForLinker(
                        oc->sections[i].mapped_start,
                        oc->sections[i].mapped_size,
                        "freeObjectCode");
                    break;
#endif
                case SECTION_M32:
                    // Freed by m32_allocator_free
                    break;
                case SECTION_MALLOC:
                    IF_DEBUG(zero_on_gc,
                        memset(oc->sections[i].start,
                            0x00, oc->sections[i].size));
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
    freeSegments(oc);

    /* Free symbol_extras.  On x86_64 Windows, symbol_extras are allocated
     * alongside the image, so we don't need to free. */
#if defined(NEED_SYMBOL_EXTRAS) && (!defined(x86_64_HOST_ARCH) \
                                    || !defined(mingw32_HOST_OS))
    if (RTS_LINKER_USE_MMAP) {
      if (!USE_CONTIGUOUS_MMAP && !RtsFlags.MiscFlags.linkerAlwaysPic &&
          oc->symbol_extras != NULL) {
        // Freed by m32_allocator_free
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

#if defined(NEED_M32)
    m32_allocator_free(oc->rx_m32);
    m32_allocator_free(oc->rw_m32);
#endif

    stgFree(oc->fileName);
    stgFree(oc->archiveMemberName);

    freeHashSet(oc->dependencies);

    stgFree(oc);
}

ObjectCode*
mkOc( ObjectType type, pathchar *path, char *image, int imageSize,
      bool mapped, pathchar *archiveMemberName, int misalignment ) {
   ObjectCode* oc;


   IF_DEBUG(linker, debugBelch("mkOc: %" PATH_FMT "\n", path));
   oc = stgMallocBytes(sizeof(ObjectCode), "mkOc(oc)");

   oc->info = NULL;
   oc->type = type;

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
       oc->archiveMemberName = stgMallocBytes( (pathlen(archiveMemberName)+1) * pathsize,
                                               "loadObj" );
       pathcopy(oc->archiveMemberName, archiveMemberName);
   } else {
       oc->archiveMemberName = NULL;
   }

   if (oc->archiveMemberName == NULL) {
       oc->status = OBJECT_NEEDED;
   } else {
       oc->status = OBJECT_LOADED;
   }

   oc->fileSize          = imageSize;
   oc->n_symbols         = 0;
   oc->symbols           = NULL;
   oc->n_sections        = 0;
   oc->sections          = NULL;
   oc->n_segments        = 0;
   oc->segments          = NULL;
   oc->proddables        = NULL;
   oc->foreign_exports   = NULL;
#if defined(NEED_SYMBOL_EXTRAS)
   oc->symbol_extras     = NULL;
#endif
   oc->bssBegin          = NULL;
   oc->bssEnd            = NULL;
   oc->imageMapped       = mapped;

   oc->misalignment      = misalignment;
   oc->cxa_finalize      = NULL;
   oc->extraInfos        = NULL;

   /* chain it onto the list of objects */
   oc->next              = NULL;
   oc->prev              = NULL;
   oc->next_loaded_object = NULL;
   oc->mark              = object_code_mark_bit;
   oc->dependencies      = allocHashSet();

#if defined(NEED_M32)
   oc->rw_m32 = m32_allocator_new(false);
   oc->rx_m32 = m32_allocator_new(true);
#endif

#if defined(OBJFORMAT_ELF) && defined(SHN_XINDEX)
   oc->shndx_table = SHNDX_TABLE_UNINIT;
#endif

   oc->nc_ranges = NULL;
   oc->dlopen_handle = NULL;

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
    for (ObjectCode *o = objects; o; o = o->next) {
       if (0 == pathcmp(o->fileName, path)
           && o->status != OBJECT_UNLOADED) {
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
#if defined(darwin_HOST_OS) || defined(openbsd_HOST_OS)
   image = mmapForLinker(fileSize, MEM_READ_WRITE, MAP_PRIVATE, fd, 0);
#else
   image = mmapForLinker(fileSize, MEM_READ_WRITE_EXECUTE, MAP_PRIVATE, fd, 0);
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

#  if defined(darwin_HOST_OS)

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

# else /* !defined(darwin_HOST_OS) */

   image = stgMallocBytes(fileSize, "loadObj(image)");

#endif /* !defined(darwin_HOST_OS) */

   int n;
   n = fread ( image, 1, fileSize, f );
   fclose(f);
   if (n != fileSize) {
       errorBelch("loadObj: error whilst reading `%" PATH_FMT "'", path);
       stgFree(image);
       return NULL;
   }

#endif /* RTS_LINKER_USE_MMAP */

   IF_DEBUG(linker, debugBelch("loadObj: preloaded image at %p\n", (void *) image));

   /* FIXME (AP): =mapped= parameter unconditionally set to true */
   oc = mkOc(STATIC_OBJECT, path, image, fileSize, true, NULL, misalignment);

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
   // Check that we haven't already loaded this object.
   // Ignore requests to load multiple times

   if (isAlreadyLoaded(path)) {
       IF_DEBUG(linker,
                debugBelch("ignoring repeated load of %" PATH_FMT "\n", path));
       return 1; // success
   }

   // Things that look like object files (e.g. end in `.o`) may nevertheless be
   // archives, as noted in Note [Object merging] in GHC.Driver.Pipeline.Execute.
   if (isArchive(path)) {
       if (loadArchive_(path)) {
            return 1; // success
       } else {
            IF_DEBUG(linker,
                        debugBelch("tried and failed to load %" PATH_FMT " as an archive\n", path));
       }
   }

   ObjectCode *oc = preloadObjectFile(path);
   if (oc == NULL) return 0;

   if (! loadOc(oc)) {
       // failed; free everything we've allocated
       removeOcSymbols(oc);
       // no need to freeOcStablePtrs, they aren't created until resolveObjs()
       freeObjectCode(oc);
       return 0;
   }

   insertOCSectionIndices(oc);

   oc->next_loaded_object = loaded_objects;
   loaded_objects = oc;
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

   IF_DEBUG(linker, ocDebugBelch(oc, "start\n"));

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
       IF_DEBUG(linker, ocDebugBelch(oc, "ocVerifyImage_* failed\n"));
       return r;
   }

   /* Note [loadOc orderings]
      ~~~~~~~~~~~~~~~~~~~~~~~
      The order of `ocAllocateExtras` and `ocGetNames` matters. For MachO
      and ELF, `ocInit` and `ocGetNames` initialize a bunch of pointers based
      on the offset to `oc->image`, but `ocAllocateExtras` may relocate
      the address of `oc->image` and invalidate those pointers. So we must
      compute or recompute those pointers after `ocAllocateExtras`.

      On Windows, when we have an import library we (for now, as we don't honor
      the lazy loading semantics of the library and instead GHCi is already
      lazy) don't use the library after ocGetNames as it just populates the
      symbol table.  Allocating space for jump tables in ocAllocateExtras
      would just be a waste then as we'll be stopping further processing of the
      library in the next few steps. If necessary, the actual allocation
      happens in `ocGetNames_PEi386` simply set the correct pointers.
      */

#if defined(NEED_SYMBOL_EXTRAS)
#  if defined(OBJFORMAT_MACHO)
   r = ocAllocateExtras_MachO ( oc );
   if (!r) {
       IF_DEBUG(linker,
                ocDebugBelch(oc, "ocAllocateExtras_MachO failed\n"));
       return r;
   }
#  elif defined(OBJFORMAT_ELF)
   r = ocAllocateExtras_ELF ( oc );
   if (!r) {
       IF_DEBUG(linker,
                ocDebugBelch(oc, "ocAllocateExtras_ELF failed\n"));
       return r;
   }
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
       IF_DEBUG(linker, ocDebugBelch(oc, "ocGetNames_* failed\n"));
       return r;
   }

   /* Loaded, but not resolved yet, ensure the OC is in a consistent state.
      If a target has requested the ObjectCode not to be resolved then honor
      this requests.  Usually this means the ObjectCode has not been initialized
      and can't be. */
   if (oc->status != OBJECT_DONT_RESOLVE) {
       if (oc->archiveMemberName == NULL) {
           oc->status = OBJECT_NEEDED;
       } else {
           oc->status = OBJECT_LOADED;
       }
   }
   IF_DEBUG(linker, ocDebugBelch(oc, "done\n"));

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
    Symbol_t symbol;
    for (x = 0; x < oc->n_symbols; x++) {
        symbol = oc->symbols[x];
        if (   symbol.name
            && !ghciInsertSymbolTable(oc->fileName, symhash, symbol.name,
                                      symbol.addr,
                                      isSymbolWeak(oc, symbol.name),
                                      symbol.type, oc)) {
            return 0;
        }
    }

    IF_DEBUG(linker, ocDebugBelch(oc, "resolving\n"));
#   if defined(OBJFORMAT_ELF)
    r = ocResolve_ELF ( oc );
#   elif defined(OBJFORMAT_PEi386)
    r = ocResolve_PEi386 ( oc );
#   elif defined(OBJFORMAT_MACHO)
    r = ocResolve_MachO ( oc );
#   else
    barf("ocTryLoad: not implemented on this platform");
#   endif
    if (!r) {
        IF_DEBUG(linker, ocDebugBelch(oc, "resolution failed\n"));
        return r;
    }

    IF_DEBUG(linker, ocDebugBelch(oc, "protecting mappings\n"));
#if defined(NEED_SYMBOL_EXTRAS)
    ocProtectExtras(oc);
#endif

    // We have finished loading and relocating; flush the m32 allocators to
    // setup page protections.
#if defined(NEED_M32)
    m32_allocator_flush(oc->rx_m32);
    m32_allocator_flush(oc->rw_m32);
#endif

    IF_DEBUG(linker, ocDebugBelch(oc, "resolved\n"));
    oc->status = OBJECT_RESOLVED;

    return 1;
}

// run init/init_array/ctors/mod_init_func
int ocRunInit(ObjectCode *oc)
{
    if (oc->status != OBJECT_RESOLVED) {
        return 1;
    }

    IF_DEBUG(linker, ocDebugBelch(oc, "running initializers\n"));

    // See Note [Tracking foreign exports] in ForeignExports.c
    foreignExportsLoadingObject(oc);
    int r;
#if defined(OBJFORMAT_ELF)
    r = ocRunInit_ELF ( oc );
#elif defined(OBJFORMAT_PEi386)
    r = ocRunInit_PEi386 ( oc );
#elif defined(OBJFORMAT_MACHO)
    r = ocRunInit_MachO ( oc );
#else
    barf("ocTryLoad: initializers not implemented on this platform");
#endif
    foreignExportsFinishedLoadingObject();

    if (!r) { return r; }
    oc->status = OBJECT_READY;

    return 1;
}

int runPendingInitializers (void)
{
    for (ObjectCode *oc = objects; oc; oc = oc->next) {
        int r = ocRunInit(oc);
        if (!r) {
            errorBelch("Could not run initializers of Object Code %" PATH_FMT ".\n", OC_INFORMATIVE_FILENAME(oc));
            IF_DEBUG(linker, printLoadedObjects());
            fflush(stderr);
            return r;
        }
    }
    return 1;
}

/* -----------------------------------------------------------------------------
 * resolve all the currently unlinked objects in memory
 *
 * Returns: 1 if ok, 0 on error.
 */
static HsInt resolveObjs_ (void)
{
    IF_DEBUG(linker, debugBelch("resolveObjs: start\n"));

    for (ObjectCode *oc = objects; oc; oc = oc->next) {
        int r = ocTryLoad(oc);
        if (!r) {
            errorBelch("Could not load Object Code %" PATH_FMT ".\n", OC_INFORMATIVE_FILENAME(oc));
            IF_DEBUG(linker, printLoadedObjects());
            fflush(stderr);
            return r;
        }
    }

    if (!runPendingInitializers()) {
        return 0;
    }

#if defined(PROFILING)
    // collect any new cost centres & CCSs that were defined during runInit
    refreshProfilingCCSs();
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
    ASSERT(symhash != NULL);
    ASSERT(objects != NULL);

    IF_DEBUG(linker, debugBelch("unloadObj: %" PATH_FMT "\n", path));

    bool unloadedAnyObj = false;
    ObjectCode *prev = NULL;
    // NOTE (osa): There may be more than one object with the same file name
    // (happens when loading archive files) so we don't stop after unloading one
    for (ObjectCode *oc = loaded_objects; oc; oc = oc->next_loaded_object) {
        if (pathcmp(oc->fileName,path) == 0) {
            oc->status = OBJECT_UNLOADED;

            // These are both idempotent, so in just_purge mode we can later
            // call unloadObj() to really unload the object.
            removeOcSymbols(oc);
            freeOcStablePtrs(oc);

            unloadedAnyObj = true;

            if (!just_purge) {
                n_unloaded_objects += 1;
                // Remove object code from root set
                if (prev == NULL) {
                    loaded_objects = oc->next_loaded_object;
                } else {
                    prev->next_loaded_object = oc->next_loaded_object;
                }
            }
        } else {
            prev = oc;
        }
    }

    if (unloadedAnyObj) {
        return 1;
    } else {
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

OStatus getObjectLoadStatus_ (pathchar *path)
{
    for (ObjectCode *o = objects; o; o = o->next) {
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
            void* start, StgWord size,
            StgWord mapped_offset, void* mapped_start, StgWord mapped_size)
{
   s->start        = start;     /* actual start of section in memory */
   s->size         = size;      /* actual size of section in memory */
   s->kind         = kind;
   s->alloc        = alloc;
   s->mapped_offset = mapped_offset; /* offset from the image of mapped_start */

   s->mapped_start = mapped_start; /* start of mmap() block */
   s->mapped_size  = mapped_size;  /* size of mmap() block */

   if (!s->info)
     s->info
       = (struct SectionFormatInfo*)stgCallocBytes(1, sizeof *s->info,
                                            "addSection(SectionFormatInfo)");

   IF_DEBUG(linker,
            debugBelch("addSection: %p-%p (size %" FMT_Word "), kind %d\n",
                       start, (void*)((StgWord)start + size),
                       size, kind ));
}

#define UNUSED(x) (void)(x)

#if defined(OBJFORMAT_ELF)
void * loadNativeObj (pathchar *path, char **errmsg)
{
   ACQUIRE_LOCK(&linker_mutex);
   void *r = loadNativeObj_ELF(path, errmsg);
   RELEASE_LOCK(&linker_mutex);
   return r;
}
#else
void * STG_NORETURN
loadNativeObj (pathchar *path, char **errmsg)
{
   UNUSED(path);
   UNUSED(errmsg);
   barf("loadNativeObj: not implemented on this platform");
}
#endif

HsInt unloadNativeObj (void *handle)
{
    bool unloadedAnyObj = false;

    IF_DEBUG(linker, debugBelch("unloadNativeObj: %p\n", handle));

    ObjectCode *prev = NULL, *next;
    for (ObjectCode *nc = loaded_objects; nc; nc = next) {
        next = nc->next_loaded_object; // we might move nc

        if (nc->type == DYNAMIC_OBJECT && nc->dlopen_handle == handle) {
            nc->status = OBJECT_UNLOADED;
            n_unloaded_objects += 1;

            // dynamic objects have no symbols
            CHECK(nc->symbols == NULL);
            freeOcStablePtrs(nc);

            // Remove object code from root set
            if (prev == NULL) {
              loaded_objects = nc->next_loaded_object;
            } else {
              prev->next_loaded_object = nc->next_loaded_object;
            }
            unloadedAnyObj = true;
        } else {
            prev = nc;
        }
    }

    if (unloadedAnyObj) {
        return 1;
    } else {
        errorBelch("unloadObjNativeObj_ELF: can't find `%p' to unload", handle);
        return 0;
    }
}

/* -----------------------------------------------------------------------------
 * Segment management
 */
void
initSegment (Segment *s, void *start, size_t size, SegmentProt prot, int n_sections)
{
    s->start = start;
    s->size = size;
    s->prot = prot;
    s->sections_idx = (int *)stgCallocBytes(n_sections, sizeof(int),
                                               "initSegment(segment)");
    s->n_sections = n_sections;
}

void freeSegments (ObjectCode *oc)
{
    if (oc->segments != NULL) {
        IF_DEBUG(linker, ocDebugBelch(oc, "freeing %d segments\n", oc->n_segments));

        for (int i = 0; i < oc->n_segments; i++) {
            Segment *s = &oc->segments[i];

            IF_DEBUG(linker, ocDebugBelch(oc, "freeing segment %d at %p size %zu\n",
                                          i, s->start, s->size));

            stgFree(s->sections_idx);
            s->sections_idx = NULL;

            if (0 == s->size) {
                IF_DEBUG(linker, ocDebugBelch(oc, "skipping segment of 0 size\n"));
                continue;
            } else {
#if RTS_LINKER_USE_MMAP
                munmapForLinker(s->start, s->size, "freeSegments");
#else
                stgFree(s->start);
#endif
            }
            s->start = NULL;
        }

        stgFree(oc->segments);
        oc->segments = NULL;
    }
}
