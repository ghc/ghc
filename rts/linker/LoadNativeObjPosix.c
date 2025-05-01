#include "LinkerInternals.h"
#include "Rts.h"

#if defined(OBJFORMAT_ELF) || defined(OBJFORMAT_MACHO)

#include "CheckUnload.h"
#include "ForeignExports.h"
#include "RtsUtils.h"
#include "Profiling.h"

#include "linker/LoadNativeObjPosix.h"

#if defined(HAVE_DLFCN_H)
#include <dlfcn.h>
#endif

#if defined(HAVE_DLINFO)
#include <link.h>
#endif

#include <string.h>

/*
 * Shared object loading
 */

#if defined(HAVE_DLINFO)
struct piterate_cb_info {
  ObjectCode *nc;
  void *l_addr;   /* base virtual address of the loaded code */
};

static int loadNativeObjCb_(struct dl_phdr_info *info,
    size_t _size STG_UNUSED, void *data) {
  struct piterate_cb_info *s = (struct piterate_cb_info *) data;

  // This logic mimicks _dl_addr_inside_object from glibc
  // For reference:
  // int
  // internal_function
  // _dl_addr_inside_object (struct link_map *l, const ElfW(Addr) addr)
  // {
  //   int n = l->l_phnum;
  //   const ElfW(Addr) reladdr = addr - l->l_addr;
  //
  //   while (--n >= 0)
  //     if (l->l_phdr[n].p_type == PT_LOAD
  //         && reladdr - l->l_phdr[n].p_vaddr >= 0
  //         && reladdr - l->l_phdr[n].p_vaddr < l->l_phdr[n].p_memsz)
  //       return 1;
  //   return 0;
  // }

  if ((void*) info->dlpi_addr == s->l_addr) {
    int n = info->dlpi_phnum;
    while (--n >= 0) {
      if (info->dlpi_phdr[n].p_type == PT_LOAD) {
        NativeCodeRange* ncr =
          stgMallocBytes(sizeof(NativeCodeRange), "loadNativeObjCb_");
        ncr->start = (void*) ((char*) s->l_addr + info->dlpi_phdr[n].p_vaddr);
        ncr->end = (void*) ((char*) ncr->start + info->dlpi_phdr[n].p_memsz);

        ncr->next = s->nc->nc_ranges;
        s->nc->nc_ranges = ncr;
      }
    }
  }
  return 0;
}
#endif /* defined(HAVE_DLINFO) */

static void copyErrmsg(char** errmsg_dest, char* errmsg) {
  if (errmsg == NULL) errmsg = "loadNativeObj_POSIX: unknown error";
  *errmsg_dest = stgMallocBytes(strlen(errmsg)+1, "loadNativeObj_POSIX");
  strcpy(*errmsg_dest, errmsg);
}

void freeNativeCode_POSIX (ObjectCode *nc) {
  ASSERT_LOCK_HELD(&linker_mutex);

  dlclose(nc->dlopen_handle);

  NativeCodeRange *ncr = nc->nc_ranges;
  while (ncr) {
    NativeCodeRange* last_ncr = ncr;
    ncr = ncr->next;
    stgFree(last_ncr);
  }
}

/*
 * Note [Don't fail due to RTLD_NOW]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * If possible we want to load dynamic objects immediately (e.g. using
 * RTLD_NOW) so that we can query their mappings and therefore be able to
 * safely unload them. However, there are some cases where an object cannot be
 * successfully eagerly loaded yet execution can nevertheless succeed with lazy
 * binding.
 *
 * One such instance was found in #25943, where a library referenced undefined
 * symbols. While this pattern is quite dodgy (really, these symbol references
 * should be weakly bound in the library), previous GHC versions accepted such
 * programs. Moreover, it is important that we are able to load such libraries
 * since GHC insists on loading all package dependencies when, e.g., evaluating
 * TemplateHaskell splices.
 *
 * To ensure that we don't fail to load such programs, we first attempt loading
 * with RTLD_NOW and, if this fails, attempt to load again with lazy binding
 * (taking care to mark the object as not unloadable in this case).
 */

void * loadNativeObj_POSIX (pathchar *path, char **errmsg)
{
   ObjectCode* nc;
   void *hdl, *retval;

   ASSERT_LOCK_HELD(&linker_mutex);

   IF_DEBUG(linker, debugBelch("loadNativeObj_POSIX %" PATH_FMT "\n", path));

   retval = NULL;

   /* If we load the same object multiple times, just return the
    * already-loaded handle. Note that this is broken if unloadNativeObj
    * is used, as we don’t do any reference counting; see #24345.
    */
   ObjectCode *existing_oc = lookupObjectByPath(path);
   if (existing_oc && existing_oc->status != OBJECT_UNLOADED) {
     if (existing_oc->type == DYNAMIC_OBJECT) {
       retval = existing_oc->dlopen_handle;
       goto success;
     }
     copyErrmsg(errmsg, "loadNativeObj_POSIX: already loaded as non-dynamic object");
     goto dlopen_fail;
   }

   nc = mkOc(DYNAMIC_OBJECT, path, NULL, 0, false, NULL, 0);

   // If we HAVE_DLINFO, we use RTLD_NOW rather than RTLD_LAZY because we want
   // to learn eagerly about all external functions. Otherwise, there is no
   // additional advantage to being eager, so it is better to be lazy and only
   // bind functions when needed for better performance.
   //
   // Moreover, it is possible that loading will fail (e.g. if the library
   // being loaded depends upon symbols from a library which is not available);
   // in this case we will retry loading with load_now=false. See
   // Note [Don't fail due to RTLD_NOW]..
   bool load_now;
#if defined(HAVE_DLINFO)
   load_now = true;
#else
   load_now = false;
#endif

try_again:
   foreignExportsLoadingObject(nc);

   // When dlopen() loads a profiled dynamic library, it calls the ctors which
   // will call registerCcsList() to append the defined CostCentreStacks to
   // CCS_LIST. However, another thread may be doing other things with the RTS
   // linker that transitively calls refreshProfilingCCSs() which also accesses
   // CCS_LIST. So there's a risk of data race that may lead to segfaults
   // (#24423), and we need to ensure the ctors are also protected by
   // ccs_mutex.
#if defined(PROFILING)
   ACQUIRE_LOCK(&ccs_mutex);
#endif

   const int dlopen_mode = load_now ? RTLD_NOW : RTLD_LAZY;
   hdl = dlopen(path, dlopen_mode|RTLD_LOCAL); /* see Note [RTLD_LOCAL] */
   nc->dlopen_handle = hdl;
   nc->status = OBJECT_READY;

#if defined(PROFILING)
   RELEASE_LOCK(&ccs_mutex);
#endif

   foreignExportsFinishedLoadingObject();

   if (hdl == NULL) {
     if (load_now) {
       // See Note [Don't fail due to RTLD_NOW]
       load_now = false;
       goto try_again;
     } else {
       /* dlopen failed; save the message in errmsg */
       copyErrmsg(errmsg, dlerror());
       goto dlopen_fail;
     }
   }

#if defined(HAVE_DLINFO)
   if (load_now) {
     struct link_map *map;
     if (dlinfo(hdl, RTLD_DI_LINKMAP, &map) == -1) {
       /* dlinfo failed; save the message in errmsg */
       copyErrmsg(errmsg, dlerror());
       goto dlinfo_fail;
     }

     hdl = NULL; // pass handle ownership to nc

     struct piterate_cb_info piterate_info = {
       .nc = nc,
       .l_addr = (void *) map->l_addr
     };
     dl_iterate_phdr(loadNativeObjCb_, &piterate_info);
     if (!nc->nc_ranges) {
       copyErrmsg(errmsg, "dl_iterate_phdr failed to find obj");
       goto dl_iterate_phdr_fail;
     }
     nc->unloadable = true;
   } else {
     nc->nc_ranges = NULL;
     nc->unloadable = false;
   }
#else
   nc->nc_ranges = NULL;
   nc->unloadable = false;
#endif /* defined (HAVE_DLINFO) */

   insertOCSectionIndices(nc);

   nc->next_loaded_object = loaded_objects;
   loaded_objects = nc;

   retval = nc->dlopen_handle;

#if defined(PROFILING)
  // collect any new cost centres that were defined in the loaded object.
  refreshProfilingCCSs();
#endif

   goto success;

#if defined(HAVE_DLINFO)
dl_iterate_phdr_fail:
#endif
   freeNativeCode_POSIX(nc);
#if defined(HAVE_DLINFO)
dlinfo_fail:
#endif
   if (hdl) dlclose(hdl);
dlopen_fail:
success:

   IF_DEBUG(linker, debugBelch("loadNativeObj_POSIX result=%p\n", retval));

   return retval;
}

#endif /* elf + macho */
