#include "Rts.h"
#include "RtsUtils.h"
#include "RtsSymbols.h"
#include "ForeignExports.h"
#include "LinkerInternals.h"
#include "StablePtr.h"

#include <dlfcn.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#ifndef MAP_ANONYMOUS
#ifdef MAP_ANON
#define MAP_ANONYMOUS MAP_ANON
#endif
#endif

void* ghci_mmap_exec(size_t size, int use_low32)
{
  int flags = MAP_PRIVATE | MAP_ANONYMOUS;
#ifdef MAP_32BIT
  if (use_low32) {
    flags |= MAP_32BIT;
  }
#else
  (void)use_low32;
#endif
  void* p = mmap(NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC, flags, -1, 0);
  if (p == MAP_FAILED) {
    return NULL;
  }
  return p;
}

void ghci_munmap(void* addr, size_t size)
{
  if (addr != NULL && size != 0) {
    munmap(addr, size);
  }
}

long ghci_page_size(void)
{
  long sz = sysconf(_SC_PAGESIZE);
  return sz > 0 ? sz : 4096;
}

void* ghci_dlopen(const char* path, int flags)
{
  return dlopen(path, flags);
}

void* ghci_dlsym(void* handle, const char* sym)
{
  return dlsym(handle, sym);
}

void* ghci_dlsym_default(const char* sym)
{
  return dlsym(RTLD_DEFAULT, sym);
}

int ghci_dlclose(void* handle)
{
  return dlclose(handle);
}

const char* ghci_dlerror(void)
{
  return dlerror();
}

void ghci_getProgEnvv(int *out_envc, char **out_envv[])
{
  int envc = 0;
  extern char **environ;
  while (environ[envc] != NULL) {
    envc++;
  }
  *out_envc = envc;
  *out_envv = environ;
}

void ghci_freeProgEnvv(int envc, char *envv[])
{
  (void)envc;
  (void)envv;
}

int ghci_rts_syms_count(void)
{
  int n = 0;
  while (rtsSyms[n].lbl != NULL) {
    n++;
  }
  return n;
}

const char* ghci_rts_sym_name(int idx)
{
  return rtsSyms[idx].lbl;
}

void* ghci_rts_sym_addr(int idx)
{
  return rtsSyms[idx].addr;
}

int ghci_rts_sym_strength(int idx)
{
  return (int) rtsSyms[idx].strength;
}

int ghci_rts_sym_type(int idx)
{
  return (int) rtsSyms[idx].type;
}

static RtsSymbolVal* extra_syms(void)
{
  if (rtsExtraSyms == NULL) {
    return NULL;
  }
  return rtsExtraSyms();
}

int ghci_rts_extra_syms_count(void)
{
  RtsSymbolVal* syms = extra_syms();
  if (syms == NULL) return 0;
  int n = 0;
  while (syms[n].lbl != NULL) {
    n++;
  }
  return n;
}

const char* ghci_rts_extra_sym_name(int idx)
{
  RtsSymbolVal* syms = extra_syms();
  if (syms == NULL) return NULL;
  return syms[idx].lbl;
}

void* ghci_rts_extra_sym_addr(int idx)
{
  RtsSymbolVal* syms = extra_syms();
  if (syms == NULL) return NULL;
  return syms[idx].addr;
}

int ghci_rts_extra_sym_strength(int idx)
{
  RtsSymbolVal* syms = extra_syms();
  if (syms == NULL) return 0;
  return (int) syms[idx].strength;
}

int ghci_rts_extra_sym_type(int idx)
{
  RtsSymbolVal* syms = extra_syms();
  if (syms == NULL) return 0;
  return (int) syms[idx].type;
}

void* ghci_new_object_code(void)
{
  ObjectCode* oc = (ObjectCode*) calloc(1, sizeof(ObjectCode));
  return oc;
}

void ghci_free_object_code(void* oc)
{
  if (oc != NULL) {
    free(oc);
  }
}

void ghci_free_object_code_exports(void* oc_void)
{
  if (oc_void == NULL) return;
  ObjectCode* oc = (ObjectCode*) oc_void;
  struct ForeignExportsList* exports = oc->foreign_exports;
  while (exports != NULL) {
    if (exports->stable_ptrs != NULL) {
      for (int i = 0; i < exports->n_entries; i++) {
        hs_free_stable_ptr((HsStablePtr) exports->stable_ptrs[i]);
      }
      free(exports->stable_ptrs);
      exports->stable_ptrs = NULL;
    }
    exports = exports->next;
  }
  oc->foreign_exports = NULL;
}

// C wrappers matching rts/Linker.h API, delegating to Haskell exports.

extern void hs_initLinker(void);
extern void hs_initLinker_(HsInt32 retain_cafs);
extern HsInt32 hs_loadObj(HsPtr path);
extern HsInt32 hs_loadArchive(HsPtr path);
extern HsInt32 hs_resolveObjs(void);
extern HsInt32 hs_unloadObj(HsPtr path);
extern HsInt32 hs_purgeObj(HsPtr path);
extern HsPtr hs_lookupSymbol(HsPtr name);
extern HsPtr hs_lookupSymbolInNativeObj(HsPtr handle, HsPtr name);
extern HsPtr hs_loadNativeObj(HsPtr path, HsPtr errmsg);
extern HsInt32 hs_unloadNativeObj(HsPtr handle);
extern HsPtr hs_addDLL(HsPtr dll_name);
extern HsInt32 hs_getObjectLoadStatus(HsPtr path);

void initLinker(void)
{
  hs_initLinker();
}

void initLinker_(int retain_cafs)
{
  hs_initLinker_((HsInt32) retain_cafs);
}

HsInt loadObj(pathchar *path)
{
  return (HsInt) hs_loadObj((HsPtr) path);
}

HsInt loadArchive(pathchar *path)
{
  return (HsInt) hs_loadArchive((HsPtr) path);
}

HsInt resolveObjs(void)
{
  return (HsInt) hs_resolveObjs();
}

HsInt unloadObj(pathchar *path)
{
  return (HsInt) hs_unloadObj((HsPtr) path);
}

HsInt purgeObj(pathchar *path)
{
  return (HsInt) hs_purgeObj((HsPtr) path);
}

void *lookupSymbol(char *lbl)
{
  return (void *) hs_lookupSymbol((HsPtr) lbl);
}

void *lookupSymbolInNativeObj(void *handle, const char *symbol_name)
{
  return (void *) hs_lookupSymbolInNativeObj((HsPtr) handle, (HsPtr) symbol_name);
}

void *loadNativeObj(pathchar *path, char **errmsg)
{
  return (void *) hs_loadNativeObj((HsPtr) path, (HsPtr) errmsg);
}

HsInt unloadNativeObj(void *handle)
{
  return (HsInt) hs_unloadNativeObj((HsPtr) handle);
}

const char *addDLL(pathchar *dll_name)
{
  return (const char *) hs_addDLL((HsPtr) dll_name);
}

OStatus getObjectLoadStatus(pathchar *path)
{
  return (OStatus) hs_getObjectLoadStatus((HsPtr) path);
}
