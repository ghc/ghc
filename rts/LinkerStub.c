#include "Rts.h"
#include "LinkerInternals.h"
#include "CheckUnload.h"
#include "RtsUtils.h"
#include "Hash.h"
#include "RtsFlags.h"
#include "linker/MMap.h"

#include <string.h>

// Provide the linker mutex used by various RTS subsystems.
#if defined(THREADED_RTS)
Mutex linker_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

#if defined(__GNUC__)
#define GHCI_EXPORT __attribute__((visibility("default")))
#else
#define GHCI_EXPORT
#endif

GHCI_EXPORT void ghci_init_unload_check(void);
GHCI_EXPORT void* ghci_register_object(const char* path,
                                       int n_sections,
                                       const void** starts,
                                       const size_t* sizes,
                                       const int* kinds,
                                       void* image,
                                       size_t image_size);
GHCI_EXPORT void ghci_set_object_status(void* oc_void, int status);
GHCI_EXPORT void ghci_unload_object(void* oc_void, int just_purge);
GHCI_EXPORT int ghci_get_object_status(const char* path);
GHCI_EXPORT int ghci_linker_optimistic(void);
GHCI_EXPORT void ghci_report_missing_symbol(const char* sym, int optimistic);

// The RTS object linker is implemented in Haskell (libraries/ghci).
// These are minimal stubs to satisfy RTS references now that the C linker
// is removed from the build.

void exitLinker(void)
{
  // No-op: linker teardown is handled in Haskell.
}

void freeObjectCode(ObjectCode *oc)
{
  if (oc == NULL) {
    return;
  }

  if (oc->imageMapped && oc->image != NULL && oc->fileSize > 0) {
    munmapForLinker(oc->image, (size_t) oc->fileSize, "freeObjectCode");
  }

  if (oc->sections != NULL) {
    stgFree(oc->sections);
    oc->sections = NULL;
  }

  if (oc->dependencies != NULL) {
    freeHashSet(oc->dependencies);
    oc->dependencies = NULL;
  }

  stgFree(oc->fileName);
  stgFree(oc->archiveMemberName);
  stgFree(oc);
}

HsInt insertSymbol(pathchar *obj_name, char *key, void *data)
{
  (void)obj_name;
  (void)key;
  (void)data;
  return 0;
}

// --------------------------------------------------------------------------
// GHCi linker support exported from RTS for the Haskell-based linker.
// --------------------------------------------------------------------------

GHCI_EXPORT void ghci_init_unload_check(void)
{
  initUnloadCheck();
}

GHCI_EXPORT void* ghci_register_object(const char* path,
                                       int n_sections,
                                       const void** starts,
                                       const size_t* sizes,
                                       const int* kinds,
                                       void* image,
                                       size_t image_size)
{
  ObjectCode* oc = (ObjectCode*) stgCallocBytes(1, sizeof(ObjectCode),
                                                "ghci_register_object");
  if (oc == NULL) {
    return NULL;
  }

  size_t len = strlen(path);
  oc->fileName = (pathchar*) stgMallocBytes(len + 1, "ghci_register_object filename");
  memcpy(oc->fileName, path, len + 1);

  oc->status = OBJECT_LOADED;
  oc->type = STATIC_OBJECT;
  oc->archiveMemberName = NULL;
  oc->symbols = NULL;
  oc->n_symbols = 0;
  oc->image = (char*) image;
  oc->fileSize = (int) image_size;
  oc->imageMapped = 1;
  oc->misalignment = 0;
  oc->cxa_finalize = NULL;
  oc->extraInfos = NULL;
  oc->info = NULL;
  oc->n_segments = 0;
  oc->segments = NULL;
  oc->mark = object_code_mark_bit;
  oc->unloadable = true;
  oc->dependencies = allocHashSet();
  oc->foreign_exports = NULL;
  oc->next = NULL;
  oc->prev = NULL;
  oc->next_loaded_object = NULL;

  if (n_sections > 0) {
    oc->sections = (Section*) stgCallocBytes((size_t) n_sections, sizeof(Section),
                                             "ghci_register_object sections");
    oc->n_sections = n_sections;
    for (int i = 0; i < n_sections; i++) {
      oc->sections[i].start = (void*) starts[i];
      oc->sections[i].size = (StgWord) sizes[i];
      oc->sections[i].kind = (SectionKind) kinds[i];
      oc->sections[i].alloc = SECTION_NOMEM;
      oc->sections[i].mapped_offset = 0;
      oc->sections[i].mapped_start = NULL;
      oc->sections[i].mapped_size = 0;
      oc->sections[i].info = NULL;
    }
  } else {
    oc->sections = NULL;
    oc->n_sections = 0;
  }

  ACQUIRE_LOCK(&linker_mutex);
  insertOCSectionIndices(oc);
  oc->next_loaded_object = loaded_objects;
  loaded_objects = oc;
  RELEASE_LOCK(&linker_mutex);

  return oc;
}

GHCI_EXPORT void ghci_set_object_status(void* oc_void, int status)
{
  ObjectCode* oc = (ObjectCode*) oc_void;
  if (oc != NULL) {
    oc->status = (OStatus) status;
  }
}

// Mark an object as unloaded; optionally keep it in the loaded_objects list.
GHCI_EXPORT void ghci_unload_object(void* oc_void, int just_purge)
{
  ObjectCode* oc = (ObjectCode*) oc_void;
  if (oc == NULL) {
    return;
  }

  ACQUIRE_LOCK(&linker_mutex);
  oc->status = OBJECT_UNLOADED;

  if (!just_purge) {
    ObjectCode* prev = NULL;
    for (ObjectCode* cur = loaded_objects; cur != NULL; cur = cur->next_loaded_object) {
      if (cur == oc) {
        if (prev == NULL) {
          loaded_objects = cur->next_loaded_object;
        } else {
          prev->next_loaded_object = cur->next_loaded_object;
        }
        break;
      }
      prev = cur;
    }
    n_unloaded_objects += 1;
  }
  RELEASE_LOCK(&linker_mutex);
}

GHCI_EXPORT int ghci_get_object_status(const char* path)
{
  int status = (int) OBJECT_NOT_LOADED;
  ACQUIRE_LOCK(&linker_mutex);
  for (ObjectCode* oc = objects; oc != NULL; oc = oc->next) {
    if (oc->fileName != NULL && strcmp((const char*) oc->fileName, path) == 0) {
      status = (int) oc->status;
      break;
    }
  }
  RELEASE_LOCK(&linker_mutex);
  return status;
}

GHCI_EXPORT int ghci_linker_optimistic(void)
{
  return RtsFlags.MiscFlags.linkerOptimistic ? 1 : 0;
}

GHCI_EXPORT void ghci_report_missing_symbol(const char* sym, int optimistic)
{
  if (optimistic) {
    errorBelch("^^ Could not load '%s', dependency unresolved, optimistically continuing\n",
               sym);
  } else {
    errorBelch("^^ Could not load '%s', dependency unresolved. See top entry above. "
               "You might consider using --optimistic-linking\n",
               sym);
  }
}
