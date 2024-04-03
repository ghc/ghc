#include "Rts.h"

#if defined(mingw32_HOST_OS)
#define PATH_STR(str) L##str
#else
#define PATH_STR(str) str
#endif

typedef void (*hello_t)();

int main(int argc, char *argv[])
{
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&argc, &argv, conf);

  initLinker_(0);

  int ok;
  char *errmsg;
  void *obj = loadNativeObj("./libobj.so", &errmsg);
  if (!obj) {
     barf("loadNativeObj failed: %s", errmsg);
  }

  hello_t sym;
  const char* lbl;

#if defined(darwin_HOST_OS)
  // mach-o symbols are prefixed with _
  lbl = "_hello";
#else
  lbl = "hello";
#endif

  sym = lookupSymbolInNativeObj(obj, lbl);
  if (sym == NULL) {
     barf("lookupSymbolInNativeObj failed unexpectedly");
  }
  sym();

#if defined(darwin_HOST_OS)
  lbl = "_hello_world";
#else
  lbl = "hello_world";
#endif

  sym = lookupSymbolInNativeObj(obj, lbl);
  if (sym != NULL) {
     barf("lookupSymbolInNativeObj succeeded unexpectedly");
  }

  return 0;
}
