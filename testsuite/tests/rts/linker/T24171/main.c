#include "Rts.h"

#if defined(mingw32_HOST_OS)
#define PATH_STR(str) L##str
#else
#define PATH_STR(str) str
#endif

int main(int argc, char *argv[])
{
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  conf.rts_opts = "-hm -hbvoid --no-automatic-heap-samples";
  hs_init_ghc(&argc, &argv, conf);

  initLinker_(0);

  int ok;
  ok = loadArchive(PATH_STR("Lib.a"));
  if (!ok) {
    errorBelch("loadArchive(Lib.a) failed");
    return 1;
  }
  ok = resolveObjs();
  if (!ok) {
    errorBelch("resolveObjs() failed");
    return 1;
  }

  StgInt (*chooseInt)(StgBool) = lookupSymbol("chooseInt");
  if (!chooseInt) {
    errorBelch("lookupSymbol(chooseInt) failed");
    return 1;
  }

  printf("chooseInt(0) = %" FMT_Int "\n", chooseInt(0));

  requestHeapCensus();
  performMajorGC();

  ok = resolveObjs();
  if (!ok) {
    errorBelch("resolveObjs() failed");
    return 1;
  }
  ok = unloadObj(PATH_STR("Lib.a"));
  if (!ok) {
    errorBelch("unloadObj(Lib.a) failed");
    return 1;
  }

  requestHeapCensus();
  performMajorGC();

  hs_exit();
  return 0;
}
