# On Windows, ensure that the paths to all toolchain components passed to
# Hadrian via hadrian/cfg/system.config are Windows-style paths rather than
# msys paths.
AC_DEFUN([FP_NORMALISE_PATH],[
  if test -n "$$1"; then
    dnl use mixed (-m) mode to get C:/mingw64/... with forward slashes.
    dnl windows (-w) mode will give us C:\... and mess with escaping.
    $1=`cygpath -m $$1`
  fi
])

AC_DEFUN([FP_NORMALISE_WINDOWS_PATHS],[
  if test "$HostOS" = "mingw32"; then
    FP_NORMALISE_PATH(AlexCmd)
    FP_NORMALISE_PATH(ArCmd)
    FP_NORMALISE_PATH(AutoreconfCmd)
    FP_NORMALISE_PATH(CC)
    FP_NORMALISE_PATH(HappyCmd)
    FP_NORMALISE_PATH(HaskellCPPCmd)
    FP_NORMALISE_PATH(LdCmd)
    FP_NORMALISE_PATH(MakeCmd)
    FP_NORMALISE_PATH(NmCmd)
    FP_NORMALISE_PATH(MergeObjsCmd)
    FP_NORMALISE_PATH(LD_STAGE0)
    FP_NORMALISE_PATH(ObjdumpCmd)
    FP_NORMALISE_PATH(REAL_RANLIB_CMD)
    FP_NORMALISE_PATH(SPHINXBUILD)
    FP_NORMALISE_PATH(AR_STAGE0)
    FP_NORMALISE_PATH(CC_STAGE0)
    FP_NORMALISE_PATH(WithGhc)
    FP_NORMALISE_PATH(GhcPkgCmd)
    FP_NORMALISE_PATH(TarCmd)
    FP_NORMALISE_PATH(PatchCmd)
    FP_NORMALISE_PATH(XELATEX)
    FP_NORMALISE_PATH(MAKEINDEX)
    FP_NORMALISE_PATH(MAKEINFO)
    FP_NORMALISE_PATH(SH)
    FP_NORMALISE_PATH(GIT)
    FP_NORMALISE_PATH(CABAL)
    FP_NORMALISE_PATH(PythonCmd)
  fi
])
