# GHC_TABLES_NEXT_TO_CODE
# --------------------------------
# Do a build with tables next to code?
#
# Whether the target architecture supports placing info tables
# directly before the entry code (see TABLES_NEXT_TO_CODE in the RTS).
# Whether we actually compile for TABLES_NEXT_TO_CODE depends on
# whether we're building unregisterised code or not, which may be
# decided by options to the compiler later.
#
# See https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects#tables_next_to_code
#
AC_DEFUN([GHC_TABLES_NEXT_TO_CODE],
[
  AC_REQUIRE([GHC_UNREGISTERISED])
  AC_MSG_CHECKING(whether target supports tables next to code)
  case "$Unregisterised" in
      NO)
          case "$TargetArch" in
              ia64|powerpc64|powerpc64le|s390x|wasm32)
                  TablesNextToCodeDefault=NO
                  AC_MSG_RESULT([no])
                  ;;
              *)
                  if test "$TargetOS" = "mingw32" && test "$TargetArch" = "aarch64"; then
                    TablesNextToCodeDefault=NO
                    AC_MSG_RESULT([no])
                  else
                    TablesNextToCodeDefault=YES
                    AC_MSG_RESULT([yes])
                  fi
                  ;;
          esac
          ;;
      YES)
          TablesNextToCodeDefault=NO
          AC_MSG_RESULT([no])
          ;;
  esac
  FP_DEFAULT_CHOICE_OVERRIDE_CHECK(
    [tables-next-to-code],
    [tables next to code],
    [tables apart from code],
    [TablesNextToCode],
    [Build a tool chain with info tables laid out next to code (enabled by default when using the registerised ABI, on platforms that support it)])
])
