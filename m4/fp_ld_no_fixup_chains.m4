# FP_LD_NO_FIXUP_CHAINS
# --------------------
# See if whether we are using a version of ld64 on darwin platforms which
# requires us to pass -no_fixup_chains
#
# $1 = the platform
# $2 = the name of the linker flags variable when linking with $CC
AC_DEFUN([FP_LD_NO_FIXUP_CHAINS], [
    case $$1 in
      *-darwin)
      AC_MSG_CHECKING([whether ld64 requires -no_fixup_chains])
      echo 'int main(void) {return 0;}' > conftest.c
      if $CC -o conftest.o -Wl,-no_fixup_chains conftest.c > /dev/null 2>&1
      then
          $2="$$2 -Wl,-no_fixup_chains"
          AC_MSG_RESULT([yes])
      else
          AC_MSG_RESULT([no])
      fi
      rm -f conftest.c conftest.o
      ;;

    esac
])
