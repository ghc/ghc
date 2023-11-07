# FP_LD_NO_WARN_DUPLICATE_LIBRARIES
# ---------------------------------
# XCode 15 introduced a new linker which warns on duplicate libraries being
# linked. To disable this warning, we pass -Wl,-no_warn_duplicate_libraries as
# suggested by Brad King in CMake issue #25297.
#
# This flag isn't necessarily available to other linkers on darwin, so we must
# only configure it into the CC linker arguments if valid.
#
# $1 = the platform
# $2 = the name of the linker flags variable when linking with $CC
AC_DEFUN([FP_LD_NO_WARN_DUPLICATE_LIBRARIES], [
    case $$1 in
      *-darwin)
      AC_MSG_CHECKING([whether the linker requires -no_warn_duplicate_libraries])
      echo 'int main(void) {return 0;}' > conftest.c
      if $CC -o conftest -Wl,-no_warn_duplicate_libraries conftest.c > /dev/null 2>&1
      then
          $2="$$2 -Wl,-no_warn_duplicate_libraries"
          AC_MSG_RESULT([yes])
      else
          AC_MSG_RESULT([no])
      fi
      rm -f conftest.c conftest.o conftest
      ;;

    esac
])

