# FP_PROG_LD_SUPPORTS_DOT_O_ARCHIVES
# ----------------------------------
# Sets fp_prog_ar_supports_dot_o_archives to yes or no, depending on whether
# the linker accepts archives named with a .o suffix.
# On Darwin the ld64 linker does not support archives with a .o suffix
# therefore we cannot use ar -L, even if ar supports it. See #23188.
AC_DEFUN([FP_PROG_LD_SUPPORTS_DOT_O_ARCHIVES],
[
  AC_CACHE_CHECK([whether $fp_prog_ld supports .o archives], [fp_cv_prog_ld_supports_dot_o_archives],
    [
      rm -f conftest*
      touch conftest.file
      echo 'int a = 42;' > conftest-a.c
      echo 'int b = 314;' > conftest-b.c
      cat >conftest-main.c <<EOF
#include <stdio.h>
extern int a, b;
int main(int argc, char** argv) {
    printf("%d %d\n", a, b);
    return 0;
}
EOF

      "$CC" -c conftest-a.c
      "$CC" -c conftest-b.c
      "$CC" -c conftest-main.c
      "$fp_prog_ar" qc conftest-lib.o conftest-a.o conftest-b.o
      if "$CC" -o conftest conftest-main.o conftest-lib.o >/dev/null
      then
        AC_MSG_RESULT([yes])
        fp_cv_prog_ar_supports_dot_o_archives=yes
      else
        AC_MSG_RESULT([no])
        fp_cv_prog_ar_supports_dot_o_archives=no
      fi
      rm -f conftest*
    ])
])
