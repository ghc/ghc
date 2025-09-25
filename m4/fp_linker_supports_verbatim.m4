# Checks whether the linker supports '-l:libfoo.a' syntax
# via invoking the C compiler.
AC_DEFUN([LINKER_SUPPORTS_VERBATIM_LINKING],[
  AC_MSG_CHECKING([whether linker supports -l:libfoo.a])

  # autoconf macros don't give us enough flexibility to run this test:
  #
  # * we need to manually create a static archive
  # * link against said static archive via custom linker options
  test_verbatim() {
    # from https://www.gnu.org/software/autoconf/manual/autoconf-2.68/html_node/Limitations-of-Usual-Tools.html
    # Create a temporary directory $dir in $TMPDIR (default /tmp).
    # Use mktemp if possible; otherwise fall back on mkdir,
    # with $RANDOM to make collisions less likely.
    : "${TMPDIR:=/tmp}"
    {
      dir=$(umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null &&
      test -d "$dir"
    } || {
      dir=$TMPDIR/foo$$-$RANDOM

      (umask 077 && mkdir "$dir")
    } || exit $?

    cat > "${dir}/test.c" << EOF
void my_func(void) {
  /* A simple function to be archived */
}
EOF

    cat > ${dir}/main.c << EOF
void my_func(void);
int main(void) {
  my_func();
  return 0;
}
EOF

    $CC -c "${dir}/test.c" -o "${dir}/test.o"
    $AR q "${dir}/libtest.a" "${dir}/test.o"
    $RANLIB "${dir}/libtest.a"
    $CC "${dir}/main.c" -o "${dir}/main" "-L${dir}" -l:libtest.a || return 1

    rm -f "${dir}/test.c" "${dir}/test.o" "${dir}/libtest.a" "${dir}/main.c" "${dir}/main"
    rmdir "${dir}"
  }

test_verbatim >&AS_MESSAGE_LOG_FD 2>&1
status=$?

if test $status -ne 0 ; then
  AC_MSG_RESULT([no])
  AC_SUBST([LdSupportsVerbatimNamespace], [NO])
else
  AC_MSG_RESULT([yes])
  AC_SUBST([LdSupportsVerbatimNamespace], [YES])
fi

]
)
