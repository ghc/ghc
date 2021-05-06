# FP_PROG_SORT
# ------------
# Find a Unix-like sort
AC_DEFUN([FP_PROG_SORT],
[AC_PATH_PROG([fp_prog_sort], [sort])
echo conwip > conftest.txt
$fp_prog_sort -f conftest.txt > conftest.out 2>&1
if grep 'conwip' conftest.out > /dev/null 2>&1 ; then
  # The goods
  SortCmd="$fp_prog_sort"
else
  # Summink else..pick next one.
  AC_MSG_WARN([$fp_prog_sort looks like a non-*nix sort, ignoring it])
  FP_CHECK_PROG([SortCmd], [sort], [], [], [$fp_prog_sort])
fi
rm -f conftest.txt conftest.out
AC_SUBST([SortCmd])[]dnl
])# FP_PROG_SORT
