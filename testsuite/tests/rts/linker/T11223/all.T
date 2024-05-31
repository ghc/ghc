import string
import re

#--------------------------------------
# Python normalization functions
#--------------------------------------
def normalise_duplicate_errmsg( msg ):
     return re.sub(r"((?:[a-z, A-Z]+):|)[\/\\\\]+(?:.+[\/\\\\]+?)?(.+?)[\/\\\\]+", "    ",
               re.sub(r"version\s(\d+\.\d+\.\d+)\sfor\s[a-z,A-Z,0-9,_,-]+(?=\))", r"", msg))

#--------------------------------------
# Test functions
#--------------------------------------

test('T11223_simple_link',
     [extra_files(['foo.c', 'foo.hs']),
      when(ghc_dynamic(), skip),
      req_c],
     makefile_test, ['t_11223_simple_link'])

test('T11223_simple_link_lib',
     [extra_files(['foo.c', 'foo.hs']),
      when(ghc_dynamic(), skip),
      req_c],
     makefile_test, ['t_11223_simple_link_lib'])

# I'm ignoring the output since for this particular invocation normalise_errmsg
# isn't being called and I can't figure out why not.
test('T11223_simple_duplicate',
     [extra_files(['bar.c', 'foo.c', 'foo.hs']),
      when(ghc_dynamic(), skip), ignore_stderr, ignore_stdout, exit_code(2), normalise_errmsg_fun(normalise_duplicate_errmsg)],
     makefile_test, ['t_11223_simple_duplicate'])

test('T11223_simple_duplicate_lib',
     [extra_files(['bar.c', 'foo.c', 'foo.hs']),
      when(ghc_dynamic(), skip), normalise_errmsg_fun(normalise_duplicate_errmsg),
      req_c],
     makefile_test, ['t_11223_simple_duplicate_lib'])

test('T11223_simple_unused_duplicate_lib',
     [extra_files(['bar.c', 'foo.c', 'foo.hs']),
      when(ghc_dynamic(), skip),
      req_c],
     makefile_test, ['t_11223_simple_unused_duplicate_lib'])

test('T11223_link_order_a_b_succeed',
     [extra_files(['bar.c', 'foo.c', 'foo2.hs']),
      when(ghc_dynamic(), skip),
      req_c],
     makefile_test, ['t_11223_link_order_a_b_succeed'])

test('T11223_link_order_b_a_succeed',
     [extra_files(['bar.c', 'foo.c', 'foo2.hs']),
      when(ghc_dynamic(), skip),
      req_c],
     makefile_test, ['t_11223_link_order_b_a_succeed'])

test('T11223_link_order_a_b_2_fail',
     [extra_files(['bar.c', 'foo.c', 'foo3.hs']),
      when(ghc_dynamic(), skip), normalise_errmsg_fun(normalise_duplicate_errmsg),
      req_c],
     makefile_test, ['t_11223_link_order_a_b_2_fail'])

test('T11223_link_order_b_a_2_succeed',
     [extra_files(['bar.c', 'foo.c', 'foo3.hs']),
      when(ghc_dynamic(), skip),
      req_c],
     makefile_test, ['t_11223_link_order_b_a_2_succeed'])

# Weak Symbols are not currently implemented. So Disable all the tests
# See Note [weak-symbols-support] in Linker.c

test('T11223_weak_only_link_fail',
     [extra_files(['power.c', 'power.hs']),
      when(ghc_dynamic(), skip), expect_broken(11817)],
     makefile_test, ['t_11223_weak_only_link_fail'])

test('T11223_weak_only_link_succeed',
     [extra_files(['power3.hs', 'power_slow.c']),
      when(ghc_dynamic(), skip), expect_broken(11817)],
     makefile_test, ['t_11223_weak_only_link_succeed'])

test('T11223_weak_both_link_order_a_b_succeed',
     [extra_files(['power.c', 'power3.hs', 'power_slow.c']),
      when(ghc_dynamic(), skip), expect_broken(11817)],
     makefile_test, ['t_11223_weak_both_link_order_a_b_succeed'])

test('T11223_weak_both_link_order_b_a_succeed',
     [extra_files(['power.c', 'power3.hs', 'power_slow.c']),
      when(ghc_dynamic(), skip), expect_broken(11817)],
     makefile_test, ['t_11223_weak_both_link_order_b_a_succeed'])

test('T11223_weak_single_link_order_a_b_succeed',
     [extra_files(['power.c', 'power3.hs', 'power_slow.c']),
      when(ghc_dynamic(), skip), expect_broken(11817)],
     makefile_test, ['t_11223_weak_single_link_order_a_b_succeed'])

test('T11223_weak_single_link_order_b_a_succeed',
     [extra_files(['power.c', 'power3.hs', 'power_slow.c']),
      when(ghc_dynamic(), skip), expect_broken(11817)],
     makefile_test, ['t_11223_weak_single_link_order_b_a_succeed'])
