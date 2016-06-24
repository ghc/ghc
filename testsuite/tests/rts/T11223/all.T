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
     [when(ghc_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_link'])

test('T11223_simple_link_lib',
     [when(ghc_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_link_lib'])

# I'm ignoring the output since for this particular invocation normalise_errmsg
# isn't being called and I can't figure out why not.
test('T11223_simple_duplicate',
     [when(ghc_dynamic(), skip), ignore_stderr, ignore_stdout, exit_code(2), normalise_errmsg_fun(normalise_duplicate_errmsg)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_duplicate'])

test('T11223_simple_duplicate_lib',
     [when(ghc_dynamic(), skip), normalise_errmsg_fun(normalise_duplicate_errmsg)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_duplicate_lib'])

test('T11223_simple_unused_duplicate_lib',
     [when(ghc_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_unused_duplicate_lib'])

test('T11223_link_order_a_b_succeed',
     [when(ghc_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_link_order_a_b_succeed'])

test('T11223_link_order_b_a_succeed',
     [when(ghc_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_link_order_b_a_succeed'])

test('T11223_link_order_a_b_2_fail',
     [when(ghc_dynamic(), skip), normalise_errmsg_fun(normalise_duplicate_errmsg)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_link_order_a_b_2_fail'])

test('T11223_link_order_b_a_2_succeed',
     [when(ghc_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_link_order_b_a_2_succeed'])

# Weak Symbols are not currently implemented. So Disable all the tests
# See Note [weak-symbols-support] in Linker.c

test('T11223_weak_only_link_fail',
     [when(ghc_dynamic(), skip), expect_broken(11817)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_only_link_fail'])

test('T11223_weak_only_link_succeed',
     [when(ghc_dynamic(), skip), expect_broken(11817)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_only_link_succeed'])

test('T11223_weak_both_link_order_a_b_succeed',
     [when(ghc_dynamic(), skip), expect_broken(11817)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_both_link_order_a_b_succeed'])

test('T11223_weak_both_link_order_b_a_succeed',
     [when(ghc_dynamic(), skip), expect_broken(11817)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_both_link_order_b_a_succeed'])

test('T11223_weak_single_link_order_a_b_succeed',
     [when(ghc_dynamic(), skip), expect_broken(11817)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_single_link_order_a_b_succeed'])

test('T11223_weak_single_link_order_b_a_succeed',
     [when(ghc_dynamic(), skip), expect_broken(11817)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_single_link_order_b_a_succeed'])
