import string
import re

#--------------------------------------
# Python normalization functions
#--------------------------------------

def normalise_search_dirs (str):
  str = re.sub(r"directories searched:\s+.+$",
               r"directories searched: (none)", str, flags=re.MULTILINE)
  str = re.sub(r"^\s+[A-Za-z]:[\\\/].+$", "", str, flags=re.MULTILINE)
  return str

#--------------------------------------
# Test functions
#--------------------------------------
test('T13082_good',
     [extra_files(['foo.c', 'main.hs', 'foo_dll.c']),
      unless(opsys('mingw32'), skip)],
     run_command, ['$MAKE -s --no-print-directory T13082_good'])

test('T13082_fail', [extra_files(['main.hs']), unless(opsys('mingw32'), skip),
                     normalise_errmsg_fun(normalise_search_dirs)],
     run_command, ['$MAKE -s --no-print-directory T13082_fail'])
