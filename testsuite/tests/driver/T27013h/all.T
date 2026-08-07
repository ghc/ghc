import re

def strip_call_stack(s):
    return re.sub(r'^[ ]*Call stack:\n([ ]+.*\n)*', '', s, flags=re.M)

test('T27013h',
     [ extra_files(['GHC/', 'unitT27013h'])
     , normalise_errmsg_fun(strip_call_stack)
     ], multiunit_compile_fail, [['unitT27013h'], '-v0'])
