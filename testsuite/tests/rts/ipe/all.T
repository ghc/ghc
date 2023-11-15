# Remove the capability prefix from IPE event log lines.
def noCapabilityOutputFilter(s):
     return re.sub(r'[a-f0-9]+: IPE:', 'IPE:', s)

test('ipeMap', [extra_files(['ipe_lib.c', 'ipe_lib.h']), c_src], compile_and_run, ['ipe_lib.c'])

# Manually create IPE entries and dump them to event log (stderr).
test('ipeEventLog',
     [ c_src,
       extra_files(['ipe_lib.c', 'ipe_lib.h']),
       extra_run_opts('+RTS -va -RTS'),
       grep_errmsg('table_name_'),
       only_ways(debug_ways),
       normalise_errmsg_fun(noCapabilityOutputFilter),
       ignore_stdout,
       # Due to issues on Darwin CI runners that couldn't be tracked down.
       # In general this test should work on Darwin - Just not on our CI.
       when(opsys('darwin'), fragile(0))
     ],
     compile_and_run, ['ipe_lib.c'])

# Manually create IPE entries, force the initialization of the underlying hash map
# and dump them to event log (stderr).
test('ipeEventLog_fromMap',
     [ c_src,
       extra_files(['ipe_lib.c', 'ipe_lib.h']),
       extra_run_opts('+RTS -va -RTS'),
       grep_errmsg('table_name_'),
       only_ways(debug_ways),
       normalise_errmsg_fun(noCapabilityOutputFilter),
       ignore_stdout,
       # Due to issues on Darwin CI runners that couldn't be tracked down.
       # In general this test should work on Darwin - Just not on our CI.
       when(opsys('darwin'), fragile(0))
     ],
     compile_and_run, ['ipe_lib.c'])

