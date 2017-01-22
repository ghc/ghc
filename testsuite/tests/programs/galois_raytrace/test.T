# Floating point differences on x86 using the NCG
if config.platform.startswith('i386-') and \
       config.platform != 'i386-unknown-openbsd':
    setTestOpts(expect_fail_for(['hpc','optasm','profasm','threaded2','profthreaded']))

test('galois_raytrace', [when(fast(), skip), reqlib('parsec')],
     multimod_compile_and_run, ['Main', '-package parsec'])
