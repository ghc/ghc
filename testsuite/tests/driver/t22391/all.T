test('t22391', [extra_files(['src'])],
     multimod_compile, ['Lib', '-v1 -Wall -fhide-source-paths -isrc -fdefer-diagnostics'])

test('t22391j', [req_ghc_smp, req_target_smp, extra_files(['src'])],
     multimod_compile, ['Lib', '-v1 -Wall -fhide-source-paths -isrc -fdefer-diagnostics -j2'])
