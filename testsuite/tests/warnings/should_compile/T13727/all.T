# basic tests
## complains about M1 only
test('T13727a', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['Main', '-isrc-exe -isrc-lib -Wmissing-home-modules'])
test('T13727b', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['src-exe/Main.hs', '-isrc-lib -Wmissing-home-modules'])
## warning-free
test('T13727c', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['Main', 'M1 -isrc-exe -isrc-lib -Wmissing-home-modules'])
test('T13727d', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['src-exe/Main.hs', 'M1 -isrc-lib -Wmissing-home-modules'])
test('T13727e', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['src-exe/Main.hs', 'src-lib/M1.hs -Wmissing-home-modules'])

# tests involving -main-is
## complains about M1 & Main
test('T13727f', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['AltMain', '-main-is AltMain.alt_main -isrc-exe -isrc-lib -Wmissing-home-modules'])
test('T13727g', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['src-exe/AltMain.hs', '-main-is AltMain.alt_main -isrc-exe -isrc-lib -Wmissing-home-modules'])
## complains about M1 only
test('T13727h', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['src-exe/AltMain.hs', '-main-is AltMain.alt_main ./src-exe/Main.hs -isrc-exe -isrc-lib -Wmissing-home-modules'])
test('T13727i', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['AltMain', '-main-is AltMain.alt_main Main -isrc-exe -isrc-lib -Wmissing-home-modules'])
## complains about Main only
test('T13727j', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['AltMain', '-main-is AltMain.alt_main M1 -isrc-exe -isrc-lib -Wmissing-home-modules'])
## warning-free
test('T13727k', [javascript_failures, extra_files(['src-exe/','src-lib/'])], multimod_compile, ['AltMain', '-main-is AltMain.alt_main M1 Main -isrc-exe -isrc-lib -Wmissing-home-modules'])
