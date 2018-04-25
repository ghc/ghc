# basic tests
## complains about M1 only
test('T13727a', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['Main', '-isrc-exe -isrc-lib -Wmissing-home-modules'])
test('T13727b', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['src-exe/Main.hs', '-isrc-lib -Wmissing-home-modules'])
## warning-free
test('T13727c', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['Main', 'M1 -isrc-exe -isrc-lib -Wmissing-home-modules'])
test('T13727d', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['src-exe/Main.hs', 'M1 -isrc-lib -Wmissing-home-modules'])
test('T13727e', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['src-exe/Main.hs', 'src-lib/M1.hs -Wmissing-home-modules'])

# tests involving -main-is
## complains about M1 & Main
test('T13727f', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['AltMain', '-main-is AltMain.alt_main -isrc-exe -isrc-lib -Wmissing-home-modules'])
test('T13727g', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['src-exe/AltMain.hs', '-main-is AltMain.alt_main -isrc-exe -isrc-lib -Wmissing-home-modules'])
## complains about M1 only
test('T13727h', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['src-exe/AltMain.hs', '-main-is AltMain.alt_main ./src-exe/Main.hs -isrc-exe -isrc-lib -Wmissing-home-modules'])
test('T13727i', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['AltMain', '-main-is AltMain.alt_main Main -isrc-exe -isrc-lib -Wmissing-home-modules'])
## complains about Main only
test('T13727j', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['AltMain', '-main-is AltMain.alt_main M1 -isrc-exe -isrc-lib -Wmissing-home-modules'])
## warning-free
test('T13727k', [extra_files(['src-exe/','src-lib/'])], multimod_compile, ['AltMain', '-main-is AltMain.alt_main M1 Main -isrc-exe -isrc-lib -Wmissing-home-modules'])
