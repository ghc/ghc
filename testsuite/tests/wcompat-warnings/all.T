# N.B. the source files are all stubs; edit Template.hs
test('WCompatWarningsOn',    extra_files(['Template.hs']), compile, [''])
test('WCompatWarningsOff',   extra_files(['Template.hs']), compile, [''])
test('WCompatWarningsOnOff', extra_files(['Template.hs']), compile, [''])
