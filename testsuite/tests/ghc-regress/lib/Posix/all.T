if config.platform == 'i386-unknown-mingw32':
   conf = skip
else:
   conf = normal

test('signals001',  conf, compile_and_run, ['-package unix -cpp'])
test('signals002',  conf, compile_and_run, ['-package unix'])
test('fileexist01', conf, compile_and_run, ['-package unix'])
