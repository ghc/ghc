test('readLitChar',   normal, compile_and_run, [''])

if config.platform == 'i386-unknown-openbsd':
        ways = expect_fail
else:
        ways = normal

test('unicode001',    ways, compile_and_run, [''])
test('unicode002',    ways, compile_and_run, [''])

