test('hie001',       normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('hie002',
     [# Allocation numbers unstable on 32-bit, skip:
      when(wordsize(32), skip),
      # No linting in perf tests:
      no_lint,
      high_memory_usage,
      collect_compiler_stats('bytes allocated',10)],
     compile,
     ['-fno-code -fwrite-ide-info'])
test('hie003',       normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('hie004',       normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('hie005',       normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('hie006',       normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('hie007',       normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('hie008',       normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('hie009',       normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('hie010',       normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('CPP',          normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('Constructors', normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('Scopes',       normal,                   compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
# See https://gitlab.haskell.org/ghc/ghc/-/issues/18425 and https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2464#note_301989
test('ScopesBug',    expect_broken(18425),     compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('T18425',       normal,     compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])
test('T22416',       normal,     compile, ['-fno-code -fwrite-ide-info -fvalidate-ide-info'])