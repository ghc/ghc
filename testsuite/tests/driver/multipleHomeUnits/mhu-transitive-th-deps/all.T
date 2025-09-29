# We just want compilation to succeed here
test('mhu-transitive-th-deps-bc',
  [ extra_files(["dep1", "DepA", "DepC", "unita","unitc"])
  ], makefile_test, ['test-bc'])

test('mhu-transitive-th-deps-obj',
  [ extra_files(["dep1", "DepA", "DepC", "unita","unitc"])
  ], makefile_test, ['test-obj'])

