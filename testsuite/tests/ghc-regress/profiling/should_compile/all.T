
# We need to run prof001 and prof002 the normal way, as the extra flags
# added for the profiling ways makes it pass
test('prof001', only_ways(['normal']), compile, ['-prof -caf-all'])
test('prof002', only_ways(['normal']), compile, ['-prof -caf-all'])

