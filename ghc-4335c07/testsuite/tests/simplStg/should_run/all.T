# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# Only compile with optimisation
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('T9291', normal, compile_and_run, [''])
test('T13536', normal, compile_and_run, [''])

test('T13536a',
    [stats_num_field('bytes allocated',
                    [ (wordsize(64), 86664, 5) ]),
                    # 2017-04-10     86664 -- 25769889696 if broken
     only_ways(['optasm'])],
    compile_and_run,
    [''])

