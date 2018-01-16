# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

# Only compile with optimisation
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

def checkStgString(needle):
    def norm(str):
        if needle in str:
            return "%s contained in -ddump-simpl\n" % needle
        else:
            return "%s not contained in -ddump-simpl\n" % needle
    return normalise_errmsg_fun(norm)



test('T13588', [ checkStgString('case') ] , compile, ['-ddump-stg'])
