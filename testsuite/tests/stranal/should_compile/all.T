# Only compile with optimisation
setTestOpts( only_ways(['optasm']) )

def checkCoreString(needle):
    def norm(str):
        if needle in str:
            return "%s contained in -ddump-simpl\n" % needle
        else:
            return "%s not contained in -ddump-simpl\n" % needle
    return normalise_errmsg_fun(norm)

def grepCoreString(needle):
    def norm(str):
        return "".join(filter(lambda l: re.search(needle, l), str.splitlines(True)))
    return normalise_errmsg_fun(norm)

test('default', normal, compile, [''])
test('fact', normal, compile, [''])
test('fun', normal, compile, [''])
test('goo', normal, compile, [''])
test('ins', normal, compile, [''])
test('map', normal, compile, [''])
test('sim', normal, compile, [''])
test('str001', normal, compile, [''])
test('str002', normal, compile, [''])
test('syn', normal, compile, [''])
test('test', normal, compile, [''])
test('tst', normal, compile, [''])
test('unu', normal, compile, [''])
test('newtype', req_profiling, compile, ['-prof -fprof-auto'])
test('T1988', normal, compile, [''])
test('T8467', normal, compile, [''])
test('T8037', normal, compile, [''])
test('T8743', [ extra_clean(['T8743.o-boot', 'T8743a.hi', 'T8743a.o', 'T8743.hi-boot']) ], multimod_compile, ['T8743', '-v0'])

# T10482
#   The intent here is to check that $wfoo has type
#         $wfoo :: Int# -> Int# -> Int
#   with two unboxed args.  See Trac #10482 for background
# 
# Set -dppr-cols to ensure output doesn't wrap
test('T10482',  [ grepCoreString(r'wfoo.*Int#') ], compile, ['-dppr-cols=200 -ddump-simpl'])
test('T10482a', [ grepCoreString(r'wf.*Int#') ],   compile, ['-dppr-cols=200 -ddump-simpl'])

test('T9208', when(compiler_debugged(), expect_broken(9208)), compile, [''])
# T9208 fails (and should do so) if you have assertion checking on in the compiler
# Hence the above expect_broken.  See comments in the Trac ticket

test('T10694', [ grepCoreString(r'Str=') ],   compile, ['-dppr-cols=200 -ddump-simpl'])
test('T11770', [ checkCoreString('OneShot') ], compile, ['-ddump-simpl'])


