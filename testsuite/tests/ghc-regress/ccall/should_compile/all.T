
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtc are: extra compile flags

-- Use this itsy helper fn to pass in an extra flag
def myvtc($extra_comp_args)
{
   vtc(" -fglasgow-exts -package lang " ++ $extra_comp_args)
}

test "cc001" { myvtc("") }
test "cc002" { myvtc("") }
test "cc003" { myvtc("") }
test "cc004" { myvtc("-fvia-C") }
test "cc005" { myvtc("-fvia-C") }
test "cc006" { myvtc("-fvia-C -fno-prune-tydecls") }
test "cc007" { myvtc("") }
test "cc008" { myvtc("") }
test "cc009" { myvtc("") }
test "cc010" { myvtc("-fvia-C") }
