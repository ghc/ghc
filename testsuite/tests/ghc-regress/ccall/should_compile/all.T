
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtc are: extra compile flags

-- Use this itsy helper fn to pass in an extra flag
def myvtc($extra_comp_args)
{
   vtc(" -fglasgow-exts " ++ $extra_comp_args)
}

$compile_to_hc = "True"

test "cc001" { myvtc("") }
test "cc002" { myvtc("") }
test "cc003" { myvtc("") }
test "cc004" { myvtc("") }
test "cc005" { myvtc("") }
test "cc006" { myvtc("-fno-prune-tydecls") }
test "cc007" { myvtc("") }
test "cc008" { myvtc("") }
test "cc009" { myvtc("") }
test "cc010" { myvtc("") }
test "cc011" { myvtc("") }
