
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtc are: extra compile flags

-- Use this itsy helper fn to pass in an extra flag
def myvtc($extra_comp_args)
{
   vtc(" -O " ++ $extra_comp_args)
}


test "default" { myvtc("") }
test "fact"    { myvtc("") }
test "fun"     { myvtc("") }
test "goo"     { myvtc("") }
test "ins"     { myvtc("") }
test "map"     { myvtc("") }
test "sim"     { myvtc("") }
test "str001"  { myvtc("") }
test "str002"  { myvtc("") }
test "syn"     { myvtc("") }
test "test"    { myvtc("") }
test "tst"     { myvtc("") }
test "unu"     { myvtc("") }
