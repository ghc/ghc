
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtf are: extra compile flags

def myvtf ( $extra_compile_args )
{
   vtf ( $extra_compile_args ++ " -ddump-parsed ")
}

test "read001" { myvtf("") }
test "read002" { myvtf("") }
test "read003" { myvtf("") }
test "read004" { myvtf("") }
test "read005" { myvtf("") }
test "read006" { myvtf("") }
test "read007" { myvtf("") }
test "read008" { myvtf("") }
test "read009" { myvtf("") }
test "read011" { myvtf("") }
test "read012" { myvtf("") }
test "read013" { myvtf("-fglasgow-exts") }
test "read014" { myvtf("") }
test "read015" { myvtf("") }
test "read016" { myvtf("") }
test "read017" { myvtf("") }
