
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtcf are: extra compile flags

def myvtcf ( $extra_compile_args )
{
   vtcf ( $extra_compile_args ++ " -ddump-parsed ")
}

test "read001" { myvtcf("") }
test "read002" { myvtcf("") }
test "read003" { myvtcf("") }
test "read004" { myvtcf("") }
test "read005" { myvtcf("") }
test "read006" { myvtcf("") }
test "read007" { myvtcf("") }
test "read008" { myvtcf("") }
test "read009" { myvtcf("") }
test "read011" { myvtcf("") }
test "read012" { myvtcf("") }
test "read013" { myvtcf("-fglasgow-exts") }
test "read014" { myvtcf("") }
test "read015" { myvtcf("") }
test "read016" { myvtcf("") }
test "read017" { myvtcf("") }
