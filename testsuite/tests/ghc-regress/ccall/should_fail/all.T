
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtcf are: extra compile flags

def myvtcf ( $args )
{
   vtcf ( " -fglasgow-exts -package lang " ++ $args)
}

test "cc001" { myvtcf("") }
test "cc002" { myvtcf("") }
test "cc004" { myvtcf("") }
test "cc005" { myvtcf("") }
