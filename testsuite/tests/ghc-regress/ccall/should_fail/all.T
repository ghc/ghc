
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtf are: extra compile flags

def myvtf ( $args )
{
   vtf ( " -package lang " ++ $args)
}

test "cc001" { myvtf("") }
test "cc002" { myvtf("") }
test "cc004" { myvtf("") }
test "cc005" { myvtf("") }
