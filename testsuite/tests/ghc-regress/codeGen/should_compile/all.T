
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtc are: extra compile flags

test "cg001" { vtc("") }
