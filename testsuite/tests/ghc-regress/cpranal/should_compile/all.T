
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtc are: extra compile flags

test "Cpr001_imp" { vtc("-O") }
test "Cpr001"     { vtc("-O") }
