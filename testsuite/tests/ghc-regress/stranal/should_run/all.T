
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "strun001" { vtr("", "", "") }
test "strun002" { vtr("-O", "", "1") }

