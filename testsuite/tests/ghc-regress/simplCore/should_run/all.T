
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "simplrun001" { vt("-O", "", "") }
test "simplrun002" { vt("-O", "", "") }
