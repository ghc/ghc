
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "dsrun001" { vtr("", "", "") }
test "dsrun002" { vtr("", "", "") }
test "dsrun003" { vtr("", "", "") }
test "dsrun004" { vtr("", "", "") }
test "dsrun005" { vtr("", "", "1") }
test "dsrun006" { vtr("", "", "") }
test "dsrun007" { vtr("", "", "1") }
test "dsrun008" { vtr("", "", "1") }
