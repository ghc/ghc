
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "dsrun001" { vt("", "", "") }
test "dsrun002" { vt("", "", "") }
test "dsrun003" { vt("", "", "") }
test "dsrun004" { vt("", "", "") }
test "dsrun005" { vt("", "", "1") }
test "dsrun006" { vt("", "", "") }
test "dsrun007" { vt("", "", "1") }
