
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "arr001" { vt("", "", "") }
test "arr002" { vt("", "", "") }
test "arr003" { vt("", "", "1") }
test "arr004" { vt("", "", "1") }
test "arr005" { vt("", "", "") }
test "arr006" { vt("", "", "") }
test "arr007" { vt("", "", "7") }
test "arr008" { vt("", "", "8") }
test "arr009" { vt("", "", "") }
test "arr010" { vt("", "", "") }
test "arr011" { vt("", "", "") }
test "arr012" { vt("", "", "") }
test "arr013" { vt("", "", "") }
test "arr014" { vt("-package lang", "", "") }
test "arr015" { vt("", "", "") }
test "arr016" { vt("-fglasgow-exts", "", "") }
