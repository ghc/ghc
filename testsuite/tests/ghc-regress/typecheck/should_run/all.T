
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "tcrun001" { vt("", "", "") }
test "tcrun002" { vt("", "", "") }
test "tcrun003" { vt("-fglasgow-exts", "", "") }
test "tcrun004" { vt("-fglasgow-exts", "", "") }
test "tcrun005" { vt("", "", "") }
test "tcrun006" { vt("", "", "") }
test "tcrun007" { vt("", "", "") }
test "tcrun008" { vt("", "", "") }
test "tcrun009" { vt("", "", "") }
test "tcrun010" { vt("", "", "") }
test "tcrun011" { vt("", "", "") }
test "tcrun012" { vt("", "", "") }
test "tcrun013" { vt("", "", "") }
