
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "cg001" { vt("", "", "") }
test "cg002" { vt("", "", "") }
test "cg003" { vt("", "", "") }
test "cg004" { vt("", "", "") }
test "cg005" { vt("", "", "") }
test "cg006" { vt("", "", "") }
test "cg007" { vt("", "", "") }
test "cg008" { vt("", "", "") }
test "cg009" { vt("", "", "") }
test "cg010" { vt("", "", "") }
test "cg011" { vt("", "", "") }
test "cg012" { vt("-fglasgow-exts", "", "") }
test "cg013" { vt("", "", "") }
test "cg014" { vt("", "", "") }
test "cg015" { vt("-fglasgow-exts", "", "") }
test "cg016" { vt("", "", "1") }
test "cg017" { vt("", "", "") }
test "cg018" { vt("-fglasgow-exts", "", "") }
test "cg019" { vt("", "", "") }
test "cg020" { vt("", "", "") }
test "cg021" { vt("", "", "") }
test "cg022" { vt("", "", "") }
test "cg023" { vt("", "", "") }
test "cg024" { vt("", "", "") }
test "cg025" { vt("-package lang -package text", "", "1") }
test "cg026" { vt("-package lang -fglasgow-exts -fvia-C", "", "") }
test "cg027" { vt("", "", "") }
test "cg028" { vt("", "", "") }
test "cg029" { vt("-package lang -fglasgow-exts", "", "") }
test "cg030" { vt("-package lang -fglasgow-exts", "", "") }
test "cg031" { vt("-fglasgow-exts", "", "") }
test "cg032" { vt("-fglasgow-exts", "", "") }
test "cg033" { vt("-fglasgow-exts", "", "") }
test "cg034" { vt("", "", "") }
test "cg035" { vt("-package lang -fglasgow-exts", "", "") }
test "cg036" { vt("", "", "") }
test "cg037" { vt("", "", "") }
test "cg038" { vt("", "", "") }
test "cg039" { vt("", "", "") }
test "cg040" { vt("", "", "") }
test "cg041" { vt("", "", "") }
test "cg042" { vt("-package lang -fglasgow-exts", "", "") }
test "cg043" { vt("", "", "") }
test "cg044" { vt("-package lang", "", "" ) }

-- tmp, until we fix the problems with seq#...
test "cg045" { vt( "-O", "", "1") }
test "cg046" { vt("", "", "") }
test "cg047" { vt("", "", "") }
test "cg048" { vt("", "", "") }
test "cg049" { vt( "-funbox-strict-fields", "", "") }
