
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "arith001" { vt("", "", "") }
test "arith002" { vt("", "", "") }
test "arith003" { vt("", "", "") }
test "arith004" { vt("", "", "") }
test "arith005" { vt("", "", "") }
test "arith006" { vt("-package lang", "", "") }
test "arith007" { vt("", "", "") }
test "arith008" { vt("", "", "") }
test "arith009" { vt("", "", "") }
test "arith010" { vt("-package lang", "", "") }
test "arith011" { vt("-package lang", "", "") }
test "arith012" { vt("-package lang", "", "") }
test "arith013" { vt("", "", "") }
test "num001" { vt("", "", "") }
test "num002" { vt("", "", "") }
test "num003" { vt("", "", "") }
test "num004" { vt("", "", "") }
test "num005" { vt("", "", "") }
test "num006" { vt("-package lang", "", "") }
test "num007" { vt("", "", "") }
test "num008" { vt("", "", "") }
test "num009" { vt("", "", "") }
test "num010" { vt("", "", "") }
test "num011" { vt("", "", "") }
