
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "arith001" { vtr("", "", "") }
test "arith002" { vtr("", "", "") }
test "arith003" { vtr("", "", "") }
test "arith004" { vtr("", "", "") }
test "arith005" { vtr("", "", "") }
test "arith006" { vtr("-package lang", "", "") }
test "arith007" { vtr("", "", "") }
test "arith008" { vtr("", "", "") }
test "arith009" { vtr("", "", "") }
test "arith010" { vtr("-package lang", "", "") }
test "arith011" { vtr("-package lang", "", "") }
test "arith012" { vtr("-package lang", "", "") }
test "arith013" { vtr("", "", "") }
test "arith014" { vtr("", "", "") }
test "arith015" { vtr("", "", "") }
test "num001" { vtr("", "", "") }
test "num002" { vtr("", "", "") }
test "num003" { vtr("", "", "") }
test "num004" { vtr("", "", "") }
test "num005" { vtr("", "", "") }
test "num006" { vtr("-package lang", "", "") }
test "num007" { vtr("", "", "") }
test "num008" { vtr("", "", "") }
test "num009" { vtr("", "", "") }
test "num010" { vtr("", "", "") }
test "num011" { vtr("", "", "") }
