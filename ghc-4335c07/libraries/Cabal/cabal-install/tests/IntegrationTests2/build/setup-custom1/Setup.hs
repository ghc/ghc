import Distribution.Simple
main = defaultMain >> writeFile "marker" "ok"
