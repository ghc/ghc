-- Test that UTF-8 is passed through correctly by hsc2hs

#define uni Ⴀ

#let name = "αβγ"

main = let (#name) = 3 in print (#name)
