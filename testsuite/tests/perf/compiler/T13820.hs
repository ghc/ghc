-- Regression test for #13820. Instead of the original 27 `id`s, this
-- test uses only 24, so a regression is less likely to result in an
-- out-of-memory situation.
f = id id id id
    id id id id
    id id id id
    id id id id
    id id id id
    id id id id

main = print 1
