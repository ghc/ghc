-- !!! empty decls, alts and stmts
module M where {
;;;;;;;;import Data.Char;;x = 1;;y = 2;
v = do {;;;;;;;;;;;;;;;;;;;;;;;x <- [1];;return x;;};
f x = case x of { ;;;;;;2 -> 'a';;;;;;3->'b';;;;;;;;;;;};;;
g x = case x of { ;;;;;;;;;; _ -> "aa"; }
}
