-- Trac #5252
-- Killed 7.03 when compiled witout -O, 
-- because it could not see that x had a product type
-- but MkS still unpacked it

module T5252 where
import T5252a

blah :: S -> T
blah (MkS x _) = x



