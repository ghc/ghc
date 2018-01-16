{-
 - Decode.hs
 -
 - Module containing the code to decode LZW encodings
 -
 - Paul Sanders, Applications Research Division, BTL 1992
 -
 - DEC_VERSION 1 uses a list with keys in ascending order as a table, ie.
 - entry n is given by table!!n.
 -
 - DEC_VERSION 2 uses a list with keys in descending order as a table, ie.
 - entry n is given by table!!(#table-n). We don't need to calculate the
 - length of the table however as this is given by the value of the next
 - code to be added.
 -
 - DEC_VERSION 3 uses a balanced binary tree to store the keys. We can do
 - this cheaply by putting the key in the correct place straight away and
 - therefore not doing any rebalancing.
 -}

module Decode (decode)
where

import Prelude hiding( lookup )		-- lookup defined locally
import Defaults
import BinConv

data Optional a = NONE | SOME a deriving (Eq, Show{-was:Text-})

{- We ideally want to store the table as an array but these are inefficient
 - so we use a list instead. We don't use the tree used by encode since we
 - can make use of the fact that all our keys (the codes) come in order and
 - will be placed at the end of the table, at position 'code'.
 -
 - An entry of (SOME n, 'c') indicates that this code has prefix code n
 - and final character c.
 -}


{- Kick off the decoding giving the real function the first code value and
 - the initial table.
 -}

decode :: [Int] -> String
decode []
      = []
decode cs
      = decode' cs first_code init_table

{- decode` decodes the first character which is special since no new code
 - gets added for it. It is also special in so far as we know that the
 - code is a singleton character and thus has prefix NONE. The '@' is a
 - dummy character and can be anything.
 -}

decode' [] _ _ = []
decode' (c:cs) n t
      = ch : do_decode cs n c ch t
        where
        (NONE, ch) = lookup c t

{- do_decode decodes all the codes bar the first. 
 -
 - If the code is in the table (ie the code is less than the next code to be 
 - added) then we output the string for that code (using unfold if a prefix 
 - type) and add a new code to the table with the final character output as 
 - the extension and the previous code as prefix.
 -
 - If the code is not one we know about then we give it to decode_special for
 - special treatment
 -}

do_decode [] _ _ _ _ = []
do_decode (c:cs) n old_n fin_char t
      = if c >= n          -- we don't have this code in the table yet
        then decode_special (c:cs) n old_n fin_char t
        else outchs ++ do_decode cs n' c (head outchs) t'
        where
        outchs = reverse (unfold c (n-1) t)
        (n', t') = if n == max_entries
                   then (n, t)
                   else (n+1, insert n (SOME old_n, head outchs) t)

{- decode_special decodes a code that isn't in the table.
 -
 - The algorithm in Welch describes why this works, suffice it to say that
 - the output string is given by the last character output and the string
 - given by the previous code. An entry is also made in the table for the
 - last character output and the old code.
 -}

decode_special (c:cs) n old_n fin_char t
      = outchs ++ do_decode cs n' c (head outchs) t'
        where
        outchs = reverse (fin_char : unfold old_n (n-1) t)
        (n', t') = if n == max_entries
                   then (n, t)
                   else (n+1, insert n (SOME old_n, fin_char) t)

{- unfold a prefix code.
 -
 - chain back through the prefixes outputting the extension characters as we
 - go.
 -}

unfold n t_len t
      = if prefix == NONE
        then [c]
        else c : unfold n' t_len t
        where
        (prefix, c) = lookup n t
        SOME n' = prefix

data DecompTable = Branch DecompTable DecompTable | Leaf (Optional Int, Char) deriving (Show{-was:Text-})

{- Insert a code pair into the table. The position of the code is given by
 - the breakdown of the key into its binary digits
 -}

insert n v t = insert' (dec_to_binx code_bits n) v t

{- We can place a code exactly where it belongs using the following algorithm.
 - Take the code's binary rep expanded to the maximum number of bits. Start
 - at the first bit, if a 0 then insert the code to the left, if a 1 then
 - insert to the right. Carry on with the other bits until we run out and are
 - thus at the right place and can construct the node.
 -}

insert' [] v (Leaf _)
      = Leaf v
insert' ('0' : bs) v (Branch l r)
      = Branch (insert' bs v l) r
insert' ('1' : bs) v (Branch l r)
      = Branch l (insert' bs v r)
insert' ('0' : bs) v t
      = Branch (insert' bs v t) t
insert' ('1' : bs) v t
      = Branch t (insert' bs v t)

{- For a lookup we use the same mechanism to locate the position of the item
 - in the tree but if we find that the route has not been constructed or the
 - node has the dummy value then that code is not yet in the tree. The way
 - in which the decode algorithm works this should never happen.
 -}

lookup n t = lookup' (dec_to_binx code_bits n) t

lookup' [] (Leaf v)
      = v
lookup' ('0' : bs) (Branch l _)
      = lookup' bs l
lookup' ('1' : bs) (Branch _ r)
      = lookup' bs r
lookup' _  _ = error "tree insert error - seek professional help"

init_table = mk_init_table 0 (Leaf (SOME 99999, '@'))

mk_init_table 256 t = t
mk_init_table n t = mk_init_table (n+1) (insert n (NONE, toEnum n) t)

