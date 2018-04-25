module Main where

import Pretty

main = putStr ((ppShow (80::Int) pretty_stuff) ++ "\n")
 where
  pretty_stuff = ppAboves [ ppBesides [ppInt (-42), ppChar '@', ppStr "This is a string"],
			    pp'SP,
			    ppHang (ppStr "This is the label")
				(8::Int) (ppCat (take (50::Int) pp_words)) ]
  pp_words = pp_word : pp_words
  pp_word = ppStr "xxxxx"
