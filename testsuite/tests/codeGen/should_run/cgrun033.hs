{-# LANGUAGE MagicHash #-}
-- !! worker/wrapper turns ( \ <absent> -> Int# ) function
-- !! into Int# -- WRONG

--import PrelBase --ghc1.3
import GHC.Base

main = putStr (shows true_or_false "\n")
  where
    true_or_false
      = case (cmp_name True imp1 imp2) of
	  -1# -> False
	  0#  -> True
	  1#  -> False
   
    imp1 = Imp s "Imp1" s s
    imp2 = Imp s "Imp2" s s

    s = "String!"

-- taken from compiler: basicTypes/ProtoName.lhs

cmp_name :: Bool -> ProtoName -> ProtoName -> Int#

cmp_name by_local (Unk n1) (Unk n2)        = cmpString n1 n2
cmp_name by_local (Unk n1) (Imp m n2 _ o2) = cmpString n1 (if by_local then o2 else n2)
cmp_name by_local (Unk n1) (Prel nm)
  =  let  (_, n2) = getOrigName nm  in
     cmpString n1 n2

cmp_name by_local (Prel n1) (Prel n2) = cmpName n1 n2

cmp_name True  (Imp _ _ _ o1) (Imp _ _ _ o2) = cmpString o1 o2

cmp_name False (Imp m1 n1 _ _) (Imp m2 n2 _ _)
  = case cmpString n1 n2 of {
      -1# -> -1#;
      0# -> case cmpString m1 m2 of {
	       0# -> 0#;
	       xxx -> if null m1 || null m2
		      then 0#
		      else xxx
	     };
      _ -> 1#
    }

cmp_name True (Imp _ _ _ o1) (Prel nm)
  = let
	(_, n2) = getOrigName nm
    in
    cmpString o1 n2

cmp_name False (Imp m1 n1 _ _) (Prel nm)
  = case getOrigName nm   of { (m2, n2) ->
    case cmpString n1 n2 of { -1# -> -1#; 0# -> cmpString m1 m2; _ -> 1# }}

cmp_name by_local other_p1 other_p2
  = case cmp_name by_local other_p2 other_p1 of -- compare the other way around
      -1#  -> 1#
      0#  -> 0#
      _ -> -1#

data ProtoName
  = Unk		String	-- local name in module

  | Imp		String 	-- name of defining module 
		String 	-- name used in defining name
		String  -- name of the module whose interface told me
			-- about this thing
		String 	-- occurrence name

  | Prel	String{-Name-}

cmpString, cmpName :: String -> String -> Int#
cmpString a b = 0#
cmpName = cmpString

getOrigName :: String -> (String, String)
getOrigName x = ("MODULE", x)
