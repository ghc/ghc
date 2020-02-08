module T17752 where

-- Should compile to unfoldCStr if the rules fire
isElemLit    x = x `elem`    "theQuickShinyGHCJumpsOverTheRuleRecursion"
isNotElemLit x = x `notElem` "_theQuickShinyGHCCompileJumpsOverTheRuleRecursion"

-- Should compile to a pattern match if the rules fire
isElemList    x = x `elem` ['a','b','c']
isNotElemList x = x `elem` ['x','y','z']

{-

Should the grep tests fail make sure the core still behaves
like the one below (or better!)

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
isElemLit1 = "theQuickShinyGHCJumpsOverTheRuleRecursion"#

-- RHS size: {terms: 20, types: 9, coercions: 0, joins: 0/0}
isElemLit
  = \ x ->
      unpackFoldrCString#
        isElemLit1
        (\ y r ->
           case x of { C# x1 ->
           case y of { C# y1 ->
           case eqChar# x1 y1 of {
             __DEFAULT -> r;
             1# -> True
           }
           }
           })
        False

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
isNotElemLit1
  = "_theQuickShinyGHCCompileJumpsOverTheRuleRecursion"#

-- RHS size: {terms: 25, types: 10, coercions: 0, joins: 0/0}
isNotElemLit
  = \ x ->
      case unpackFoldrCString#
             isNotElemLit1
             (\ y r ->
                case x of { C# x1 ->
                case y of { C# y1 ->
                case eqChar# x1 y1 of {
                  __DEFAULT -> r;
                  1# -> True
                }
                }
                })
             False
      of {
        False -> True;
        True -> False
      }

-- RHS size: {terms: 14, types: 4, coercions: 0, joins: 0/0}
isElemList
  = \ x ->
      case x of { C# x1 ->
      case x1 of {
        __DEFAULT -> False;
        'a'# -> True;
        'b'# -> True;
        'c'# -> True
      }
      }

-- RHS size: {terms: 14, types: 4, coercions: 0, joins: 0/0}
isNotElemList
  = \ x ->
      case x of { C# x1 ->
      case x1 of {
        __DEFAULT -> False;
        'x'# -> True;
        'y'# -> True;
        'z'# -> True
      }
      }
-}
