{-# LANGUAGE BlockArguments #-}

module Generator.GeneralCategory (GeneralCategory (..), generateGeneralCategoryCode) where

import Data.ByteString.Builder qualified as BB
import Generator.ByteString (unlinesBB, unlinesBBWithIndent, unwordsBB)
import Generator.RangeSwitch
import Generator.WordEncoding
import Unicode.CharacterDatabase.Parser.UnicodeData (GeneralCategory (Cn))

genEnumBitmap ::
  forall a.
  (Bounded a, Enum a, Show a) =>
  -- | Function name
  BB.Builder ->
  -- | Default value
  a ->
  -- | List of values to encode
  [a] ->
  BB.Builder
genEnumBitmap funcName def as =
  unlinesBB
    [ "{-# INLINE " <> funcName <> " #-}",
      funcName <> " :: Char -> Int",
      funcName
        <> " c = let n = ord c in if n >= "
        <> BB.intDec (length as)
        <> " then "
        <> BB.intDec (fromEnum def)
        <> " else lookup_bitmap n"
    ]

generateHaskellCode :: Int -> [GeneralCategory] -> BB.Builder
generateHaskellCode max_char_length cats =
  let (index_tree, all_allocs) = extract [] range_tree
   in unlinesBB
        [ "{-# NOINLINE lookup_bitmap #-}"
        , "lookup_bitmap :: Int -> Int"
        , "lookup_bitmap n ="
        , "  (" <> (genCode' index_tree 2)
        , "  )"
        , "  where"
        , unlinesBB (fmap genDecompressed (zip all_allocs [0 ..]))
        ]
  where
    range_tree = rangeCases max_char_length cats

    genCode' :: (Enum a, Show a) => RangeTree (Either a Int) -> Int -> BB.Builder
    genCode' (Leaf _ _ cat) _ = BB.string7 $ show cat
    genCode' (Node start _ (Leaf _ endl c_l) (Leaf startr _ c_r)) _ =
      unwordsBB
        [ "({- 1 -} if n <"
        , BB.intDec (endl + 1)
        , "then"
        , "(" <> (genResult start c_l) <> ")"
        , "else"
        , "(" <> (genResult startr c_r) <> "))"
        ]
    genCode' (Node start _ (Leaf _ endl c_l) node_r@(Node _ _ _ _)) indent =
      unlinesBBWithIndent
        (indent * 2)
        [ unwordsBB ["({- 2 -} if n <", BB.intDec (endl + 1), "then", "(" <> (genResult start c_l) <> ") else ("]
        , (genCode' node_r $ indent + 1)
        , "{- 2 -}))"
        ]
    genCode' (Node _ _ node_l@(Node _ _ _ _) (Leaf startr _ c_r)) indent =
      unlinesBBWithIndent
        (indent * 2)
        [ unwordsBB ["({- 3 -} if n >=", BB.intDec startr, "then", "(" <> (genResult startr c_r) <> ") else ("]
        , (genCode' node_l $ indent + 1)
        , "{- 3 -}))"
        ]
    genCode' (Node _ _ node_l@(Node _ endl _ _) node_r@(Node _ _ _ _)) indent =
      unlinesBBWithIndent
        (indent * 2)
        [ unwordsBB ["({- 4 -} if n <", BB.intDec (endl + 1), "then ("]
        , (genCode' node_l $ indent + 1)
        , ") {- 4 -} else ("
        , (genCode' node_r $ indent + 1)
        , "{- 4 -} ))"
        ]

    genResult :: (Enum a, Show a) => Int -> Either a Int -> BB.Builder
    genResult _ (Left s) = BB.string7 $ show (toWord8 s)
    genResult mi (Right idx) = unwordsBB ["lookupIntN", "decompressed_table_" <> (BB.string7 $ show idx), "(n -", BB.string7 (show mi) <> ")"]

    extract :: [[a]] -> RangeTree (Either a [a]) -> (RangeTree (Either a Int), [[a]])
    extract acc (Leaf mi ma (Left v)) = (Leaf mi ma (Left v), acc)
    extract acc (Leaf mi ma (Right v)) = (Leaf mi ma (Right (length acc)), acc ++ [v])
    extract acc (Node mi ma lt rt) =
      let (e_lt, l_acc) = extract acc lt
          (e_rt, r_acc) = extract l_acc rt
      in (Node mi ma e_lt e_rt, r_acc)

    genDecompressed :: forall a. (Enum a, Bounded a, Show a) => ([a], Int) -> BB.Builder
    genDecompressed (acc, idx) =
      let fn_name = "decompressed_table_" <> (BB.string7 $ show idx)
      in "    " <> unwordsBB [fn_name, "=", "\"" <> enumMapToAddrLiteral acc "\"#"]

generateGeneralCategoryCode ::
  -- | -- How to generate module header where first arg us module name
  (BB.Builder -> BB.Builder) ->
  -- | -- Module name
  BB.Builder ->
  -- | -- Max char length
  Int ->
  -- | -- imported general categories for all symbol list
  [GeneralCategory] ->
  BB.Builder
generateGeneralCategoryCode mkModuleHeader moduleName char_length cats =
  unlinesBB
    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE MagicHash #-}"
    , "{-# LANGUAGE TypeApplications #-}"
    , "{-# OPTIONS_HADDOCK hide #-}"
    , ""
    , mkModuleHeader moduleName
    , "module " <> moduleName
    , "(generalCategory)"
    , "where"
    , ""
    , "import GHC.Internal.Base (Char, Int, Ord(..), ord)"
    , "import GHC.Internal.Unicode.Bits (lookupIntN)"
    , "import GHC.Internal.Num ((-))"
    , ""
    , generateHaskellCode char_length cats
    , ""
    , genEnumBitmap "generalCategory" Cn (reverse cats)
    ]
