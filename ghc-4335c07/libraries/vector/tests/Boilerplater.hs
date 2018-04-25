module Boilerplater where

import Test.Framework.Providers.QuickCheck2

import Language.Haskell.TH


testProperties :: [Name] -> Q Exp
testProperties nms = fmap ListE $ sequence [[| testProperty $(stringE prop_name) $(varE nm) |]
                                           | nm <- nms
                                           , Just prop_name <- [stripPrefix_maybe "prop_" (nameBase nm)]]

-- This nice clean solution doesn't quite work since I need to use lexically-scoped type
-- variables, which aren't supported by Template Haskell. Argh!
-- testProperties :: Q [Dec] -> Q Exp
-- testProperties mdecs = do
--     decs <- mdecs
--     property_exprs <- sequence [[| testProperty "$prop_name" $(return $ VarE nm) |]
--                                | FunD nm _clauses <- decs
--                                , Just prop_name <- [stripPrefix_maybe "prop_" (nameBase nm)]]
--     return $ LetE decs (ListE property_exprs)

stripPrefix_maybe :: String -> String -> Maybe String
stripPrefix_maybe prefix what
  | what_start == prefix = Just what_end
  | otherwise            = Nothing
  where (what_start, what_end) = splitAt (length prefix) what
