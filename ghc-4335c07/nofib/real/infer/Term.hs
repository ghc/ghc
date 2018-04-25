module Term
  (VarId, Term (Var, Abs, App, Let), readsId)
where

import Parse
import Shows
type  VarId   =  String
data  Term    =  Var VarId
              |  Abs VarId Term
              |  App Term Term
              |  Let VarId Term Term
instance Show Term where
      showsPrec d  =  showsTerm d
instance Read Term where
      readsPrec d  =  readsTerm
readsTerm, readsAbs, readsAtomics, readsAtomic, readsVar :: Parses Term
readsTerm     =       readsAbs
              `elseP` readsLet
              `elseP` readsAtomics
readsAtomic   =       readsVar
              `elseP` parenP readsTerm
readsAbs      =       lexP "\\"               `thenP` (\_  ->
                      plusP readsId           `thenP` (\xs ->
                      lexP "."                `thenP` (\_  ->
                      readsTerm               `thenP` (\v  ->
                                              returnP (foldr Abs v xs)))))
readsLet      =       lexP "let"              `thenP` (\_ ->
                      readsId                 `thenP` (\x ->
                      lexP "="                `thenP` (\_ ->
                      readsTerm               `thenP` (\u ->
                      lexP "in"               `thenP` (\_ ->
                      readsTerm               `thenP` (\v ->
                                              returnP (Let x u v)))))))
readsAtomics  =       readsAtomic             `thenP` (\t  ->
                      starP readsAtomic       `thenP` (\ts ->
                                              returnP (foldl App t ts)))
readsVar      =       readsId                 `thenP` (\x ->
                                              returnP (Var x))
readsId       :: Parses String
readsId       =  lexicalP (isntKeyword `filterP` plusP alphaP)
                 where  isntKeyword x  =  (x /= "let" && x /= "in")
showsTerm                     :: Int -> Shows Term
showsTerm d (Var x)           =  showsString x
showsTerm d (Abs x v)         =  showsParenIf (d>0)
                                 (showsString "\\" . showsString x . showsAbs v)
showsTerm d (App t u)         =  showsParenIf (d>1)
                                 (showsTerm 1 t . showsChar ' ' . showsTerm 2 u)
showsTerm d (Let x u v)       =  showsParenIf (d>0)
                                 (showsString "let  "   . showsString x .
                                  showsString " = "     . showsTerm 1 u .
                                  showsString "  in  "  . showsTerm 0 v)
showsAbs                      :: Shows Term
showsAbs (Abs x t)            =  showsString " " . showsString x . showsAbs t
{- ELSE -}
showsAbs t                    =  showsString ". " . showsTerm 0 t
