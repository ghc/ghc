{-# LANGUAGE TemplateHaskellQuotes #-}

module T25164_aux where

-- base
import Data.Functor.Identity

-- template-haskell
import Language.Haskell.TH.Syntax

--------------------------------------------------------------------------------

newtype Value a = Value { getValue :: a }

genDoBlock :: Q [ Dec ]
genDoBlock = do
  funNm <- newName "fun"
  argNm <- newName "arg"
  let doBlock =
        DoE Nothing
        [ BindS
            ( ConP 'Value [ ] [ VarP argNm ] )
            ( AppE ( ConE 'Identity ) ( AppE ( ConE 'Value ) ( ConE '() ) ) )
        , NoBindS $
            AppE ( VarE 'pure ) ( VarE argNm )
        ]

  {-
  fun :: Identity ()
  fun = do { Value arg <- Identity ( Value () )
           ; pure arg }
  -}

  pure $
    [ SigD funNm ( AppT ( ConT ''Identity ) ( ConT ''() ) )
    , FunD funNm [ Clause [ ] ( NormalB doBlock ) [ ] ]
    ]
