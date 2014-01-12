{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TH_RichKinds2 where

import Data.Char
import Data.List
import Language.Haskell.TH

$(return [FamilyD TypeFam (mkName "Map") [KindedTV (mkName "f")
                                                   (AppT (AppT ArrowT (VarT (mkName "k1")))
                                                           (VarT (mkName "k2"))),
                                          KindedTV (mkName "l")
                                                   (AppT ListT
                                                         (VarT (mkName "k1")))]
                                          (Just (AppT ListT (VarT (mkName "k2"))))])

$( let fixKs :: String -> String -- need to remove TH renaming index from k variables
       fixKs s =
         case (elemIndex 'k' s) of
           Nothing -> s
           Just i ->
             if i == (length s) || (s !! (i+1) /= '_') then s else
             let (prefix, suffix) = splitAt (i+2) s -- the +2 for the "k_"
                 (index, rest) = span isDigit suffix in
             if length index == 0 then s else
             prefix ++ "0" ++ (fixKs rest)
   in
   do decls <- [d| data SMaybe :: (k -> *) -> (Maybe k) -> * where
                     SNothing :: SMaybe s 'Nothing
                     SJust :: s a -> SMaybe s ('Just a)

                   type instance Map f '[] = '[]
                   type instance Map f (h ': t) = ((f h) ': (Map f t))
                   |]
      reportWarning (fixKs (pprint decls))
      return decls )

data SBool :: Bool -> * where
  SFalse :: SBool 'False
  STrue :: SBool 'True

mbool :: SMaybe SBool ('Just 'False)
mbool = SJust SFalse
