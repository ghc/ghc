{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TH_PromotedList where

import Language.Haskell.TH

$(let ty = AppT (AppT PromotedConsT (ConT ''Int))
            (AppT (AppT PromotedConsT (ConT ''Bool)) PromotedNilT)

  in  reportWarning (pprint ty) >>
        return [])

data Proxy a = Proxy

f :: Proxy (True ': $(appT (appT promotedConsT (conT 'False)) promotedNilT))
f = Proxy :: Proxy ('[True, False] :: [Bool])
