{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module OkGivenTypeError where

import GHC.TypeLits

badShow :: Show (TypeError ('Text "Bad show!")) => ()
badShow = ()

