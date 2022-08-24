{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

module GHC.Hs.Extension.Ppr where

import GHC.TypeLits (KnownSymbol, symbolVal)

import Data.Data hiding ( Fixity )
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Utils.Outputable hiding ((<>))
import GHC.Types.SrcLoc ( GenLocated(..) )
import GHC.Parser.Annotation

-- |Constraint type to bundle up the requirement for 'OutputableBndr' on both
-- the @id@ and the 'NoGhcTc' of it. See Note [NoGhcTc].
type OutputableBndrId pass =
  ( OutputableBndr (IdGhcP pass)
  , OutputableBndr (IdGhcP (NoGhcTcPass pass))
  , Outputable (GenLocated (Anno (IdGhcP pass)) (IdGhcP pass))
  , Outputable (GenLocated (Anno (IdGhcP (NoGhcTcPass pass))) (IdGhcP (NoGhcTcPass pass)))
  , IsPass pass
  )

-- useful helper functions:
pprIfPs :: forall p. IsPass p => (p ~ 'Parsed => SDoc) -> SDoc
pprIfPs pp = case ghcPass @p of GhcPs -> pp
                                _     -> empty

pprIfRn :: forall p. IsPass p => (p ~ 'Renamed => SDoc) -> SDoc
pprIfRn pp = case ghcPass @p of GhcRn -> pp
                                _     -> empty

pprIfTc :: forall p. IsPass p => (p ~ 'Typechecked => SDoc) -> SDoc
pprIfTc pp = case ghcPass @p of GhcTc -> pp
                                _     -> empty

type instance Anno (HsToken tok) = TokenLocation

noHsTok :: GenLocated TokenLocation (HsToken tok)
noHsTok = L NoTokenLoc HsTok

type instance Anno (HsUniToken tok utok) = TokenLocation

noHsUniTok :: GenLocated TokenLocation (HsUniToken tok utok)
noHsUniTok = L NoTokenLoc HsNormalTok

--- Outputable

instance Outputable NoExtField where
  ppr _ = text "NoExtField"

instance Outputable DataConCantHappen where
  ppr = dataConCantHappen

instance KnownSymbol tok => Outputable (HsToken tok) where
   ppr _ = text (symbolVal (Proxy :: Proxy tok))

instance (KnownSymbol tok, KnownSymbol utok) => Outputable (HsUniToken tok utok) where
   ppr HsNormalTok  = text (symbolVal (Proxy :: Proxy tok))
   ppr HsUnicodeTok = text (symbolVal (Proxy :: Proxy utok))
