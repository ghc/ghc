{-# LANGUAGE CPP, TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, 
      FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances,
      TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HSX.XMLGenerator
-- Copyright   :  (c) Niklas Broberg 2008
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, niklas.broberg@chalmers.se
-- Stability   :  experimental
-- Portability :  requires newtype deriving and MPTCs with fundeps
--
-- The class and monad transformer that forms the basis of the literal XML
-- syntax translation. Literal tags will be translated into functions of
-- the GenerateXML class, and any instantiating monads with associated XML
-- types can benefit from that syntax.
-----------------------------------------------------------------------------
module T4809_XMLGenerator where

import Control.Monad.Trans
import Control.Monad.Cont  (MonadCont)
import Control.Monad.Error (MonadError)
import Control.Monad.Reader(MonadReader)
import Control.Monad.Writer(MonadWriter)
import Control.Monad.State (MonadState)
import Control.Monad.RWS   (MonadRWS)
import Control.Monad (MonadPlus(..),liftM)

----------------------------------------------
-- General XML Generation

-- | The monad transformer that allows a monad to generate XML values.
newtype XMLGenT m a = XMLGenT (m a)
  deriving (Monad, Functor, MonadIO, MonadPlus, MonadWriter w, MonadReader r,
            MonadState s, MonadRWS r w s, MonadCont, MonadError e)

-- | un-lift.
unXMLGenT :: XMLGenT m a -> m a
unXMLGenT   (XMLGenT ma) =  ma

instance MonadTrans XMLGenT where
 lift = XMLGenT

type Name = (Maybe String, String)

-- | Generate XML values in some XMLGenerator monad.
class Monad m => XMLGen m where
 type XML m
 data Child m
 genElement  :: Name -> [XMLGenT m [Int]] -> [XMLGenT m [Child m]] -> XMLGenT m (XML m)
 genEElement :: Name -> [XMLGenT m [Int]]                          -> XMLGenT m (XML m)
 genEElement n ats = genElement n ats []

-- | Embed values as child nodes of an XML element. The parent type will be clear
-- from the context so it is not mentioned.
class XMLGen m => EmbedAsChild m c where
 asChild :: c -> XMLGenT m [Child m]

instance (MonadIO m, EmbedAsChild m c, m ~ n) => EmbedAsChild m (XMLGenT n c) where
 asChild m = do
      liftIO $ putStrLn "EmbedAsChild m (XMLGenT n c)"
      a <- m
      asChild a

instance (MonadIO m, EmbedAsChild m c) => EmbedAsChild m [c] where
  asChild cs = 
      do liftIO $ putStrLn "EmbedAsChild m [c]"
         liftM concat . mapM asChild $ cs

instance (MonadIO m, XMLGen m) => EmbedAsChild m (Child m) where
 asChild c = 
     do liftIO $ putStrLn "EmbedAsChild m (Child m)"
        return . return $ c 
