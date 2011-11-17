{-# OPTIONS_GHC -w #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleContexts, FlexibleInstances,
             OverlappingInstances, UndecidableInstances,
             KindSignatures #-}

-- Cut down from a larger core-lint error

module Q where

import Control.Monad (foldM)

data NameId = NameId
data Named name a = Named
data Arg e  = Arg

data Range = Range
data Name = Name
data ALetBinding = ALetBinding
data APattern a = APattern
data CExpr = CExpr
data CPattern = CPattern
data NiceDeclaration = QQ
data TypeError = NotAValidLetBinding NiceDeclaration  
data TCState = TCSt { stFreshThings :: FreshThings }  
data FreshThings = Fresh

newtype NewName a = NewName a
newtype LetDef = LetDef NiceDeclaration  
newtype TCMT (m :: * -> *) a = TCM ()

localToAbstract :: ToAbstract c a => c -> (a -> TCMT IO b) -> TCMT IO b  
localToAbstract = undefined

typeError :: MonadTCM tcm => TypeError -> tcm a  
typeError = undefined

lhsArgs :: [Arg (Named String CPattern)]  
lhsArgs = undefined

freshNoName :: (MonadState s m, HasFresh NameId s) => Range -> m Name  
freshNoName = undefined

class (Monad m) => MonadState s m | m -> s  
class (Monad m) => MonadIO m

class ToAbstract concrete abstract | concrete -> abstract where
    toAbstract :: concrete -> TCMT IO abstract

class (MonadState TCState tcm) => MonadTCM tcm where
    liftTCM :: TCMT IO a -> tcm a

class HasFresh i a where
    nextFresh :: a -> (i,a)

instance ToAbstract c a => ToAbstract [c] [a] where  
instance ToAbstract c a => ToAbstract (Arg c) (Arg a) where  
instance ToAbstract c a => ToAbstract (Named name c) (Named name a) where  
instance ToAbstract CPattern (APattern CExpr) where

instance ToAbstract LetDef [ALetBinding] where
    toAbstract (LetDef d) = do _ <- letToAbstract
                               undefined
        where letToAbstract = do
                  localToAbstract lhsArgs $ \args ->
                          foldM lambda undefined undefined
              lambda _ _ = do x <- freshNoName undefined
                              return undefined
              lambda _ _ = typeError $ NotAValidLetBinding d

instance HasFresh NameId FreshThings where
    nextFresh = undefined

instance HasFresh i FreshThings => HasFresh i TCState where
    nextFresh = undefined

instance Monad m => MonadState TCState (TCMT m) where

instance Monad m => MonadTCM (TCMT m) where
    liftTCM = undefined

instance Monad (TCMT m) where
    return = undefined
    (>>=) = undefined
    fail = undefined

instance Monad m => MonadIO (TCMT m) where

