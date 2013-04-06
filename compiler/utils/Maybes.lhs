%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}
module Maybes (
        module Data.Maybe,

        MaybeErr(..), -- Instance of Monad
        failME, isSuccess,

        orElse,
        mapCatMaybes,
        allMaybes,
        firstJust, firstJusts,
        expectJust,
        maybeToBool,

        MaybeT(..)
    ) where

import Data.Maybe

infixr 4 `orElse`
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Maybe type]{The @Maybe@ type}
%*                                                                      *
%************************************************************************

\begin{code}
maybeToBool :: Maybe a -> Bool
maybeToBool Nothing  = False
maybeToBool (Just _) = True

-- | Collects a list of @Justs@ into a single @Just@, returning @Nothing@ if
-- there are any @Nothings@.
allMaybes :: [Maybe a] -> Maybe [a]
allMaybes [] = Just []
allMaybes (Nothing : _)  = Nothing
allMaybes (Just x  : ms) = case allMaybes ms of
                           Nothing -> Nothing
                           Just xs -> Just (x:xs)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing  b = b

-- | Takes a list of @Maybes@ and returns the first @Just@ if there is one, or
-- @Nothing@ otherwise.
firstJusts :: [Maybe a] -> Maybe a
firstJusts = foldr firstJust Nothing
\end{code}

\begin{code}
expectJust :: String -> Maybe a -> a
{-# INLINE expectJust #-}
expectJust _   (Just x) = x
expectJust err Nothing  = error ("expectJust " ++ err)
\end{code}

\begin{code}
mapCatMaybes :: (a -> Maybe b) -> [a] -> [b]
mapCatMaybes _ [] = []
mapCatMaybes f (x:xs) = case f x of
                        Just y  -> y : mapCatMaybes f xs
                        Nothing -> mapCatMaybes f xs
\end{code}

\begin{code}
-- | flipped version of @fromMaybe@.
orElse :: Maybe a -> a -> a
(Just x) `orElse` _ = x
Nothing  `orElse` y = y
\end{code}

%************************************************************************
%*                                                                      *
\subsection[MaybeT type]{The @MaybeT@ monad transformer}
%*                                                                      *
%************************************************************************

\begin{code}

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f x = MaybeT $ fmap (fmap f) $ runMaybeT x

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
  fail _ = MaybeT $ return Nothing

\end{code}


%************************************************************************
%*                                                                      *
\subsection[MaybeErr type]{The @MaybeErr@ type}
%*                                                                      *
%************************************************************************

\begin{code}
data MaybeErr err val = Succeeded val | Failed err

instance Monad (MaybeErr err) where
  return v = Succeeded v
  Succeeded v >>= k = k v
  Failed e    >>= _ = Failed e

isSuccess :: MaybeErr err val -> Bool
isSuccess (Succeeded {}) = True
isSuccess (Failed {})    = False

failME :: err -> MaybeErr err val
failME e = Failed e
\end{code}
