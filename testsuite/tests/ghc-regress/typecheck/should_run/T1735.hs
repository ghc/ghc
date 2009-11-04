{-# LANGUAGE TemplateHaskell, FlexibleInstances, ScopedTypeVariables,
             ScopedTypeVariables, GADTs, RankNTypes, FlexibleContexts,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving,
             DeriveDataTypeable,
             OverlappingInstances, UndecidableInstances, CPP #-}

module Main (main) where

import T1735_Help.Basics
import T1735_Help.Xml

data YesNo = Yes | No
    deriving (Eq, Show, Typeable)
instance Sat (ctx YesNo) => Data ctx YesNo where
    toConstr _ Yes = yesConstr
    toConstr _ No  = noConstr
    gunfold _ _ z c  = case constrIndex c of
                           1 -> z Yes
                           2 -> z No
                           _ -> error "Foo"
    dataTypeOf _ _ = yesNoDataType
yesConstr :: Constr
yesConstr = mkConstr yesNoDataType "Yes" [] Prefix
noConstr :: Constr
noConstr = mkConstr yesNoDataType "No" [] Prefix
yesNoDataType :: DataType
yesNoDataType = mkDataType "YesNo" [yesConstr, noConstr]

newtype MyList a = MkMyList { unMyList :: [a] }
    deriving (Show, Eq, Typeable)
instance (Sat (ctx (MyList a)), Sat (ctx [a]), Data ctx a)
      => Data ctx (MyList a) where
    gfoldl _ f z x  = z MkMyList `f` unMyList x
    toConstr _ (MkMyList _) = mkMyListConstr
    gunfold _ k z c  = case constrIndex c of
                           1 -> k (z MkMyList)
                           _ -> error "Foo"
    dataTypeOf _ _ = myListDataType
mkMyListConstr :: Constr
mkMyListConstr = mkConstr myListDataType "MkMyList" [] Prefix
myListDataType :: DataType
myListDataType = mkDataType "MyList" [mkMyListConstr]

#ifdef FOO
rigidTests :: Maybe (Maybe [YesNo])
rigidTests =
 mkTest [Elem "No"  []] (Just [No])
#endif

rigidManualTests :: Maybe (Maybe (MyList YesNo))
rigidManualTests =
 mkTest [Elem "MkMyList" [Elem "Yes" []]] (Just (MkMyList [Yes]))

mkTest :: (Eq a, Xml a) => [Element] -> Maybe a -> Maybe (Maybe a)
mkTest es v = case fromXml es of
                  v' | v == v'   -> Nothing
                     | otherwise -> Just v'

main :: IO ()
main = print rigidManualTests

