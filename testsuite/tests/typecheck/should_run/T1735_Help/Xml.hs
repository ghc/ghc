{-# LANGUAGE TemplateHaskell, FlexibleInstances, ScopedTypeVariables,
             GADTs, RankNTypes, FlexibleContexts, TypeSynonymInstances,
             MultiParamTypeClasses, DeriveDataTypeable, PatternGuards,
             OverlappingInstances, UndecidableInstances, CPP #-}

module T1735_Help.Xml (Element(..), Xml, fromXml) where

import T1735_Help.Basics
import T1735_Help.Instances ()
import T1735_Help.State

data Element = Elem String [Element]
             | CData String
             | Attr String String

fromXml :: Xml a => [Element] -> Maybe a
fromXml xs = case readXml xs of
             Just (_, v) -> return v
             Nothing -> error "XXX"

class (Data XmlD a) => Xml a where
    toXml :: a -> [Element]
    toXml = defaultToXml

    readXml :: [Element] -> Maybe ([Element], a)
    readXml = defaultReadXml

    readXml' :: [Element] -> Maybe ([Element], a)
    readXml' = defaultReadXml'

instance (Data XmlD t, Show t) => Xml t

data XmlD a = XmlD { toXmlD :: a -> [Element], readMXmlD :: ReadM Maybe a }

xmlProxy :: Proxy XmlD
xmlProxy = error "xmlProxy"

instance Xml t => Sat (XmlD t) where
    dict = XmlD { toXmlD = toXml, readMXmlD = readMXml }

defaultToXml :: Xml t => t -> [Element]
defaultToXml x = [Elem (constring $ toConstr xmlProxy x) (transparentToXml x)]

transparentToXml :: Xml t => t -> [Element]
transparentToXml x = concat $ gmapQ xmlProxy (toXmlD dict) x

-- Don't do any defaulting here, as these functions can be implemented
-- differently by the user. We do the defaulting elsewhere instead.
-- The t' type is thus not used.

defaultReadXml :: Xml t => [Element] -> Maybe ([Element], t)
defaultReadXml es = readXml' es

defaultReadXml' :: Xml t => [Element] -> Maybe ([Element], t)
defaultReadXml' = readXmlWith readVersionedElement

readXmlWith :: Xml t
            => (Element -> Maybe t)
            -> [Element]
            -> Maybe ([Element], t)
readXmlWith f es = case es of
                       e : es' ->
                           case f e of
                               Just v -> Just (es', v)
                               Nothing -> Nothing
                       [] ->
                           Nothing

readVersionedElement :: forall t . Xml t => Element -> Maybe t
readVersionedElement e = readElement e

readElement :: forall t . Xml t => Element -> Maybe t
readElement (Elem n es) = res
    where resType :: t
          resType = typeNotValue resType
          resDataType = dataTypeOf xmlProxy resType
          con = readConstr resDataType n
          res = case con of
                Just c -> f c
                Nothing -> Nothing
          f c =     let m :: Maybe ([Element], t)
                        m = constrFromElements c es
                    in case m of
                           Just ([], x) -> Just x
                           _ -> Nothing
readElement _ = Nothing

constrFromElements :: forall t . Xml t
                   => Constr -> [Element] -> Maybe ([Element], t)
constrFromElements c es
 = do let st = ReadState { xmls = es }
          m :: ReadM Maybe t
          m = fromConstrM xmlProxy (readMXmlD dict) c
      -- XXX Should we flip the result order?
      (x, st') <- runStateT m st
      return (xmls st', x)

type ReadM m = StateT ReadState m

data ReadState = ReadState {
                     xmls :: [Element]
                 }

getXmls :: Monad m => ReadM m [Element]
getXmls = do st <- get
             return $ xmls st

putXmls :: Monad m => [Element] -> ReadM m ()
putXmls xs = do st <- get
                put $ st { xmls = xs }

readMXml :: Xml a => ReadM Maybe a
readMXml
 = do xs <- getXmls
      case readXml xs of
          Nothing -> fail "Cannot read value"
          Just (xs', v) ->
              do putXmls xs'
                 return v

typeNotValue :: Xml a => a -> a
typeNotValue t = error ("Type used as value: " ++ typeName)
    where typeName = dataTypeName (dataTypeOf xmlProxy t)

-- The Xml [a] context is a bit scary, but if we don't have it then
-- GHC complains about overlapping instances

instance (Xml a {-, Xml [a] -}) => Xml [a] where
    toXml = concatMap toXml
    readXml = f [] []
        where f acc_xs acc_vs [] = Just (reverse acc_xs, reverse acc_vs)
              f acc_xs acc_vs (x:xs) = case readXml [x] of
                                           Just ([], v) ->
                                               f acc_xs (v:acc_vs) xs
                                           _ ->
                                               f (x:acc_xs) acc_vs xs

instance Xml String where
    toXml x = [CData x]
    readXml = readXmlWith f
        where f (CData x) = Just x
              f _ = Nothing

