module T15630a where

data IValue = IDefault
            | IInt Int
            | IBlob String

(?) :: (IValue -> Either x a) -> IValue -> Either x (Maybe a)
-- With this NOINLINE pragma we get good behaviour, but disastrous without
-- {-# NOINLINE [0] (?) #-}
(?) _ IDefault = pure Nothing
(?) p x        = Just <$> p x

getInt :: IValue -> Either () Int
{-# NOINLINE getInt #-}
getInt (IInt i) = Right i
getInt v = Left ()

getString :: IValue -> Either () String
{-# NOINLINE getString #-}
getString (IBlob b) = Right $ b
getString v = Left ()

(<+>) :: (Either x (a -> b), [IValue]) -> (IValue -> Either x a) -> (Either x b, [IValue])
(<+>) (f, (v:vs)) p = (f <*> (p v), vs)

data TestStructure = TestStructure
    { _param1 :: Int
    , _param2 :: Maybe String
    , _param3 :: Maybe Int
    , _param4 :: Maybe String
    , _param5 :: Maybe Int
    , _param6 :: Maybe Int

    , _param7 :: Maybe String
    , _param8 :: Maybe String
    , _param9 :: Maybe Int
    , _param10 :: Maybe Int
    , _param11 :: Maybe String
    , _param12 :: Maybe String
    , _param13 :: Maybe Int
    , _param14 :: Maybe Int
    , _param15 :: Maybe String

    }

getMenuItem :: [IValue] -> Either () TestStructure
getMenuItem vs = fst $ (pure TestStructure, vs)
             <+> getInt
             <+> (getString ?)
             <+> (getInt ?)
             <+> (getString ?)
             <+> (getInt ?)
             <+> (getInt ?)

             <+> (getString ?)
             <+> (getString ?)
             <+> (getInt ?)
             <+> (getInt ?)
             <+> (getString ?)
             <+> (getString ?)
             <+> (getInt ?)
             <+> (getInt ?)
             <+> (getString ?)

