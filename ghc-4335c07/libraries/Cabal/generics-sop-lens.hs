{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified GHC.Generics as GHC
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Typeable
import Generics.SOP
import Generics.SOP.GGP

-- | An example of generic deriving of lens code.
--
-- >>> putStrLn $ genericLenses (Proxy :: Proxy Foobar)
-- fooBar :: Lens' Foobar Int
-- fooBar f s = fmap (\x -> s { T.fooBar = x }) (T.fooBar s)
-- {-# INLINE fooBar #-}
-- <BLANKLINE>
-- fooXyzzy :: Lens' Foobar [[Char]]
-- fooXyzzy f s = fmap (\x -> s { T.fooXyzzy = x }) (T.fooXyzzy s)
-- {-# INLINE fooXyzzy #-}
-- ...
--
-- /Note:/ 'FilePath' i.e @type@ aliases are lost.
--
data Foobar = Foobar
    { fooBar   :: Int
    , fooXyzzy :: [FilePath]
    , fooQuux  :: Bool
    }
  deriving (GHC.Generic)

genericLenses
    :: forall a xs proxy. (GDatatypeInfo a, GCode a ~ '[xs], All Typeable xs)
    => proxy a 
    -> String
genericLenses p = case gdatatypeInfo p of
    Newtype _ _ _                   -> "-- newtype deriving not implemented"
    ADT _ _  (Constructor _ :* Nil) -> "-- fieldnameless deriving not implemented"
    ADT _ _  (Infix _ _ _ :* Nil)   -> "-- infix consturctor deriving not implemented"
    ADT _ dn (Record _ fis :* Nil) ->
        unlines $ concatMap replaceTypes $ hcollapse $ hcmap (Proxy :: Proxy Typeable) derive fis
      where
        derive :: forall x. Typeable x => FieldInfo x -> K [String] x
        derive (FieldInfo fi) = K
            [ fi ++ " :: Lens' " ++ dn ++ " " ++ showsPrec 11 (typeRep (Proxy :: Proxy x)) []
            , fi ++ " f s = fmap (\\x -> s { T." ++ fi ++ " = x }) (f (T." ++  fi ++ " s))"
            , "{-# INLINE " ++ fi ++ " #-}"
            , ""
            ]

genericClassyLenses
    :: forall a xs proxy. (GDatatypeInfo a, GCode a ~ '[xs], All Typeable xs)
    => proxy a 
    -> String
genericClassyLenses p = case gdatatypeInfo p of
    Newtype _ _ _                   -> "-- newtype deriving not implemented"
    ADT _ _  (Constructor _ :* Nil) -> "-- fieldnameless deriving not implemented"
    ADT _ _  (Infix _ _ _ :* Nil)   -> "-- infix consturctor deriving not implemented"
    ADT _ dn (Record _ fis :* Nil) ->
        unlines $ concatMap replaceTypes $
            [[ "class Has" ++ dn ++ " a where"
            , "   " ++ dn' ++ " :: Lens' a " ++ dn
            , ""
            ]] ++
            (hcollapse $ hcmap (Proxy :: Proxy Typeable) deriveCls fis) ++
            [[ ""
            , "instance Has" ++ dn ++ " " ++ dn ++ " where"
            , "    " ++ dn' ++ " = id"
            , "    {-# INLINE " ++ dn' ++ " #-}"
            ]] ++
            (hcollapse $ hcmap (Proxy :: Proxy Typeable) deriveInst fis)
      where
        dn' = case dn of
            []   -> []
            c:cs -> toLower c : cs

        deriveCls :: forall x. Typeable x => FieldInfo x -> K [String] x
        deriveCls (FieldInfo fi) = K
            [ "   " ++ fi ++ " :: Lens' a " ++ showsPrec 11 (typeRep (Proxy :: Proxy x)) []
            , "   " ++ fi ++ " = " ++ dn' ++ " . " ++ fi
            , "   {-# INLINE " ++ fi ++ " #-}"
            , ""
            ]

        deriveInst :: forall x. Typeable x => FieldInfo x -> K [String] x
        deriveInst (FieldInfo fi) = K
            [ "    " ++ fi ++ " f s = fmap (\\x -> s { T." ++ fi ++ " = x }) (f (T." ++  fi ++ " s))"
            , "    {-# INLINE " ++ fi ++ " #-}"
            , ""
            ]

replaceTypes :: [String] -> [String]
replaceTypes = map
    $ replace "[Char]" "String"

replace :: String -> String -> String -> String
replace needle replacement = go where
    go [] = []
    go xs@(x:xs')
        | Just ys <- stripPrefix needle xs = replacement ++ go ys
        | otherwise                        = x : go xs'
