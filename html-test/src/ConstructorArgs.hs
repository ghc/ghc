{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE GADTs, PatternSynonyms #-}

module ConstructorArgs (Foo(..), Boo(Foo, Foa, Fo, Fo'), pattern Bo, pattern Bo') where

data Foo
  = Rec             -- ^ doc on a record
     { x :: String  -- ^ doc on the `String` field of `Rec`
     , y :: String  -- ^ doc on the `String` field of `Rec`
     }
   | Baz Int String  -- ^ old prefix doc style
   | Boa             -- ^ doc on the `Boa` constrictor
       !Int          -- ^ doc on the `Int` field of `Boa`
       !String       -- ^ doc on the `String` field of `Boa`
   | Int :| String   -- ^ old infix doc style
   | Int             -- ^ doc on the `Int` field of the `:*` constructor
       :*            -- ^ doc on the `:*` constructor
     String          -- ^ doc on the `String` field of the `:*` constructor

infixr 1 `Foo`
infixr 2 `Boa`
infixr 3 :*

data Boo where
  -- | Info about a 'Foo'
  Foo :: Int    -- ^ `Int` field of `Foo`
      -> String -- ^ `String` field of `Foo`
      -> Boo    -- ^ Make a `Boo`

  -- | no argument docs GADT
  Foa :: Int -> Boo

infixr 4 `Boo`

-- | Info about bundled 'Fo'
pattern Fo :: Int    -- ^ an 'Int'
           -> String -- ^ a 'String'
           -> Boo    -- ^ a 'Boo'
pattern Fo x y = Foo x y

-- | Bundled and no argument docs
pattern Fo' :: Boo
pattern Fo' = Foo 1 "hi"

infixr 5 `Fo`

-- | Info about not-bundled 'Bo'
pattern Bo :: Int    -- ^ an 'Int'
           -> String -- ^ a 'String'
           -> Boo -- ^ a 'Boo' pattern
pattern Bo x y = Foo x y

-- | Not bunded and no argument docs
pattern Bo' :: Int -> String -> Boo
pattern Bo' x y = Foo x y

infixr 6 `Bo`
