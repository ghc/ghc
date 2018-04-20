module ConstructorFields where

data Foo
  = Bar Int String  -- ^ doc on `Bar` constructor

  | Baz             -- ^ doc on the `Baz` constructor
      Int           -- ^ doc on the `Int` field of `Baz`
      String        -- ^ doc on the `String` field of `Baz`

  | Int :+ String   -- ^ doc on the `:+` constructor

  | Int             -- ^ doc on the `Int` field of the `:*` constructor
      :*            -- ^ doc on the `:*` constructor
    String          -- ^ doc on the `String` field of the `:*` constructor

  | Boo { x :: () } -- ^ doc on the `Boo` record constructor

  | Boa             -- ^ doc on the `Boa` record constructor
      { y :: () }
