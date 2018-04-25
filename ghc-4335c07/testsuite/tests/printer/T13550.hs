{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques #-}
module Bug where

$([d| type family Foo a b
      type instance Foo (Maybe a) b = Either (Maybe a) (Maybe b)

      data family Bar a b
      data instance Bar (Maybe a) b = BarMaybe (Maybe a) (Maybe b)
    |])

{-
    type instance Foo (Maybe a) b = Either (Maybe a) (Maybe b)

becomes

[TySynInstD Bug.Foo
  (TySynEqn
    [AppT
      (ConT GHC.Base.Maybe)
      (VarT a_6989586621679027317)
     ,VarT b_6989586621679027318]
    (AppT
      (AppT
        (ConT Data.Either.Either)
        (AppT
          (ConT GHC.Base.Maybe)
          (VarT a_6989586621679027317)
        )
      )
      (AppT (ConT GHC.Base.Maybe) (VarT b_6989586621679027318))
    )
  )
]

    data instance Bar (Maybe a) b = BarMaybe (Maybe a) (Maybe b)

becomes

[DataInstD [] Bug.Bar
  [AppT
    (ConT GHC.Base.Maybe)
    (VarT a_6989586621679027707)
  ,VarT b_6989586621679027708
  ]
  Nothing
  [NormalC
    BarMaybe_6989586621679027706
    [(Bang
        NoSourceUnpackedness
        NoSourceStrictness
      ,AppT
        (ConT GHC.Base.Maybe)
         (VarT a_6989586621679027707)
      )
    ,(Bang
       NoSourceUnpackedness
       NoSourceStrictness
     ,AppT
       (ConT GHC.Base.Maybe)
       (VarT b_6989586621679027708)
     )
    ]
  ]
  []]


-}
