{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, TypeFamilies, GADTs, PartialTypeSignatures #-}

module T13035 where

newtype MyAttr a b = MyAttr { _unMyAttr :: MyFun (a b) }
type MyRec a b = Rec (MyAttr a) b

type family MyFun (a :: k1) :: k2

data GY (a :: k1) (b :: k2) (c :: k1 -> k3) (d :: k1)
data GNone (a :: k1)

type family GYTF a where
    GYTF (GY a b _ a) = b
    GYTF (GY _ _ c d) = MyFun (c d)

type instance MyFun (GY a b c d) = GYTF (GY a b c d)

type family GNoneTF (a :: k1) :: k2 where

type instance MyFun (GNone a) = GNoneTF a

type (a :: k1) =: (b :: k2) = a `GY` b
type (a :: j1 -> j2) $ (b :: j1) = a b

infixr 0 $
infixr 9 =:

data FConst (a :: *) (b :: Fields)
data FApply (a :: * -> * -> *) b c (d :: Fields)
data FMap (a :: * -> *) b (d :: Fields)

type instance MyFun (FConst a b) = a
type instance MyFun (FApply b c d a) = b (MyFun (c a)) (MyFun (d a))
type instance MyFun (FMap b c a) = b (MyFun (c a))

data Fields = Name
            | Author
            | Image
            | Description
            | Ingredients
            | Instructions
            | CookTime
            | PrepTime
            | TotalTime
            | Yield
            | Nutrition
            | Tags
            | Url
            | Section
            | Items
            | Subsections
            | Calories
            | Carbohydrates
            | Cholesterol
            | Fat
            | Fiber
            | Protien
            | SaturatedFat
            | Sodium
            | Sugar
            | TransFat
            | UnsaturatedFat
            | ServingSize

data Rec :: (u -> *) -> [u] -> * where
  RNil :: Rec f '[]
  (:&) :: !(f r) -> !(Rec f rs) -> Rec f (r ': rs)

data family Sing (a :: k)
data instance Sing (z_a6bn :: Fields)
      = z_a6bn ~ Name => SName |
        z_a6bn ~ Author => SAuthor |
        z_a6bn ~ Image => SImage |
        z_a6bn ~ Description => SDescription |
        z_a6bn ~ Ingredients => SIngredients |
        z_a6bn ~ Instructions => SInstructions |
        z_a6bn ~ CookTime => SCookTime |
        z_a6bn ~ PrepTime => SPrepTime |
        z_a6bn ~ TotalTime => STotalTime |
        z_a6bn ~ Yield => SYield |
        z_a6bn ~ Nutrition => SNutrition |
        z_a6bn ~ Tags => STags |
        z_a6bn ~ Url => SUrl |
        z_a6bn ~ Section => SSection |
        z_a6bn ~ Items => SItems |
        z_a6bn ~ Subsections => SSubsections |
        z_a6bn ~ Calories => SCalories |
        z_a6bn ~ Carbohydrates => SCarbohydrates |
        z_a6bn ~ Cholesterol => SCholesterol |
        z_a6bn ~ Fat => SFat |
        z_a6bn ~ Fiber => SFiber |
        z_a6bn ~ Protien => SProtien |
        z_a6bn ~ SaturatedFat => SSaturatedFat |
        z_a6bn ~ Sodium => SSodium |
        z_a6bn ~ Sugar => SSugar |
        z_a6bn ~ TransFat => STransFat |
        z_a6bn ~ UnsaturatedFat => SUnsaturatedFat |
        z_a6bn ~ ServingSize => SServingSize

(=::) :: sing f -> MyFun (a f) -> MyAttr a f
_ =:: x = MyAttr x

type NutritionT
    = Calories       =: Maybe Int
    $ Carbohydrates  =: Maybe Int
    $ Cholesterol    =: Maybe Int
    $ Fat            =: Maybe Int
    $ Fiber          =: Maybe Int
    $ Protien        =: Maybe Int
    $ SaturatedFat   =: Maybe Int
    $ Sodium         =: Maybe Int
    $ Sugar          =: Maybe Int
    $ TransFat       =: Maybe Int
    $ UnsaturatedFat =: Maybe Int
    $ ServingSize    =: String
    $ GNone

type NutritionRec = MyRec NutritionT ['Calories, 'Carbohydrates,
                                      'Cholesterol, 'Fat, 'Fiber,
                                      'Protien, 'SaturatedFat, 'Sodium,
                                      'Sugar, 'TransFat, 'UnsaturatedFat,
                                      'ServingSize]

type RecipeT
    = Name         =: String
    $ Author       =: String
    $ Image        =: String
    $ Description  =: String
    $ CookTime     =: Maybe Int
    $ PrepTime     =: Maybe Int
    $ TotalTime    =: Maybe Int
    $ Yield        =: String
    $ Nutrition    =: NutritionRec
    $ Tags         =: [String]
    $ Url          =: String
    $ GNone

type RecipeFormatter = FApply (->) (FConst [String]) (FMap IO RecipeT)

g :: MyRec RecipeFormatter _ --'[ 'Author ] Uncomment to prevent loop
g = SAuthor =:: (\a -> return "Hi")
    :& RNil
