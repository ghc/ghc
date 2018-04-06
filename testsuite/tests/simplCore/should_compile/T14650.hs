module MergeSort (
  msortBy
 ) where

infixl 7 :%
infixr 6 :&

data LenList a = LL {-# UNPACK #-} !Int Bool [a]

data LenListAnd a b = {-# UNPACK #-} !(LenList a) :% b

data Stack a
  = End
  | {-# UNPACK #-} !(LenList a) :& (Stack a)

msortBy :: (a -> a -> Ordering) -> [a] -> [a]
msortBy cmp = mergeSplit End where
  splitAsc n _ _ _ | n `seq` False = undefined
  splitAsc n as _ [] = LL n True as :% []
  splitAsc n as a bs@(b:bs') = case cmp a b of
    GT -> LL n False as :% bs
    _  -> splitAsc (n + 1) as b bs'

  splitDesc n _ _ _ | n `seq` False = undefined
  splitDesc n rs a [] = LL n True (a:rs) :% []
  splitDesc n rs a bs@(b:bs') = case cmp a b of
    GT -> splitDesc (n + 1) (a:rs) b bs'
    _  -> LL n True (a:rs) :% bs

  mergeLL (LL na fa as) (LL nb fb bs) = LL (na + nb) True $ mergeLs na as nb bs where
    mergeLs nx  _ ny  _ | nx `seq` ny `seq` False = undefined
    mergeLs  0  _ ny ys = if fb then ys else take ny ys
    mergeLs  _ [] ny ys = if fb then ys else take ny ys
    mergeLs nx xs  0  _ = if fa then xs else take nx xs
    mergeLs nx xs  _ [] = if fa then xs else take nx xs
    mergeLs nx xs@(x:xs') ny ys@(y:ys') = case cmp x y of
      GT -> y:mergeLs nx xs (ny - 1) ys'
      _  -> x:mergeLs (nx - 1) xs' ny ys

  push ssx px@(LL nx _ _) = case ssx of
    End -> px :% ssx
    py@(LL ny _ _) :& ssy -> case ssy of
      End
        | nx >= ny -> mergeLL py px :% ssy
      pz@(LL nz _ _) :& ssz
        | nx >= ny || nx + ny >= nz -> case nx > nz of
            False -> push ssy $ mergeLL py px
            _     -> case push ssz $ mergeLL pz py of
              pz' :% ssz' -> push (pz' :& ssz') px
      _ -> px :% ssx

  mergeAll _ px | px `seq` False = undefined
  mergeAll ssx px@(LL nx _ xs) = case ssx of
    End -> xs
    py@(LL _ _ _) :& ssy -> case ssy of
      End -> case mergeLL py px of
        LL _ _ xys -> xys
      pz@(LL nz _ _) :& ssz -> case nx > nz of
        False -> mergeAll ssy $ mergeLL py px
        _     -> case push ssz $ mergeLL pz py of
          pz' :% ssz' -> mergeAll (pz' :& ssz') px

  mergeSplit ss _ | ss `seq` False = undefined
  mergeSplit ss [] = case ss of
    End -> []
    px :& ss' -> mergeAll ss' px
  mergeSplit ss as@(a:as') = case as' of
    [] -> mergeAll ss $ LL 1 True as
    b:bs -> case cmp a b of
      GT -> case splitDesc 2 [a] b bs of
        px :% rs -> case push ss px of
          px' :% ss' -> mergeSplit (px' :& ss') rs
      _  -> case splitAsc 2 as b bs of
        px :% rs -> case push ss px of
          px' :% ss' -> mergeSplit (px' :& ss') rs
  {-# INLINABLE mergeSplit #-}
