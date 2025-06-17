{-# LANGUAGE GHC2021 #-}
module T11397 where


f :: a -> [Maybe a]
f x =
   let switch l = [l Nothing, l (Just x)]
   in  switch id

u :: a
u = u

f2 :: a
f2 = let switch l = l u in switch u


f3 :: a
f3 = let switch l = l undefined in switch undefined


newtype VectorLazy a = VectorLazy a
newtype Vector a = Vector a
newtype Pointer a = Pointer a

empty :: VectorLazy a
empty = undefined

cons :: Vector a -> Pointer a
cons = undefined

unfoldrResult :: (a -> Either c (b, a)) -> a -> (VectorLazy b, c)
unfoldrResult = undefined

switchL :: b -> (a -> Pointer a -> b) -> Pointer a -> b
switchL = undefined

inverseFrequencyModulationChunk ::
   (Num t, Ord t) =>
   (s -> Maybe (t,s)) -> (t,s) -> Vector v -> (VectorLazy v, Maybe (t,s))
inverseFrequencyModulationChunk nextC (phase,cst0) chunk =
   let {-
       switch ::
          (Maybe (t, s) -> r) ->
          ((t, v) -> (s, Pointer v) -> r) ->
          t ->
          (s, Pointer v) -> r
       -}
       switch l r t (cp0,xp0) =
          maybe
             (l Nothing)
             (\(c1,cp1) ->
                switchL
                   (l (Just (t,cp0)))
                   (\x1 xp1 -> r (t+c1,x1) (cp1,xp1))
                   xp0)
             (nextC cp0)

       {-
       go ::
          (t,v) -> (s, Pointer v) ->
          Either (Maybe (t,s)) (v, ((t,v), (s, Pointer v)))
       -}
       go (c,x) cxp =
          if c<1
            then switch Left go c cxp
            else Right (x, ((c-1,x),cxp))

   in  switch ((,) empty)
          (curry $ unfoldrResult (uncurry go))
          phase (cst0, cons chunk)
