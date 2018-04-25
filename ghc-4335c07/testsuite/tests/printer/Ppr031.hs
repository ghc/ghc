{-# LANGUAGE ImplicitParams, NamedFieldPuns, ParallelListComp, PatternGuards #-}
spec :: Spec
spec = do
  describe "split4'8" $ do
    it "0xabc" $ do
      split4'8 0xabc `shouldBe` (0x0a, 0xbc)
    it "0xfff" $ do
      split4'8 0xfff `shouldBe` (0x0f, 0xff)

    describe "(x, y) = split4'8 z" $ do
      prop "x <= 0x0f" $
        \z -> let (x, _) =  split4'8 z in x <= 0x0f
      prop "x << 8 | y == z" $ do
        \z -> let (x, y) = split4'8 z in
          fromIntegral x `shiftL` 8 .|. fromIntegral y == z

match s@Status{ pos, flips, captureAt, captureLen }
  | isOne ?pat = ite (pos .>= strLen) __FAIL__ one
  | otherwise = ite (pos + (toEnum $ minLen ?pat) .> strLen) __FAIL__ $ case ?pat of
    POr ps -> choice flips $ map (\p -> \b -> let ?pat = p in match s{ flips = b }) ps

foo = 1
