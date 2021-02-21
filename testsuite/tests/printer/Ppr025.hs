{-# LANGUAGE Arrows #-}

operator = describe "Operators on ProcessA"$
  do
    describe "feedback" $
      do
        it "acts like local variable with hold." $
          do
            let
                foo = bar $
                  do
                    return 4
                pa = proc evx ->
                  do
                    (\evy -> hold 10 -< evy)
                      `feedback` \y ->
                      do
                        returnA -< ((+y) <$> evx, (y+1) <$ evx)
            run pa [1, 2, 3] `shouldBe` [11, 13, 15]

        it "correctly handles stream end." $
          do
            let
                pa = proc x ->
                    (\asx -> returnA -< asx)
                  `feedback`
                    (\asy -> returnA -< (asy::Event Int, x))
                comp = mkProc (PgPush PgStop) >>> pa
            stateProc comp [0, 0] `shouldBe` ([], [0])

        it "correctly handles stream end.(2)" $
          do
            pendingWith "many utilities behave incorrectly at end of stream."
