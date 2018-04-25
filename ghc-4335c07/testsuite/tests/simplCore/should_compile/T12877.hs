module Bug where
-- This ugly cascading case reduces to:
--    case x of
--       0 -> "0"
--       1 -> "1"
--       _ -> "n"
--
-- but only if GHC's case-folding reduction kicks in.

{-# NOINLINE test #-}
test :: Word -> String
test x = case x of
   0  -> "0"
   1  -> "1"
   t  -> case t + 1 of
      1 -> "0"
      2 -> "1"
      t  -> case t + 1 of
         2 -> "0"
         3 -> "1"
         t  -> case t + 1 of
            3 -> "0"
            4 -> "1"
            t  -> case t + 1 of
               4 -> "0"
               5 -> "1"
               t  -> case t + 1 of
                  5 -> "0"
                  6 -> "1"
                  t  -> case t + 1 of
                     6 -> "0"
                     7 -> "1"
                     t  -> case t + 1 of
                        7 -> "0"
                        8 -> "1"
                        t  -> case t + 1 of
                           8 -> "0"
                           9 -> "1"
                           t  -> case t + 1 of
                              10 -> "0"
                              11 -> "1"
                              t  -> case t + 1 of
                                 11 -> "0"
                                 12 -> "1"
                                 t  -> case t + 1 of
                                    12 -> "0"
                                    13 -> "1"
                                    t  -> case t + 1 of
                                       13 -> "0"
                                       14 -> "1"
                                       t  -> case t + 1 of
                                          14 -> "0"
                                          15 -> "1"
                                          t  -> case t + 1 of
                                             15 -> "0"
                                             16 -> "1"
                                             t  -> case t + 1 of
                                                16 -> "0"
                                                17 -> "1"
                                                t  -> case t + 1 of
                                                   17 -> "0"
                                                   18 -> "1"
                                                   t  -> case t + 1 of
                                                      18 -> "0"
                                                      19 -> "1"
                                                      t  -> case t + 1 of
                                                         19 -> "0"
                                                         20 -> "1"
                                                         t  -> case t + 1 of
                                                            20 -> "0"
                                                            21 -> "1"
                                                            t  -> case t + 1 of
                                                               21 -> "0"
                                                               22 -> "1"
                                                               t  -> case t + 1 of
                                                                  22 -> "0"
                                                                  23 -> "1"
                                                                  t  -> case t + 1 of
                                                                     23 -> "0"
                                                                     24 -> "1"
                                                                     t  -> case t + 1 of
                                                                        24 -> "0"
                                                                        25 -> "1"
                                                                        t  -> case t + 1 of
                                                                           25 -> "0"
                                                                           26 -> "1"
                                                                           t  -> case t + 1 of
                                                                              26 -> "0"
                                                                              27 -> "1"
                                                                              t  -> case t + 1 of
                                                                                 27 -> "0"
                                                                                 28 -> "1"
                                                                                 t  -> case t + 1 of
                                                                                    28 -> "0"
                                                                                    29 -> "1"
                                                                                    t  -> case t + 1 of
                                                                                       29 -> "0"
                                                                                       30 -> "1"
                                                                                       t  -> case t + 1 of
                                                                                          30 -> "0"
                                                                                          31 -> "1"
                                                                                          t  -> case t + 1 of
                                                                                             31 -> "0"
                                                                                             32 -> "1"
                                                                                             t  -> case t + 1 of
                                                                                                32 -> "0"
                                                                                                33 -> "1"
                                                                                                t  -> case t + 1 of
                                                                                                   33 -> "0"
                                                                                                   34 -> "1"
                                                                                                   t  -> case t + 1 of
                                                                                                      34 -> "0"
                                                                                                      35 -> "1"
                                                                                                      _  -> "n"
