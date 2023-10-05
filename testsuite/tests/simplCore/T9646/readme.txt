This is a test for https://gitlab.haskell.org/ghc/ghc/issues/9646

The problem addressed in that ticket was that under some circumstances,
GHC < 7.10.3 was failing to perform eta reduction deterministically.

Compiling this code now (2016/03/16) under ghc-7.8.4 and git HEAD shows that
ghc-7.8.4 produces more complicated code, with a number of extra lambdas which
are completely absent in the fast version.

Git HEAD current produces:

    letrec {
      $wpoly_innerLoop2
      $wpoly_innerLoop2 =
        \ @ s ww ww1 ww2 ww3 ww4 w ->
          case tagToEnum# (<# ww1 dt2) of _ {
            False -> (# w, (W# ww2, W# ww3, W# ww4) #);
            True ->
              case indexWordArray# dt1 ww of w#2 { __DEFAULT ->
              case indexWordArray# dt3 ww1 of w#3 { __DEFAULT ->
              case timesWord2# w#2 w#3 of _ { (# ovf1, prod1 #) ->
              case plusWord2# prod1 ww4 of _ { (# c, s1 #) ->
              case plusWord2# ww3 ovf1 of _ { (# c1, s2 #) ->
              case plusWord2# s2 c of _ { (# c2, s3 #) ->
              $wpoly_innerLoop2
                (-# ww 1#) (+# ww1 1#) (plusWord# ww2 (plusWord# c1 c2)) s3 s1 w
              }
              }
              }
              }
              }
              }
          }; } in ....

whereas ghc-7.8, for the same block produces:

    letrec {
      $wpoly_innerLoop2
      $wpoly_innerLoop2 =
        \ @ s ww ww1 ww2 ww3 ww4 ->
          case tagToEnum# (<# ww1 dt2) of _ {
            False ->
              let {
                sum
                sum = W# ww4 } in
              let {
                carrylo
                carrylo = W# ww3 } in
              let {
                carryhi
                carryhi = W# ww2 } in
              let {
                vx
                vx = (carryhi, carrylo, sum) } in
              (\ eta -> (# eta, vx #)) `cast` ...;
            True ->
              let {
                ds3
                ds3 =
                  case indexWordArray# dt1 ww of w#2 { __DEFAULT ->
                  let {
                    x
                    x = W# w#2 } in
                  (\ eta -> (# eta, x #)) `cast` ...
                  } } in
              let {
                lvl
                lvl =
                  case indexWordArray# dt3 ww1 of w#2 { __DEFAULT ->
                  let {
                    x
                    x = W# w#2 } in
                  (\ eta -> (# eta, x #)) `cast` ...
                  } } in
              let {
                a
                a = -# ww 1 } in
              let {
                a1
                a1 = +# ww1 1 } in
              (\ eta ->
                 case (ds3 `cast` ...) eta of _ { (# ipv, ipv3 #) ->
                 case (lvl `cast` ...) ipv of _ { (# ipv4, ipv5 #) ->
                 case ipv3 of _ { W# a2 ->
                 case ipv5 of _ { W# b ->
                 case timesWord2# a2 b of _ { (# ovf1, prod1 #) ->
                 case plusWord2# prod1 ww4 of _ { (# c, s1 #) ->
                 case plusWord2# ww3 ovf1 of _ { (# c1, s2 #) ->
                 case plusWord2# s2 c of _ { (# c2, s3 #) ->
                 (($wpoly_innerLoop2 a a1 (plusWord# ww2 (plusWord# c1 c2)) s3 s1)
                  `cast` ...)
                   ipv4
                 }
                 }
                 }
                 }
                 }
                 }
                 }
                 })
              `cast` ...
          }; } in ...

I suspect that in the ghc-7.8.4 case, the lambda:

     (\ eta -> (# eta, x #)) `cast` ...

is preventing the inlining of the indexWordArray# operations.

Much of the code for this test was pulled from the primitive package:

	https://hackage.haskell.org/package/primitive
