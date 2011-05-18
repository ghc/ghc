
-- Test that we don't have rules (or othre optimisations) doing the
-- wrong thing for constant folding with Doubles.

module Main (main) where

zero :: Double
zero = 0

one :: Double
one = 1

nan :: Double
nan = 0 / 0

inf :: Double
inf = 1 / 0

neginf :: Double
neginf = -1 / 0

fzero :: Float
fzero = 0

fone :: Float
fone = 1

fnan :: Float
fnan = 0 / 0

finf :: Float
finf = 1 / 0

fneginf :: Float
fneginf = -1 / 0

main :: IO ()
main = do putStrLn "=== Subtraction ==="
          print (zero - zero)
          print (zero - nan)
          print (zero - inf)
          print (zero - neginf)
          print (nan - zero)
          print (nan - nan)
          print (nan - inf)
          print (nan - neginf)
          print (inf - zero)
          print (inf - nan)
          print (inf - inf)
          print (inf - neginf)
          print (neginf - zero)
          print (neginf - nan)
          print (neginf - inf)
          print (neginf - neginf)
          putStrLn "=== Addition ==="
          print (zero + zero)
          print (zero + nan)
          print (zero + inf)
          print (zero + neginf)
          print (nan + zero)
          print (nan + nan)
          print (nan + inf)
          print (nan + neginf)
          print (inf + zero)
          print (inf + nan)
          print (inf + inf)
          print (inf + neginf)
          print (neginf + zero)
          print (neginf + nan)
          print (neginf + inf)
          print (neginf + neginf)
          putStrLn "=== Mutiplication ==="
          print (zero * zero)
          print (zero * one)
          print (zero * nan)
          print (zero * inf)
          print (zero * neginf)
          print (one * zero)
          print (one * one)
          print (one * nan)
          print (one * inf)
          print (one * neginf)
          print (nan * zero)
          print (nan * one)
          print (nan * nan)
          print (nan * inf)
          print (nan * neginf)
          print (inf * zero)
          print (inf * one)
          print (inf * nan)
          print (inf * inf)
          print (inf * neginf)
          print (neginf * zero)
          print (neginf * one)
          print (neginf * nan)
          print (neginf * inf)
          print (neginf * neginf)
          putStrLn "=== Division ==="
          print (zero / zero)
          print (zero / one)
          print (zero / nan)
          print (zero / inf)
          print (zero / neginf)
          print (one / zero)
          print (one / one)
          print (one / nan)
          print (one / inf)
          print (one / neginf)
          print (nan / zero)
          print (nan / one)
          print (nan / nan)
          print (nan / inf)
          print (nan / neginf)
          print (inf / zero)
          print (inf / one)
          print (inf / nan)
          print (inf / inf)
          print (inf / neginf)
          print (neginf / zero)
          print (neginf / one)
          print (neginf / nan)
          print (neginf / inf)
          print (neginf / neginf)

          putStrLn "=== Subtraction ==="
          print (fzero - fzero)
          print (fzero - fnan)
          print (fzero - finf)
          print (fzero - fneginf)
          print (fnan - fzero)
          print (fnan - fnan)
          print (fnan - finf)
          print (fnan - fneginf)
          print (finf - fzero)
          print (finf - fnan)
          print (finf - finf)
          print (finf - fneginf)
          print (fneginf - fzero)
          print (fneginf - fnan)
          print (fneginf - finf)
          print (fneginf - fneginf)
          putStrLn "=== Addition ==="
          print (fzero + fzero)
          print (fzero + fnan)
          print (fzero + finf)
          print (fzero + fneginf)
          print (fnan + fzero)
          print (fnan + fnan)
          print (fnan + finf)
          print (fnan + fneginf)
          print (finf + fzero)
          print (finf + fnan)
          print (finf + finf)
          print (finf + fneginf)
          print (fneginf + fzero)
          print (fneginf + fnan)
          print (fneginf + finf)
          print (fneginf + fneginf)
          putStrLn "=== Mutiplication ==="
          print (fzero * fzero)
          print (fzero * fone)
          print (fzero * fnan)
          print (fzero * finf)
          print (fzero * fneginf)
          print (fone * fzero)
          print (fone * fone)
          print (fone * fnan)
          print (fone * finf)
          print (fone * fneginf)
          print (fnan * fzero)
          print (fnan * fone)
          print (fnan * fnan)
          print (fnan * finf)
          print (fnan * fneginf)
          print (finf * fzero)
          print (finf * fone)
          print (finf * fnan)
          print (finf * finf)
          print (finf * fneginf)
          print (fneginf * fzero)
          print (fneginf * fone)
          print (fneginf * fnan)
          print (fneginf * finf)
          print (fneginf * fneginf)
          putStrLn "=== Division ==="
          print (fzero / fzero)
          print (fzero / fone)
          print (fzero / fnan)
          print (fzero / finf)
          print (fzero / fneginf)
          print (fone / fzero)
          print (fone / fone)
          print (fone / fnan)
          print (fone / finf)
          print (fone / fneginf)
          print (fnan / fzero)
          print (fnan / fone)
          print (fnan / fnan)
          print (fnan / finf)
          print (fnan / fneginf)
          print (finf / fzero)
          print (finf / fone)
          print (finf / fnan)
          print (finf / finf)
          print (finf / fneginf)
          print (fneginf / fzero)
          print (fneginf / fone)
          print (fneginf / fnan)
          print (fneginf / finf)
          print (fneginf / fneginf)

