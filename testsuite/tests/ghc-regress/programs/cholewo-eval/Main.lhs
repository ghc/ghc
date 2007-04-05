\begin{code}
module Main(main) where
import Arr
\end{code}

\begin{code}
type F a = Vector a -> a
type DF a = Vector a -> Vector a
\end{code}

\begin{code}
data {-(Eval a) =>-} ScgData a = ScgData {k :: !Int, err :: !a,
                          w, p, r :: !(Vector a),
                          delta, pnorm2, lambda, lambdabar :: !a,
                          success :: !Bool}
\end{code}

\begin{code}
calculate2order :: Floating a => ScgData a -> a -> DF a -> ScgData a
calculate2order d sigma1 df = 
  let pnorm2' = vectorNorm2 (p d)
      sigma = sigma1 / (sqrt pnorm2')
      s = scaleVector (1/sigma) (df ((w d) + (scaleVector sigma (p d))) - df (w d)) 
  in d {pnorm2 = pnorm2', delta = inner (p d) s}
\end{code}

\begin{code}
hessPosDef :: (Floating a, Ord a) => ScgData a -> ScgData a
hessPosDef d =
  let delta' = delta d + (lambda d - lambdabar d) * pnorm2 d {- step 3 -}
  in if delta' <= 0                             {- step 4 -}
     then let lambdabar' = 2.0 * (lambda d - delta' / pnorm2 d)
          in d {delta = -delta' + lambda d * pnorm2 d, lambda = lambdabar', lambdabar = lambdabar'}
     else d {delta = delta'}
\end{code}

\begin{code}
reduceError :: (Floating a, Ord a) => ScgData a -> DF a -> Bool -> a -> a -> ScgData a
reduceError d df restart bdelta mu = 
  let r' = negate (df (w d))
      p' = if restart
           then r'
           else let beta = (vectorNorm2 r' - inner r' (r d)) / mu
                in r' + scaleVector beta (p d)
  in d {p = p', r = r', lambda = if bdelta >= 0.75 then lambda d / 4 else lambda d
    }
\end{code}

\begin{code}
data ScgInput a = ScgInput (F a) (DF a) (Vector a)
\end{code}

\begin{code}
scgIter :: (Floating a, Ord a) => ScgInput a -> [ScgData a]
scgIter (ScgInput f df w1) =
    let p1 = negate (df w1)                     {- step 1 -}
        r1 = p1
        pnorm21 = vectorNorm2 p1
        n = vectorSize w1
        sigma1 = 1.0e-4
        lambda1 = 1.0e-6
        err1 = f w1
    in iterate (\d ->
           let d1 = if success d                {- step 2 -}
                    then calculate2order d sigma1 df
                    else d
               d2 = hessPosDef d1
               mu = inner (p d2) (r d2)         {- step 5 -}
               alpha = mu / (delta d2)
               w' = (w d2) + scaleVector alpha (p d2)
               err' = f w'
               bdelta = 2 * (delta d2) * ((err d2) - err') / (mu^2) {- step 6 -}
               success' = (bdelta >= 0)         {- step 7 -}
               restart = ((k d) `mod` n == 0)
               d3 = if success' 
                    then (reduceError (d2 {w = w'}) df restart bdelta mu) 
                            {err = err', lambdabar = 0}
                    else d2 {lambdabar = lambda d2}
               d4 = if bdelta < 0.25          {- step 8 -}
                    then d3 {lambda = (lambda d3) + (delta d3) * (1 - bdelta) / (pnorm2 d3)}
                    else d3
           in d4 {k = k d4 + 1, success = success'}
       )
       (ScgData 1 err1 w1 p1 r1 0.0 pnorm21 lambda1 0.0 True)
\end{code}

\begin{code}
rosenbrock = ScgInput
  (\ (Vector x) -> 100 * (x!2 - x!1^2)^2 + (1 - x!1)^2)
  (\ (Vector x) -> listVector [-2 * (1 - x!1) - 400 * x!1 * (x!2 - x!1^2), 
                              200 * (x!2 -x!1^2)])
  (listVector [-1,-1.0])
\end{code}


\begin{code}
main = case vectorList (w ((scgIter rosenbrock)!!1)) of
       [v1, v2] -> if (e1 `isSame` v1) && (e2 `isSame` v2)
                   then print (e1, e2)
                   else putStrLn ("Mismatch: " ++ show (e1, e2, v1, v2))
       vs -> putStrLn ("Wrong list length: " ++ show vs)

e1, e2 :: Floating a => a
e1 = -0.5105811455265337
e2 = -0.7565080326002654

isSame :: (Fractional a, Ord a) => a -> a -> Bool
x `isSame` y = abs (x - y) < 0.000000000000001
\end{code}
