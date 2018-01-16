
-- ==========================================================--
-- === Printer of abstract functions                      ===--
-- ===                               File: PrintResults.m ===--
-- ==========================================================--

module PrintResults where
import BaseDefs
import Utils
import MyUtils
import Inverse
import AbstractMisc

-- ==========================================================--
--
prLift :: PrDomain -> PrDomain

prLift d = newBottom:d
           where
              dElemLen = length (head d)
              dBottomElem = minimum (concat d) - (1 :: Int)
              newBottom = copy dElemLen dBottomElem


-- ==========================================================--
--
prCross :: PrDomain -> PrDomain -> PrDomain

prCross d1 d2 = [e1++e2 | e1 <- d1,  e2 <- d2]


-- ==========================================================--
--
prCrossList :: [PrDomain] -> PrDomain

prCrossList []        = [[0]]  -- ????????????
prCrossList [d]       = d
prCrossList (a:b:abs) = prCross a (prCrossList (b:abs))


-- ==========================================================--
--
prAllPoints :: Domain -> [Char]

prAllPoints d
   = "{" ++ interleave " " ((h.g.f) d) ++ "}"
     where
	-- f creates the numbered version of a domain
	f Two = [ [(-1) :: Int], [0 :: Int] ]
        f (Lift1 ds) = prLift (prCrossList (map f ds))
        f (Lift2 ds) = prLift (prLift (prCrossList (map f ds)))

        -- g normalises the numbers in a domain so the lowest is zero
        g d = map (map (mySubtract (minimum (concat d)))) d

	-- h converts a domain of numbers into one of characters
	h x = map (map k) (g x)

        -- k turns a number into its ascii representation
        k :: Int -> Char
        k n = toEnum (n+48)


-- ==========================================================--
--
prWidth :: Domain -> Int

prWidth Two         = 1 :: Int
prWidth (Lift1 ds)  = sum (map prWidth ds)
prWidth (Lift2 ds)  = sum (map prWidth ds)


-- ==========================================================--
--
prLiftsIn :: Domain -> Int

prLiftsIn Two         = 2 :: Int
prLiftsIn (Lift1 ds)  = 1 + maximum (map prLiftsIn ds)
prLiftsIn (Lift2 ds)  = 2 + maximum (map prLiftsIn ds)


-- ==========================================================--
--
prSucc :: Int -> Int -> Int

prSucc n c = n + c


-- ==========================================================--
--
prRoute :: Domain -> Route -> [Char]

prRoute d r
   = let k :: Int -> Char
         k n = toEnum (n + 48)
     in
         map k (prRouteMain d r)


-- ==========================================================--
--
prRouteMain :: Domain -> Route -> [Int]

prRouteMain Two Zero 
   = [0 :: Int]
prRouteMain Two One
   = [1 :: Int]

prRouteMain d@(Lift1 ds) Stop1
   = copy (prWidth d) 0
prRouteMain d@(Lift1 ds) (Up1 rs) 
   = map (prSucc 1) (prRouteMain_cross ds rs)

prRouteMain d@(Lift2 ds) Stop2
   = copy (prWidth d) 0
prRouteMain d@(Lift2 ds) Up2
   = copy (prWidth d) 1
prRouteMain d@(Lift2 ds) (UpUp2 rs)
   = map (prSucc 2) (prRouteMain_cross ds rs)

prRouteMain_cross ds rs 
   = concat fixedRoutes
     where
        unFixedRoutes
           = myZipWith2 prRouteMain ds rs
        compFactors
           = map prLiftsIn ds
        compFactMax
           = maximum compFactors
        compFactNorm
           = map subCompFactMax compFactors
        fixedRoutes 
           = map applyCompensationFactor
                (myZip2 compFactNorm unFixedRoutes)
        applyCompensationFactor (n, roote) 
           = map (prSucc n) roote
        subCompFactMax :: Int -> Int
        subCompFactMax nn 
           = compFactMax - nn


-- ==========================================================--
--
prPrintFunction :: Bool -> StaticComponent -> Naam -> Point -> [Char]

-- the normal case, for printing non-constant functions
prPrintFunction mi statics fName (fDomain@(Func dss dt), Rep rep)
   | amIsaHOF (Func dss dt) || NoFormat `elem` utSCflags statics
   = "\nFunction \"" ++ fName++ "\" has input domains:\n"
     ++ layn (map show dss) ++
     "   and output domain\n      " ++
     show dt ++ "\n\nwith value:\n\n" ++ show rep ++ "\n\n"

   | otherwise
   = "\nFunction \"" ++ fName++ "\" has input domains:\n" ++
     numberedPrInDs ++
     "   and output domain\n      " ++ 
     prettyOutDomain ++
     "\n\n   Output  |  Lower frontier" ++
       "\n   --------+----------------\n" ++
        concat (map f ((reverse.sort.amAllRoutes) dt)) ++ "\n\n"
     where
        pseudoParams 
          = utSureLookup (utSCfreevars statics) 
                   "prPrintFunction" fName ++ forever ""
        forever x = x:forever x

        inputDomains = dss

        outputDomain = dt

        prettyInDomains = map prAllPoints inputDomains
        prettyOutDomain = prAllPoints outputDomain

        numberedPrInDs = layn (map ff (zip pseudoParams prettyInDomains))
        ff ("",   pid) = pid
        ff (name, pid) = pid ++ " (free \"" ++ name ++ "\")"

        f op  = let ipl = inMinInverse mi fDomain (Rep rep) op
                in (copy (8 - length outColText) ' ') ++ outColText ++
                      "   |  " ++ (interleave " and " (map g ipl)) ++ "\n"
                      where
                         outColText = prRoute dt op
        g (MkFrel rs) = interleave " " (myZipWith2 prRoute dss rs)


-- the exception case, for printing constants
prPrintFunction mi statics fName (ds, rs)
   | amContainsFunctionSpace ds
   = "\nFunction \"" ++ fName++ 
     "\" is a higher-order constant (yuck) in domain\n\n"
      ++ show ds ++
     "\n\nof value\n\n" ++ show rs ++ "\n\n"

   | otherwise
   = "\nFunction \"" ++ fName++ "\" is a constant point " ++
     prRoute ds rs ++ " in domain \n    " ++
     prAllPoints ds ++ "\n\n"


-- ==========================================================--
-- === end                                 PrintResults.m ===--
-- ==========================================================--
