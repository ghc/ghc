
-- | Mapping of prelude functions to vectorised versions.
--     Functions like filterP currently have a working but naive version in GHC.PArr
--     During vectorisation we replace these by calls to filterPA, which are
--     defined in dph-common Data.Array.Parallel.Lifted.Combinators
--
--     As renamer only sees the GHC.PArr functions, if you want to add a new function
--     to the vectoriser there has to be a definition for it in GHC.PArr, even though
--     it will never be used at runtime.
--
module Vectorise.Builtins.Prelude
	( preludeVars
	, preludeScalars)
where
import Vectorise.Builtins.Modules
import PrelNames
import Module
import FastString


preludeVars
	:: Modules			-- ^ Modules containing the DPH backens
	-> [( Module, FastString	--   Maps the original variable to the one in the DPH 
	    , Module, FastString)]      --   packages that it should be rewritten to.

preludeVars (Modules { dph_Combinators    = dph_Combinators
                     , dph_PArray         = dph_PArray
                     , dph_Prelude_Int    = dph_Prelude_Int
                     , dph_Prelude_Word8  = dph_Prelude_Word8
                     , dph_Prelude_Double = dph_Prelude_Double
                     , dph_Prelude_Bool   = dph_Prelude_Bool 
                     , dph_Prelude_PArr   = dph_Prelude_PArr
                     })

    -- Functions that work on whole PArrays, defined in GHC.PArr
  = [ mk gHC_PARR (fsLit "mapP")       dph_Combinators (fsLit "mapPA")
    , mk gHC_PARR (fsLit "zipWithP")   dph_Combinators (fsLit "zipWithPA")
    , mk gHC_PARR (fsLit "zipP")       dph_Combinators (fsLit "zipPA")
    , mk gHC_PARR (fsLit "unzipP")     dph_Combinators (fsLit "unzipPA")
    , mk gHC_PARR (fsLit "filterP")    dph_Combinators (fsLit "filterPA")
    , mk gHC_PARR (fsLit "lengthP")    dph_Combinators (fsLit "lengthPA")
    , mk gHC_PARR (fsLit "replicateP") dph_Combinators (fsLit "replicatePA")
    , mk gHC_PARR (fsLit "!:")         dph_Combinators (fsLit "indexPA")
    , mk gHC_PARR (fsLit "sliceP")     dph_Combinators (fsLit "slicePA")
    , mk gHC_PARR (fsLit "crossMapP")  dph_Combinators (fsLit "crossMapPA")
    , mk gHC_PARR (fsLit "singletonP") dph_Combinators (fsLit "singletonPA")
    , mk gHC_PARR (fsLit "concatP")    dph_Combinators (fsLit "concatPA")
    , mk gHC_PARR (fsLit "+:+")        dph_Combinators (fsLit "appPA")
    , mk gHC_PARR (fsLit "emptyP")     dph_PArray      (fsLit "emptyPA")

    -- Map scalar functions to versions using closures. 
    , mk' dph_Prelude_Int "div"         "divV"
    , mk' dph_Prelude_Int "mod"         "modV"
    , mk' dph_Prelude_Int "sqrt"        "sqrtV"
    , mk' dph_Prelude_Int "enumFromToP" "enumFromToPA"
    -- , mk' dph_Prelude_Int "upToP" "upToPA"
    ]
    ++ vars_Ord dph_Prelude_Int
    ++ vars_Num dph_Prelude_Int

    ++ vars_Ord dph_Prelude_Word8
    ++ vars_Num dph_Prelude_Word8
    ++
    [ mk' dph_Prelude_Word8 "div"     "divV"
    , mk' dph_Prelude_Word8 "mod"     "modV"
    , mk' dph_Prelude_Word8 "fromInt" "fromIntV"
    , mk' dph_Prelude_Word8 "toInt"   "toIntV"
    ]

    ++ vars_Ord        dph_Prelude_Double
    ++ vars_Num        dph_Prelude_Double
    ++ vars_Fractional dph_Prelude_Double
    ++ vars_Floating   dph_Prelude_Double
    ++ vars_RealFrac   dph_Prelude_Double
    ++
    [ mk dph_Prelude_Bool  (fsLit "andP")  dph_Prelude_Bool (fsLit "andPA")
    , mk dph_Prelude_Bool  (fsLit "orP")   dph_Prelude_Bool (fsLit "orPA")

    , mk gHC_CLASSES (fsLit "not")         dph_Prelude_Bool (fsLit "notV")
    , mk gHC_CLASSES (fsLit "&&")          dph_Prelude_Bool (fsLit "andV")
    , mk gHC_CLASSES (fsLit "||")          dph_Prelude_Bool (fsLit "orV")

    -- FIXME: temporary
    , mk dph_Prelude_PArr (fsLit "fromPArrayP")       dph_Prelude_PArr (fsLit "fromPArrayPA")
    , mk dph_Prelude_PArr (fsLit "toPArrayP")         dph_Prelude_PArr (fsLit "toPArrayPA")
    , mk dph_Prelude_PArr (fsLit "fromNestedPArrayP") dph_Prelude_PArr (fsLit "fromNestedPArrayPA")
    , mk dph_Prelude_PArr (fsLit "combineP")          dph_Combinators  (fsLit "combine2PA")
    , mk dph_Prelude_PArr (fsLit "updateP")           dph_Combinators  (fsLit "updatePA")
    , mk dph_Prelude_PArr (fsLit "bpermuteP")         dph_Combinators  (fsLit "bpermutePA")
    , mk dph_Prelude_PArr (fsLit "indexedP")          dph_Combinators  (fsLit "indexedPA")
    ]
  where
    mk  = (,,,)
    mk' mod v v' = mk mod (fsLit v) mod (fsLit v')

    vars_Ord mod 
     = [ mk' mod "=="        "eqV"
       , mk' mod "/="        "neqV"
       , mk' mod "<="        "leV"
       , mk' mod "<"         "ltV"
       , mk' mod ">="        "geV"
       , mk' mod ">"         "gtV"
       , mk' mod "min"       "minV"
       , mk' mod "max"       "maxV"
       , mk' mod "minimumP"  "minimumPA"
       , mk' mod "maximumP"  "maximumPA"
       , mk' mod "minIndexP" "minIndexPA"
       , mk' mod "maxIndexP" "maxIndexPA"
       ]

    vars_Num mod 
     = [ mk' mod "+"        "plusV"
       , mk' mod "-"        "minusV"
       , mk' mod "*"        "multV"
       , mk' mod "negate"   "negateV"
       , mk' mod "abs"      "absV"
       , mk' mod "sumP"     "sumPA"
       , mk' mod "productP" "productPA"
       ]

    vars_Fractional mod 
     = [ mk' mod "/"     "divideV"
       , mk' mod "recip" "recipV"
       ]

    vars_Floating mod 
     = [ mk' mod "pi"      "pi"
       , mk' mod "exp"     "expV"
       , mk' mod "sqrt"    "sqrtV"
       , mk' mod "log"     "logV"
       , mk' mod "sin"     "sinV"
       , mk' mod "tan"     "tanV"
       , mk' mod "cos"     "cosV"
       , mk' mod "asin"    "asinV"
       , mk' mod "atan"    "atanV"
       , mk' mod "acos"    "acosV"
       , mk' mod "sinh"    "sinhV"
       , mk' mod "tanh"    "tanhV"
       , mk' mod "cosh"    "coshV"
       , mk' mod "asinh"   "asinhV"
       , mk' mod "atanh"   "atanhV"
       , mk' mod "acosh"   "acoshV"
       , mk' mod "**"      "powV"
       , mk' mod "logBase" "logBaseV"
       ]

    vars_RealFrac mod
     = [ mk' mod "fromInt"  "fromIntV"
       , mk' mod "truncate" "truncateV"
       , mk' mod "round"    "roundV"
       , mk' mod "ceiling"  "ceilingV"
       , mk' mod "floor"    "floorV"
       ]


preludeScalars :: Modules -> [(Module, FastString)]
preludeScalars (Modules { dph_Prelude_Int    = dph_Prelude_Int
                        , dph_Prelude_Word8  = dph_Prelude_Word8
                        , dph_Prelude_Double = dph_Prelude_Double
                        })
  = [ mk dph_Prelude_Int "div"
    , mk dph_Prelude_Int "mod"
    , mk dph_Prelude_Int "sqrt"
    ]
    ++ scalars_Ord dph_Prelude_Int
    ++ scalars_Num dph_Prelude_Int

    ++ scalars_Ord dph_Prelude_Word8
    ++ scalars_Num dph_Prelude_Word8
    ++
    [ mk dph_Prelude_Word8 "div"
    , mk dph_Prelude_Word8 "mod"
    , mk dph_Prelude_Word8 "fromInt"
    , mk dph_Prelude_Word8 "toInt"
    ]

    ++ scalars_Ord dph_Prelude_Double
    ++ scalars_Num dph_Prelude_Double
    ++ scalars_Fractional dph_Prelude_Double
    ++ scalars_Floating dph_Prelude_Double
    ++ scalars_RealFrac dph_Prelude_Double
  where
    mk mod s = (mod, fsLit s)

    scalars_Ord mod 
     = [ mk mod "=="
       , mk mod "/="
       , mk mod "<="
       , mk mod "<"
       , mk mod ">="
       , mk mod ">"
       , mk mod "min"
       , mk mod "max"
       ]

    scalars_Num mod 
     = [ mk mod "+"
       , mk mod "-"
       , mk mod "*"
       , mk mod "negate"
       , mk mod "abs"
       ]

    scalars_Fractional mod 
     = [ mk mod "/"
       , mk mod "recip"
       ]

    scalars_Floating mod 
     = [ mk mod "pi"
       , mk mod "exp"
       , mk mod "sqrt"
       , mk mod "log"
       , mk mod "sin"
       , mk mod "tan"
       , mk mod "cos"
       , mk mod "asin"
       , mk mod "atan"
       , mk mod "acos"
       , mk mod "sinh"
       , mk mod "tanh"
       , mk mod "cosh"
       , mk mod "asinh"
       , mk mod "atanh"
       , mk mod "acosh"
       , mk mod "**"
       , mk mod "logBase"
       ]

    scalars_RealFrac mod 
     = [ mk mod "fromInt"
       , mk mod "truncate"
       , mk mod "round"
       , mk mod "ceiling"
       , mk mod "floor"
       ]
