
simpleLazyPrint :: a -> IO ()
simpleLazyPrint x = print (primGetHugsObject x)
 where
  -- Extra level of indirection introduced to overcome lack of
  -- polymorphic recursion!
  print :: HugsObject -> IO ()
  print x =
    primClassifyObject False x >>= \ kind ->
    case kind of
    HugsApply fun args -> 
      putChar '('    >>
      print fun      >>
      for_ args (\arg -> 
	putChar ' ' >> 
	print arg
      ) >>
      putChar ')'

    HugsFun nm ->
      putStr (primNameString nm)

    HugsCon nm ->
      putStr (primNameString nm)

    HugsTuple arity ->
      putStr ('(' : replicate arity ',' ++ ")")

    HugsInt x ->
      putStr (show x)

    HugsInteger x ->
      putStr (show x)

    HugsFloat x ->
      putStr (show x)

    HugsChar x ->
      putStr ('\'' : showLitChar x "\'")

    HugsPrim prim ->
      putStr prim

    HugsError err ->
      print err

simpleStrictPrint :: a -> IO ()
simpleStrictPrint x = print (primGetHugsObject x)
 where
  -- Extra level of indirection introduced to overcome lack of
  -- polymorphic recursion!
  print :: HugsObject -> IO ()
  print x =
    primClassifyObject True x >>= \ kind ->
    case kind of
    HugsApply fun args -> 
      putChar '('    >>
      print fun      >>
      for_ args (\arg -> 
	putChar ' ' >> 
	print arg
      ) >>
      putChar ')'

    HugsFun nm ->
      putStr (primNameString nm)

    HugsCon nm ->
      putStr (primNameString nm)

    HugsTuple arity ->
      putStr ('(' : replicate arity ',' ++ ")")

    HugsInt x ->
      putStr (show x)

    HugsInteger x ->
      putStr (show x)

    HugsFloat x ->
      putStr (show x)

    HugsChar x ->
      putStr ('\'' : showLitChar x "\'")

    HugsPrim prim ->
      putStr prim

    HugsError err ->
      -- could call lazy print (if object printer was exposed)
      putStr "{error}"

s1 = simpleStrictPrint (error "foo")
s2 = simpleStrictPrint (1 + error "foo")


-- test

lazyPrint   x = hugsPrinter False (primGetHugsObject x)
strictPrint x = hugsPrinter True (primGetHugsObject x)

t1 = lazyPrint (True &&)
t2 = lazyPrint (1:)
t3 = lazyPrint ('a':)
t4 = lazyPrint (1 `elem`)
t5 = lazyPrint "abcd"
t6 = strict lazyPrint (1 `elem`)

t11 = strictPrint (True &&)
t12 = strictPrint (1:)
t13 = strictPrint ('a':)
t14 = strictPrint (1 `elem`)
t15 = strictPrint "abcd"
t16 = strictPrint (take 10 [1..])
t17 = strictPrint [1..]
t18 = strictPrint (pi::Float)  -- used to fail because pi is a CAF.
t19 = strictPrint '\DEL'

{-
Known Bugs:

* Prints "(||) True False" (in lazy mode) instead of "True || False".

  This is a deliberate change from the original Hugs version (in builtin.c)
  which would print: '{dict} !! "abcd"' for ("abcd" !!) instead of 
  '(!!) {dict} "abcd"' or '("abcd" `(||) {dict}`)'.

  (This is a feature not a bug!)

* Should print errors to stderr.

-}