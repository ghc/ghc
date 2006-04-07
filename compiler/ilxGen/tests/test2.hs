-- To start:
-- source /bin/devghc

-- To compile GHC
-- make ilxGen/IlxGen.o hsc

-- To compile ILXASM
-- (cd /devel/fcom/src; make bin/ilxasm.exe) 

-- To compile to ILX
-- (cd ilxGen/tests; ../../../driver/ghc-inplace --ilx test.hs) 



-- To generate a complete ILX file, including preludes for GHC and ILX:
-- (cd ilxGen/tests/; cat prelude.ilx test.ilx  /devel/fcom/src/ilxasm/stdlib-func.ilx > test.full.ilx)

-- Run ILXASM to get a IL
-- ( cd ilxGen/tests/; /devel/fcom/src/bin/ilxasm.exe --no-ilasm --no-stdlib test.full.ilx > test.il)

-- To compile IL to .EXE or .DLL:
-- With build of VS (e.g. Don & Andrew)
--   ( cd ilxGen/tests/; cmd /C "c:\\bin\\devvs.bat && ilasm test.il") 
-- With Lightning SDK, where env. variables are on path (e.g. Reuben):
--   ( cd ilxGen/tests/; ilasm test.il) 

-- To validate .EXE:
-- (cd /devel/fcom/src; make  bin/ilvalid.exe mscorlib.vlb)
-- (export ILVALID_HOME=/devel/fcom/src; cd ilxGen/tests/; /devel/fcom/src/bin/ilvalid.exe test.il) 

-- To run unverifiable code:
-- With build of VS (e.g. Don & Andrew)
--    (cd ilxGen/tests/;  cmd /C "c:\\bin\\devvs.bat && .\test.exe")
-- With Lightning SDK, where env. variables are on path (e.g. Reuben):
--    (cd ilxGen/tests/; ./test.exe)

-- To compile ILX to verifiable code and verify
-- (cd /devel/fcom/src; make bin/ilxasm.exe bin/ilverify.exe)  && (cd ilxGen/tests/; export ILVALID_HOME=/devel/fcom/src; cat prelude.ilx  test.ilx /devel/fcom/src/assem/stdlib-func.ilx > test.full.ilx && cd ilxGen/tests/; /devel/fcom/src/bin/ilxasm.exe --no-ilasm test.full.ilx > test.safe.il && /devel/fcom/src/bin/ilverify.exe test.safe.il) 

-- (cd ilxGen/tests/;  cmd /C "c:\\bin\\devvs.bat && .\test.safe.exe")

--append:: [Char] -> [Char] -> [Char]
--append [] l2 = l2
--append (h:t) l2 = h:append t l2

data N = Z | S N

chooseN n  = 
  case n of 
       Z -> "even\n"
       S Z -> "odd\n"
       S (S m) -> chooseN m 

add n m = 
   case n of
       Z -> m  
       S nn -> S (add nn m)

mul n m = 
   case n of
       Z -> Z
       S nn -> add m (mul nn m)

pow n m = 
   case m of
       Z -> S Z
       S mm -> mul n (pow n mm)

sq n = mul n n

n1 = S Z
n2 = add n1 n1
n4 = add n2 n2
n6 = add n2 n4
n8 = add n2 n6
n10 = add n2 n8
n16 = add n6 n10
n17 = add n1 n16
n18 = add n8 n10
n19 = add n1 n18
n20 = add n4 n16

bign = pow n2 n10
bign1 = add bign n1

main = putStr (chooseN bign1)


