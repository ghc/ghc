
module Test.FilePattern(main) where

import Development.Shake.Internal.FilePattern
import Development.Shake.FilePath
import Control.Monad
import System.IO.Unsafe
import System.Info.Extra
import Data.List.Extra
import Test.Type
import Test.QuickCheck hiding ((===))



newtype Pattern = Pattern FilePattern deriving (Show,Eq)
newtype Path    = Path    FilePath    deriving (Show,Eq)

-- Since / and * are the only "interesting" elements, just add ab to round out the set

instance Arbitrary Pattern where
    arbitrary = fmap Pattern $ listOf $ elements "\\/*ab"
    shrink (Pattern x) = map Pattern $ shrinkList (\x -> ['/' | x == '\\']) x

instance Arbitrary Path where
    arbitrary = fmap Path $ listOf $ elements "\\/ab"
    shrink (Path x) = map Path $ shrinkList (\x -> ['/' | x == '\\']) x


main = testSimple $ do
    internalTest
    let norm = filter (/= ".") . split isPathSeparator
    let f b pat file = do
            assertBool (b == (pat ?== file)) $ show pat ++ " ?== " ++ show file ++ "\nEXPECTED: " ++ show b
            assertBool (b == (pat `walker` file)) $ show pat ++ " `walker` " ++ show file ++ "\nEXPECTED: " ++ show b
            when b $ assertBool (norm (substitute (extract pat file) pat) == norm file) $
                "FAILED substitute/extract property\nPattern: " ++ show pat ++ "\nFile: " ++ show file ++ "\n" ++
                "Extracted: " ++ show (extract pat file) ++ "\nSubstitute: " ++ show (substitute (extract pat file) pat)

    f True "//*.c" "foo/bar/baz.c"
    f True "**/*.c" "foo/bar/baz.c"
    f True (toNative "//*.c") "foo/bar\\baz.c"
    f True (toNative "**/*.c") "foo/bar\\baz.c"
    f True "*.c" "baz.c"
    f True "//*.c" "baz.c"
    f True "**/*.c" "baz.c"
    f True "test.c" "test.c"
    f False "*.c" "foor/bar.c"
    f False "*/*.c" "foo/bar/baz.c"
    f False "foo//bar" "foobar"
    f False "foo/**/bar" "foobar"
    f False "foo//bar" "foobar/bar"
    f False "foo/**/bar" "foobar/bar"
    f False "foo//bar" "foo/foobar"
    f False "foo/**/bar" "foo/foobar"
    f True "foo//bar" "foo/bar"
    f True "foo/**/bar" "foo/bar"
    f True "foo/bar" (toNative "foo/bar")
    f True (toNative "foo/bar") "foo/bar"
    f True (toNative "foo/bar") (toNative "foo/bar")
    f True "//*" "/bar"
    f False "**/*" "/bar"
    f True "/bob//foo" "/bob/this/test/foo"
    f True "/bob/**/foo" "/bob/this/test/foo"
    f False "/bob//foo" "bob/this/test/foo"
    f False "/bob/**/foo" "bob/this/test/foo"
    f True "bob//foo/" "bob/this/test/foo/"
    f True "bob/**/foo/" "bob/this/test/foo/"
    f False "bob//foo/" "bob/this/test/foo"
    f False "bob/**/foo/" "bob/this/test/foo"
    f True "a//" "a"
    f True "a/**" "a"
    f True "/a//" "/a"
    f True "/a/**" "/a"
    f True "///a//" "/a"
    f False "**/a/**" "/a"
    f False "///" ""
    f True "///" "/"
    f True "/**" "/"
    f True "///" "a/"
    f True "**/" "a/"
    f True "////" ""
    f True "**/**" ""
    f True "x///y" "x/y"
    f True "x/**/y" "x/y"
    f True "x///" "x/"
    f True "x/**/" "x/"
    f True "x///" "x/foo/"
    f True "x/**/" "x/foo/"
    f False "x///" "x"
    f False "x/**/" "x"
    f True "x///" "x/foo/bar/"
    f True "x/**/" "x/foo/bar/"
    f False "x///" "x/foo/bar"
    f False "x/**/" "x/foo/bar"
    f True "x///y" "x/z/y"
    f True "x/**/*/y" "x/z/y"
    f True "" ""
    f False "" "y"
    f False "" "/"

    f True "*/*" "x/y"
    f False "*/*" "x"
    f True "//*" "x"
    f True "**/*" "x"
    f True "//*" ""
    f True "**/*" ""
    f True "*//" "x"
    f True "*/**" "x"
    f True "*//" ""
    f True "*/**" ""
    f True "*//*" "x/y"
    f True "*/**/*" "x/y"
    f False "*//*" ""
    f False "*/**/*" ""
    f False "*//*" "x"
    f False "*/**/*" "x"
    f False "*//*//*" "x/y"
    f False "*/**/*/**/*" "x/y"
    f True "//*/" "/"
    f False "**/*/" "/"
    f True "*/////" "/"
    f True "*/**/**/" "/"
    f False "b*b*b*//" "bb"
    f False "b*b*b*/**" "bb"

    f False "**" "/"
    f False "**/x" "/x"
    f True "**" "x/"
    f (not isWindows) "**" "\\\\drive"
    f (not isWindows) "**" "C:\\drive"
    f (not isWindows) "**" "C:drive"

    -- We support ignoring '.' values in FilePath as they are inserted by @filepath@ a lot
    f True "./file" "file"
    f True ("" </> "file") "file"
    f True "foo/./bar" "foo/bar"
    f True "foo/./bar" "foo/./bar"
    f False "foo/./bar" "foo/bob"

    filePattern "**/*.c" "test.txt" === Nothing
    filePattern "**/*.c" "foo.c" === Just ["","foo"]
    filePattern "**/*.c" "bar/baz/foo.c" === Just ["bar/baz/","foo"]
    filePattern "**/*.c" "bar\\baz\\foo.c" === Just
        (if isWindows then ["bar/baz/","foo"] else ["","bar\\baz\\foo"])

    simple "a*b" === False
    simple "a//b" === False
    simple "a/**/b" === False
    simple "/a/b/cccc_" === True
    simple "a///b" === False
    simple "a/**/b" === False

    assertBool (compatible []) "compatible"
    assertBool (compatible ["//*a.txt","foo//a*.txt"]) "compatible"
    assertBool (compatible ["**/*a.txt","foo/**/a*.txt"]) "compatible"
    assertBool (compatible ["//*a.txt","foo/**/a*.txt"]) "compatible"
    assertBool (not $ compatible ["//*a.txt","foo//a*.*txt"]) "compatible"
    assertBool (not $ compatible ["**/*a.txt","foo/**/a*.*txt"]) "compatible"
    extract "//*a.txt" "foo/bar/testa.txt" === ["foo/bar/","test"]
    extract "**/*a.txt" "foo/bar/testa.txt" === ["foo/bar/","test"]
    extract "//*a.txt" "testa.txt" === ["","test"]
    extract "**/*a.txt" "testa.txt" === ["","test"]
    extract "//a.txt" "a.txt" === [""]
    extract "**/a.txt" "a.txt" === [""]
    extract "//a.txt" "/a.txt" === ["/"]
    extract "a//b" "a/b" === [""]
    extract "a/**/b" "a/b" === [""]
    extract "a//b" "a/x/b" === ["x/"]
    extract "a/**/b" "a/x/b" === ["x/"]
    extract "a//b" "a/x/y/b" === ["x/y/"]
    extract "a/**/b" "a/x/y/b" === ["x/y/"]
    extract "a///b" "a/x/y/b" === ["x/y/"]
    extract "a/**/**/b" "a/x/y/b" === ["","x/y/"]
    extract "//*a*.txt" "testada.txt" === ["","test","da"]
    extract "**/*a*.txt" "testada.txt" === ["","test","da"]
    extract (toNative "//*a*.txt") "testada.txt" === ["","test","da"]
    extract (toNative "**/*a*.txt") "testada.txt" === ["","test","da"]
    substitute ["","test","da"] "//*a*.txt" === "testada.txt"
    substitute ["","test","da"] "**/*a*.txt" === "testada.txt"
    substitute  ["foo/bar/","test"] "//*a.txt" === "foo/bar/testa.txt"
    substitute  ["foo/bar/","test"] "**/*a.txt" === "foo/bar/testa.txt"

    (False, Walk _) <- pure $ walk ["*.xml"]
    (False, Walk _) <- pure $ walk ["//*.xml"]
    (False, Walk _) <- pure $ walk ["**/*.xml"]
    (False, WalkTo ([], [("foo",Walk _)])) <- pure $ walk ["foo//*.xml"]
    (False, WalkTo ([], [("foo",Walk _)])) <- pure $ walk ["foo/**/*.xml"]
    (False, WalkTo ([], [("foo",WalkTo ([],[("bar",Walk _)]))])) <- pure $ walk ["foo/bar/*.xml"]
    (False, WalkTo (["a"],[("b",WalkTo (["c"],[]))])) <- pure $ walk ["a","b/c"]
    ([], [("foo",WalkTo ([],[("bar",Walk _)]))]) <- let (False, Walk f) = walk ["*/bar/*.xml"] in pure $ f ["foo"]
    (False, WalkTo ([],[("bar",Walk _),("baz",Walk _)])) <- pure $ walk ["bar/*.xml","baz//*.c"]
    (False, WalkTo ([],[("bar",Walk _),("baz",Walk _)])) <- pure $ walk ["bar/*.xml","baz/**/*.c"]
    (False, WalkTo ([], [])) <- pure $ walk []
    (True, Walk _) <- pure $ walk ["//"]
    (True, Walk _) <- pure $ walk ["**"]
    (True, WalkTo _) <- pure $ walk [""]

    Success{} <- quickCheckWithResult stdArgs{maxSuccess=1000} $ \(Pattern p) (Path x) ->
        let label _ = property in
            -- Ignore label to workaround QuickCheck space-leak
            -- See #450 and https://github.com/nick8325/quickcheck/pull/93
        let b = p ?== x in (if b then property else label "No match") $ unsafePerformIO $ do f b p x; pure True
    pure ()


walker :: FilePattern -> FilePath -> Bool
-- Slight difference of opinion since Walker is always relative to something
walker a b | isRelativePattern a, not $ isRelativePath b = False
walker a b = f (split isPathSeparator b) $ snd $ walk [a]
    where
        f (".":xs) w = f xs w
        f (x:xs) (Walk op) = f (x:xs) $ WalkTo $ op [x]
        f [x]    (WalkTo (file, _  )) = x `elem` file
        f (x:xs) (WalkTo (_   , dir)) | Just w <- lookup x dir = f xs w
        f _ _ = False
