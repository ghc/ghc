
-- | A module for 'FilePath' operations exposing "System.FilePath" plus some additional operations.
--
--   /Windows note:/ The extension methods ('<.>', 'takeExtension' etc) use the Posix variants since on
--   Windows @\"\/\/\*\" '<.>' \"txt\"@ produces @\"\/\/\*\\\\.txt\"@
--   (which is bad for 'Development.Shake.FilePattern' values).
module Development.Shake.FilePath(
    module System.FilePath, module System.FilePath.Posix,
    dropDirectory1, takeDirectory1, replaceDirectory1,
    makeRelativeEx, normaliseEx,
    toNative, toStandard,
    exe
    ) where

import System.Directory (canonicalizePath)
import System.Info.Extra
import Data.List.Extra
import Data.Maybe
import qualified System.FilePath as Native

import System.FilePath hiding
    (splitExtension, takeExtension, replaceExtension, dropExtension, addExtension
    ,hasExtension, (<.>), splitExtensions, takeExtensions, dropExtensions
    )
import System.FilePath.Posix
    (splitExtension, takeExtension, replaceExtension, dropExtension, addExtension
    ,hasExtension, (<.>), splitExtensions, takeExtensions, dropExtensions
    )


-- | Drop the first directory from a 'FilePath'. Should only be used on
--   relative paths.
--
-- > dropDirectory1 "aaa/bbb" == "bbb"
-- > dropDirectory1 "aaa/" == ""
-- > dropDirectory1 "aaa" == ""
-- > dropDirectory1 "" == ""
dropDirectory1 :: FilePath -> FilePath
dropDirectory1 = drop1 . dropWhile (not . isPathSeparator)


-- | Take the first component of a 'FilePath'. Should only be used on
--   relative paths.
--
-- > takeDirectory1 "aaa/bbb" == "aaa"
-- > takeDirectory1 "aaa/" == "aaa"
-- > takeDirectory1 "aaa" == "aaa"
takeDirectory1 :: FilePath -> FilePath
takeDirectory1 = takeWhile (not . isPathSeparator)



-- | Replace the first component of a 'FilePath'. Should only be used on
--   relative paths.
--
-- > replaceDirectory1 "root/file.ext" "directory" == "directory/file.ext"
-- > replaceDirectory1 "root/foo/bar/file.ext" "directory" == "directory/foo/bar/file.ext"
replaceDirectory1 :: FilePath -> String -> FilePath
replaceDirectory1 x dir = dir </> dropDirectory1 x

-- | Make a path relative. Returns Nothing only when the given paths are on
-- different drives. This will try the pure function makeRelative first. If that
-- fails, the paths are canonicalised (removing any indirection and symlinks)
-- and a relative path is derived from there.
--
-- > > -- Given that "/root/a/" is not a symlink
-- > > makeRelativeEx "/root/a/" "/root/b/file.out"
-- > Just "../b/file.out"
-- >
-- > > -- Given that "/root/c/" is a symlink to "/root/e/f/g/"
-- > > makeRelativeEx "/root/c/" "/root/b/file.out"
-- > Just "../../../b/file.out"
-- >
-- > > -- On Windows
-- > > makeRelativeEx "C:\\foo" "D:\\foo\\bar"
-- > Nothing
--
makeRelativeEx :: FilePath -> FilePath -> IO (Maybe FilePath)
makeRelativeEx pathA pathB
    | isRelative makeRelativePathAPathB =
        pure (Just makeRelativePathAPathB)
    | otherwise = do
        a' <- canonicalizePath pathA
        b' <- canonicalizePath pathB
        if takeDrive a' /= takeDrive b'
            then pure Nothing
            else Just <$> makeRelativeEx' a' b'
    where
        makeRelativePathAPathB = makeRelative pathA pathB

        makeRelativeEx' :: FilePath -> FilePath -> IO FilePath
        makeRelativeEx' a b = do
            let rel = makeRelative a b
                parent = takeDirectory a
            if isRelative rel
                then pure rel
                else if a /= parent
                    then do
                        parentToB <- makeRelativeEx' parent b
                        pure (".." </> parentToB)

                    -- Impossible: makeRelative should have succeeded in finding
                    -- a relative path once `a == "/"`.
                    else error $ "Error calculating relative path from \""
                                ++ pathA ++ "\" to \"" ++ show pathB ++ "\""

-- | Normalise a 'FilePath', applying the rules:
--
-- * All 'pathSeparators' become 'pathSeparator' (@\/@ on Linux, @\\@ on Windows)
--
-- * @foo\/bar\/..\/baz@ becomes @foo\/baz@ (not universally true in the presence of symlinks)
--
-- * @foo\/.\/bar@ becomes @foo\/bar@
--
-- * @foo\/\/bar@ becomes @foo\/bar@
--
--   This function is not based on the 'normalise' function from the @filepath@ library, as that function
--   is quite broken.
normaliseEx :: FilePath -> FilePath
normaliseEx xs | a:b:xs <- xs, isWindows && sep a && sep b = '/' : f ('/':xs) -- account for UNC paths being double //
               | otherwise = f xs
    where
        sep = Native.isPathSeparator
        f o = toNative $ deslash o $ (++"/") $ concatMap ('/':) $ reverse $ g 0 $ reverse $ split o

        deslash o x
            | x == "/" = case (pre,pos) of
                (True,True) -> "/"
                (True,False) -> "/."
                (False,True) -> "./"
                (False,False) -> "."
            | otherwise = (if pre then id else drop1) $ (if pos then id else init) x
            where pre = sep $ fromMaybe ' ' $ listToMaybe o
                  pos = sep $ last $ " " ++ o

        g i [] = replicate i ".."
        g i ("..":xs) = g (i+1) xs
        g i (".":xs) = g i xs
        g 0 (x:xs) = x : g 0 xs
        g i (_:xs) = g (i-1) xs -- equivalent to eliminating ../x

        split xs = if null ys then [] else a : split b
            where (a,b) = break sep ys
                  ys = dropWhile sep xs


-- | Convert to native path separators, namely @\\@ on Windows.
toNative :: FilePath -> FilePath
toNative = if isWindows then map (\x -> if x == '/' then '\\' else x) else id

-- | Convert all path separators to @/@, even on Windows.
toStandard :: FilePath -> FilePath
toStandard = if isWindows then map (\x -> if x == '\\' then '/' else x) else id


-- | The extension of executables, @\"exe\"@ on Windows and @\"\"@ otherwise.
exe :: String
exe = if isWindows then "exe" else ""
