{-# LANGUAGE OverloadedStrings #-}
-- Usage:
--   ghc ../examples/Test.hs
--   ghc Unit.hs
--   ./Unit ../examples/Test
-- Requirements:
-- - Empty ~/.haskeline (or set to defaults)
-- - Assumes the dummy folder is in the current folder
-- - On Mac OS X, may need to clear out /usr/lib/charset.alias
--   (In particular, the line "* UTF-8" which makes locale_charset()
--   always return UTF-8; otherwise we can't test latin-1.)
-- - NB: Window size isn't provided by screen so it's picked up from
--   terminfo or defaults (either way: 80x24), rather than the user's
--   terminal.
module Main where

import System.Environment
import Test.HUnit
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import RunTTY

legacyEncoding :: Bool
legacyEncoding = False

-- Generally we want the legacy and new backends to perform the same.
-- The only two differences I'm aware of are:
-- 1. base decodes invalid bytes as '\65533', but legacy decodes them as '?'
-- 2. if there's an incomplete sequence and no more input immediately
--   available (but not eof), then base will pause to wait for more input,
--   whereas legacy will immediately stop.
whenLegacy s = if legacyEncoding then s else B.empty

main = do
    [p] <- getArgs
    let i = setTerm "xterm"
            Invocation {
                prog = p,
                progArgs = [],
                runInTTY = True,
                environment = []
            }
    runTestTT $ test [interactionTests i, fileStyleTests i]


interactionTests i = "interaction" ~: test
    [ unicodeEncoding i
    , unicodeMovement i
    , tabCompletion i
    , incorrectInput i
    , historyTests i
    , inputChar $ setCharInput i
    , dumbTests $ setTerm "dumb" i
    ]

unicodeEncoding i = "Unicode encoding (valid)" ~:
    [ utf8Test i [utf8 "xαβγy"]
        [prompt 0, utf8 "xαβγy"]
    , utf8Test i [utf8 "a\n", "quit\n"]
        [ prompt 0
        , utf8 "a" <> end
            <> output 0 (utf8 "a") <> prompt 1
        , utf8 "quit" <> end
        ]
    , utf8Test i [utf8 "xαβyψ안기q영\n", "quit\n"]
        [ prompt 0
        , utf8 "xαβyψ안기q영" <> end
            <> output 0 (utf8 "xαβyψ안기q영") <> prompt 1
        , utf8 "quit" <> end
        ]
    -- test buffering: 32 bytes is in middle of a char encoding,
    -- also test long paste
    , "multipleLines" ~: utf8Test i [l1 <> "\n" <> l1]
        [ prompt 0
        , l1 <> end <> output 0 l1 <> prompt 1 <> l1]
    ]
  where
    l1 = utf8 $ T.replicate 30 "안" -- three bytes, width 60

unicodeMovement i = "Unicode movement" ~:
    [ "separate" ~: utf8Test i [utf8 "α", utf8 "\ESC[Dx"]
        [prompt 0, utf8 "α", utf8 "\bxα\b"]
    , "coalesced" ~: utf8Test i [utf8 "α\ESC[Dx"]
        [prompt 0, utf8 "xα\b"]
    , "lineWrap" ~: utf8Test i
        [ utf8 longWideChar 
        , raw [1]
        , raw [5]
        ]
        [prompt 0, utf8 lwc1 <> wrap <> utf8 lwc2 <> wrap <> utf8 lwc3
        , cr <> "\ESC[2A\ESC[2C"
        , cr <> nl <> nl <> "\ESC[22C"
        ]

    ]
  where
    longWideChar = T.concat $ replicate 30 $ "안기영"
    (lwc1,lwcs1) = T.splitAt ((80-2)`div`2) longWideChar
    (lwc2,lwcs2) = T.splitAt (80`div`2) lwcs1
    (lwc3,lwcs3) = T.splitAt (80`div`2) lwcs2
    -- lwc3 has length 90 - (80-2)/2 - 80/2 = 11,
    -- so the last line as wide width 2*11=22.

tabCompletion i = "tab completion" ~:
    [ utf8Test i [ utf8 "dummy-μ\t\t" ]
        [ prompt 0, utf8 "dummy-μασ/" 
            <> nl <> utf8 "bar   ςερτ" <> nl
            <> prompt' 0 <> utf8 "dummy-μασ/"
        ]
    ]

incorrectInput i = "incorrect input" ~:
    [ utf8Test i [ utf8 "x" <> raw [206] ]  -- needs one more byte
        -- non-legacy encoder ignores the "206" since it's still waiting
        -- for more input.
        [ prompt 0, utf8 "x" <> whenLegacy err ]
    , utf8Test i [ raw [206] <> utf8 "x" ]  
        -- 'x' is not valid after '\206', so both the legacy and
        -- non-legacy encoders should handle the "x" correctly.
        [ prompt 0, err <> utf8 "x"]
    , utf8Test i [ raw [236,149] <> utf8 "x" ] -- needs one more byte
        [prompt 0, err <> err <> utf8 "x"]
    ]

historyTests i =  "history encoding" ~:
    [ utf8TestValidHist i [ "\ESC[A" ]
        [prompt 0, utf8 "abcα" ]
    , utf8TestInvalidHist i [ "\ESC[A" ]
        -- NB: this is decoded by either utf8-string or base;
        -- either way they produce \65533 instead of '?'.
        [prompt 0, utf8 "abcα\65533x\65533x\65533" ]
    -- In latin-1: read errors as utf-8 '\65533', display as '?'
    , latin1TestInvalidHist i  [ "\ESC[A" ]
        [prompt 0, utf8 "abc??x?x?" ]
    ]

invalidHist =  utf8 "abcα" 
              `B.append` raw [149] -- invalid start of UTF-8 sequence
              `B.append` utf8 "x"
              `B.append` raw [206] -- incomplete start 
              `B.append` utf8 "x"
              -- incomplete at end of file
              `B.append` raw [206]

validHist = utf8 "abcα"

inputChar i = "getInputChar" ~:
    [ utf8Test i [utf8 "xαβ"]
        [ prompt 0, utf8 "x" <> end <> output 0 (utf8 "x")
          <> prompt 1 <> utf8 "α" <> end <> output 1 (utf8 "α")
          <> prompt 2 <> utf8 "β" <> end <> output 2 (utf8 "β")
          <> prompt 3
        ]
    , "bad encoding (separate)" ~: 
        utf8Test i [utf8 "α", raw [149], utf8 "x", raw [206]]
        [ prompt 0, utf8 "α" <> end <> output 0 (utf8 "α") <> prompt 1
        , err <> end <> output 1 err <> prompt 2
        , utf8 "x" <> end <> output 2 (utf8 "x") <> prompt 3
        , whenLegacy (err <> end <> output 3 err <> prompt 4)
        ]
    , "bad encoding (together)" ~: 
        utf8Test i [utf8 "α" <> raw [149] <> utf8 "x" <> raw [206]]
        [ prompt 0, utf8 "α" <> end <> output 0 (utf8 "α")
        <> prompt 1 <> err <> end <> output 1 err
        <> prompt 2 <> utf8 "x" <> end <> output 2 (utf8 "x")
        <> prompt 3 <> whenLegacy (err <> end <> output 3 err <> prompt 4)
        ]
    , utf8Test i [raw [206]] -- incomplete
        [ prompt 0, whenLegacy (utf8 "?" <> end <> output 0 (utf8 "?"))
        <> whenLegacy (prompt 1)
        ]
    ]

setCharInput i = i { progArgs = ["chars"] }


fileStyleTests i = "file style" ~:
    [ "line input" ~: utf8Test iFile
        [utf8 "xαβyψ안기q영\nquit\n"]
        [ prompt' 0, output 0 (utf8 "xαβyψ안기q영") <> prompt' 1]
    , "char input" ~: utf8Test iFileChar
        [utf8 "xαβt"]
        [ prompt' 0
        , output 0 (utf8 "x")
            <> prompt' 1 <> output 1 (utf8 "α")
            <> prompt' 2 <> output 2 (utf8 "β")
            <> prompt' 3 <> output 3 (utf8 "t")
            <> prompt' 4]
    , "invalid line input" ~: utf8Test iFile
        -- NOTE: the 206 is an incomplete byte sequence,
        -- but we MUST not pause since we're at EOF, not just
        -- end of term.
        -- 
        -- Also recall GHC bug #5436 which caused a crash
        -- if the last byte started an incomplete sequence.
        [ utf8 "a" <> raw [149] <> utf8 "x" <> raw [206] ]
        [ prompt' 0
        , B.empty
        -- It only prompts after the EOF.
        , output 0 (utf8 "a" <> err <> utf8 "x" <> err) <> prompt' 1
        ]
    , "invalid char input (following a newline)" ~: utf8Test iFileChar
        [ utf8 "a\n" <> raw [149] <> utf8 "x\n" <> raw [206] ]
        $ [ prompt' 0
          , output 0 (utf8 "a")
             <> prompt' 1 <> output 1 err
             <> prompt' 2 <> output 2 (utf8 "x")
             <> prompt' 3
             <> whenLegacy (output 3 err <> prompt' 4)
          ] ++ if legacyEncoding then [] else [ output 3 err <> prompt' 4 ]
    , "invalid char file input (no preceding newline)" ~: utf8Test iFileChar
        [ utf8 "a" <> raw [149] <> utf8 "x" <> raw [206] ]
            -- make sure it tries to read a newline
            -- and instead gets the incomplete 206.
            -- This should *not* cause it to crash or block.
        $ [ prompt' 0
          , output 0 (utf8 "a")
             <> prompt' 1 <> output 1 err
             <> prompt' 2 <> output 2 (utf8 "x")
             <> prompt' 3
             <> whenLegacy (output 3 err <> prompt' 4)
          ] ++ if legacyEncoding then [] else [ output 3 err <> prompt' 4 ]
     ]
    -- also single char and buffer break and other stuff
  where
    iFile = i { runInTTY = False }
    iFileChar = setCharInput iFile

-- Test that the dumb terminal backend does encoding correctly.
-- If all the above tests work for the terminfo backend,
-- then we just need to make sure the dumb term plugs into everything
-- correctly, i.e., encodes the input/output and doesn't double-encode.
dumbTests i = "dumb term" ~:
    [ "line input" ~: utf8Test i
        [ utf8 "xαβγy" ]
        [ prompt' 0, utf8 "xαβγy" ]
    , "line input wide movement" ~: utf8Test i
        [ utf8 wideChar, raw [1], raw [5] ]
        [ prompt' 0, utf8 wideChar
        , utf8 (T.replicate 60 "\b")
        , utf8 wideChar
        ]
    , "line char input" ~: utf8Test (setCharInput i)
        [utf8 "xαβ"]
        [ prompt' 0, utf8 "x" <> nl <> output 0 (utf8 "x")
          <> prompt' 1 <> utf8 "α" <> nl <> output 1 (utf8 "α")
          <> prompt' 2 <> utf8 "β" <> nl <> output 2 (utf8 "β")
          <> prompt' 3
        ]
    ]
  where
    wideChar = T.concat $ replicate 10 $ "안기영"

-------------
-- Building blocks for expected input/output

smkx,rmkx :: B.ByteString
smkx = utf8 "\ESC[?1h\ESC="
rmkx = utf8 "\ESC[?1l\ESC>"

prompt, prompt' :: Int -> B.ByteString
prompt k = smkx <> prompt' k

prompt' k = utf8 (T.pack (show k ++ ":"))

end :: B.ByteString
end = nl <> rmkx

cr :: B.ByteString
cr = raw [13]

nl :: B.ByteString
nl = raw [13,10] -- NB: see fixNL: this is really [13,13,10]

output :: Int -> B.ByteString -> B.ByteString
output k s = utf8 (T.pack $ "line " ++ show k ++ ":")
                <> s <> raw [10]

wrap :: B.ByteString
wrap = utf8 " \b"

(<>) :: B.ByteString -> B.ByteString -> B.ByteString
(<>) = B.append

utf8 :: T.Text -> B.ByteString
utf8 = E.encodeUtf8

raw :: [Word8] -> B.ByteString
raw = B.pack

err :: B.ByteString
err = if legacyEncoding
                then utf8 "?"
                else utf8 "\65533"

----------------------

utf8Test = testI . setUTF8

utf8TestInvalidHist i input output = test $ do
    B.writeFile "myhist" $ invalidHist
    assertInvocation (setUTF8 i) input output

utf8TestValidHist i input output = test $ do
    B.writeFile "myhist" validHist
    assertInvocation (setUTF8 i) input output

latin1TestInvalidHist i input output = test $ do
    B.writeFile "myhist" $ invalidHist
    assertInvocation (setLatin1 i) input output
