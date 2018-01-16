module NoCPP (Haystack) where

-- | For hiding needles.
data Haystack = Haystack

-- | Causes a build failure if the CPP language extension is enabled.
stringGap = "Foo\
\Bar"
