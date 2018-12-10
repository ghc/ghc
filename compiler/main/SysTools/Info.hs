{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Compiler information functions
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------
module SysTools.Info where

import Exception
import ErrUtils
import DynFlags
import Outputable
import Util

import Data.List
import Data.IORef

import System.IO

import Platform
import GhcPrelude

import SysTools.Process

{- Note [Run-time linker info]

See also: Trac #5240, Trac #6063, Trac #10110

Before 'runLink', we need to be sure to get the relevant information
about the linker we're using at runtime to see if we need any extra
options. For example, GNU ld requires '--reduce-memory-overheads' and
'--hash-size=31' in order to use reasonable amounts of memory (see
trac #5240.) But this isn't supported in GNU gold.

Generally, the linker changing from what was detected at ./configure
time has always been possible using -pgml, but on Linux it can happen
'transparently' by installing packages like binutils-gold, which
change what /usr/bin/ld actually points to.

Clang vs GCC notes:

For gcc, 'gcc -Wl,--version' gives a bunch of output about how to
invoke the linker before the version information string. For 'clang',
the version information for 'ld' is all that's output. For this
reason, we typically need to slurp up all of the standard error output
and look through it.

Other notes:

We cache the LinkerInfo inside DynFlags, since clients may link
multiple times. The definition of LinkerInfo is there to avoid a
circular dependency.

-}

{- Note [ELF needed shared libs]

Some distributions change the link editor's default handling of
ELF DT_NEEDED tags to include only those shared objects that are
needed to resolve undefined symbols. For Template Haskell we need
the last temporary shared library also if it is not needed for the
currently linked temporary shared library. We specify --no-as-needed
to override the default. This flag exists in GNU ld and GNU gold.

The flag is only needed on ELF systems. On Windows (PE) and Mac OS X
(Mach-O) the flag is not needed.

-}

{- Note [Windows static libGCC]

The GCC versions being upgraded to in #10726 are configured with
dynamic linking of libgcc supported. This results in libgcc being
linked dynamically when a shared library is created.

This introduces thus an extra dependency on GCC dll that was not
needed before by shared libraries created with GHC. This is a particular
issue on Windows because you get a non-obvious error due to this missing
dependency. This dependent dll is also not commonly on your path.

For this reason using the static libgcc is preferred as it preserves
the same behaviour that existed before. There are however some very good
reasons to have the shared version as well as described on page 181 of
https://gcc.gnu.org/onlinedocs/gcc-5.2.0/gcc.pdf :

"There are several situations in which an application should use the
 shared ‘libgcc’ instead of the static version. The most common of these
 is when the application wishes to throw and catch exceptions across different
 shared libraries. In that case, each of the libraries as well as the application
 itself should use the shared ‘libgcc’. "

-}

neededLinkArgs :: LinkerInfo -> [Option]
neededLinkArgs (GnuLD o)     = o
neededLinkArgs (GnuGold o)   = o
neededLinkArgs (LlvmLLD o)   = o
neededLinkArgs (DarwinLD o)  = o
neededLinkArgs (SolarisLD o) = o
neededLinkArgs (AixLD o)     = o
neededLinkArgs UnknownLD     = []

-- Grab linker info and cache it in DynFlags.
getLinkerInfo :: DynFlags -> IO LinkerInfo
getLinkerInfo dflags = do
  info <- readIORef (rtldInfo dflags)
  case info of
    Just v  -> return v
    Nothing -> do
      v <- getLinkerInfo' dflags
      writeIORef (rtldInfo dflags) (Just v)
      return v

-- See Note [Run-time linker info].
getLinkerInfo' :: DynFlags -> IO LinkerInfo
getLinkerInfo' dflags = do
  let platform = targetPlatform dflags
      os = platformOS platform
      (pgm,args0) = pgm_l dflags
      args1     = map Option (getOpts dflags opt_l)
      args2     = args0 ++ args1
      args3     = filter notNull (map showOpt args2)

      -- Try to grab the info from the process output.
      parseLinkerInfo stdo _stde _exitc
        | any ("GNU ld" `isPrefixOf`) stdo =
          -- GNU ld specifically needs to use less memory. This especially
          -- hurts on small object files. Trac #5240.
          -- Set DT_NEEDED for all shared libraries. Trac #10110.
          -- TODO: Investigate if these help or hurt when using split sections.
          return (GnuLD $ map Option ["-Wl,--hash-size=31",
                                      "-Wl,--reduce-memory-overheads",
                                      -- ELF specific flag
                                      -- see Note [ELF needed shared libs]
                                      "-Wl,--no-as-needed"])

        | any ("GNU gold" `isPrefixOf`) stdo =
          -- GNU gold only needs --no-as-needed. Trac #10110.
          -- ELF specific flag, see Note [ELF needed shared libs]
          return (GnuGold [Option "-Wl,--no-as-needed"])

        | any ("LLD" `isPrefixOf`) stdo =
          return (LlvmLLD $ map Option [
                                      -- see Note [ELF needed shared libs]
                                      "-Wl,--no-as-needed"])

         -- Unknown linker.
        | otherwise = fail "invalid --version output, or linker is unsupported"

  -- Process the executable call
  info <- catchIO (do
             case os of
               OSSolaris2 ->
                 -- Solaris uses its own Solaris linker. Even all
                 -- GNU C are recommended to configure with Solaris
                 -- linker instead of using GNU binutils linker. Also
                 -- all GCC distributed with Solaris follows this rule
                 -- precisely so we assume here, the Solaris linker is
                 -- used.
                 return $ SolarisLD []
               OSAIX ->
                 -- IBM AIX uses its own non-binutils linker as well
                 return $ AixLD []
               OSDarwin ->
                 -- Darwin has neither GNU Gold or GNU LD, but a strange linker
                 -- that doesn't support --version. We can just assume that's
                 -- what we're using.
                 return $ DarwinLD []
               OSMinGW32 ->
                 -- GHC doesn't support anything but GNU ld on Windows anyway.
                 -- Process creation is also fairly expensive on win32, so
                 -- we short-circuit here.
                 return $ GnuLD $ map Option
                   [ -- Reduce ld memory usage
                     "-Wl,--hash-size=31"
                   , "-Wl,--reduce-memory-overheads"
                     -- Emit gcc stack checks
                     -- Note [Windows stack usage]
                   , "-fstack-check"
                     -- Force static linking of libGCC
                     -- Note [Windows static libGCC]
                   , "-static-libgcc" ]
               _ -> do
                 -- In practice, we use the compiler as the linker here. Pass
                 -- -Wl,--version to get linker version info.
                 (exitc, stdo, stde) <- readProcessEnvWithExitCode pgm
                                        (["-Wl,--version"] ++ args3)
                                        c_locale_env
                 -- Split the output by lines to make certain kinds
                 -- of processing easier. In particular, 'clang' and 'gcc'
                 -- have slightly different outputs for '-Wl,--version', but
                 -- it's still easy to figure out.
                 parseLinkerInfo (lines stdo) (lines stde) exitc
            )
            (\err -> do
                debugTraceMsg dflags 2
                    (text "Error (figuring out linker information):" <+>
                     text (show err))
                errorMsg dflags $ hang (text "Warning:") 9 $
                  text "Couldn't figure out linker information!" $$
                  text "Make sure you're using GNU ld, GNU gold" <+>
                  text "or the built in OS X linker, etc."
                return UnknownLD)
  return info

-- Grab compiler info and cache it in DynFlags.
getCompilerInfo :: DynFlags -> IO CompilerInfo
getCompilerInfo dflags = do
  info <- readIORef (rtccInfo dflags)
  case info of
    Just v  -> return v
    Nothing -> do
      v <- getCompilerInfo' dflags
      writeIORef (rtccInfo dflags) (Just v)
      return v

-- See Note [Run-time linker info].
getCompilerInfo' :: DynFlags -> IO CompilerInfo
getCompilerInfo' dflags = do
  let (pgm,_) = pgm_c dflags
      -- Try to grab the info from the process output.
      parseCompilerInfo _stdo stde _exitc
        -- Regular GCC
        | any ("gcc version" `isInfixOf`) stde =
          return GCC
        -- Regular clang
        | any ("clang version" `isInfixOf`) stde =
          return Clang
        -- FreeBSD clang
        | any ("FreeBSD clang version" `isInfixOf`) stde =
          return Clang
        -- XCode 5.1 clang
        | any ("Apple LLVM version 5.1" `isPrefixOf`) stde =
          return AppleClang51
        -- XCode 5 clang
        | any ("Apple LLVM version" `isPrefixOf`) stde =
          return AppleClang
        -- XCode 4.1 clang
        | any ("Apple clang version" `isPrefixOf`) stde =
          return AppleClang
         -- Unknown linker.
        | otherwise = fail "invalid -v output, or compiler is unsupported"

  -- Process the executable call
  info <- catchIO (do
                (exitc, stdo, stde) <-
                    readProcessEnvWithExitCode pgm ["-v"] c_locale_env
                -- Split the output by lines to make certain kinds
                -- of processing easier.
                parseCompilerInfo (lines stdo) (lines stde) exitc
            )
            (\err -> do
                debugTraceMsg dflags 2
                    (text "Error (figuring out C compiler information):" <+>
                     text (show err))
                errorMsg dflags $ hang (text "Warning:") 9 $
                  text "Couldn't figure out C compiler information!" $$
                  text "Make sure you're using GNU gcc, or clang"
                return UnknownCC)
  return info
