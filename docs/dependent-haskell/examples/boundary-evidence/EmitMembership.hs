{-# LANGUAGE NoExperimentalDependentTypes, TemplateHaskell #-}

module EmitMembership (emitMembership, emitFilePosition) where

import BoundaryEvidence (Member (..))
import qualified Data.ByteString.Char8 as Bytes
import Data.List (genericLength)
import Language.Haskell.TH (Exp, Loc (loc_filename), Q, location, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, makeRelativeToProject)
import Numeric.Natural (Natural)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (ReadMode), withBinaryFile)

-- | Search an ordinary list at compile time. Emit a path for GHC to check.
emitMembership :: Natural -> [Natural] -> Q Exp
emitMembership target labels =
    case lookup target (zip labels [0 ..]) of
        Just index -> emitPosition index
        Nothing -> fail ("The offer has no field labelled " ++ show target ++ ".")

-- | Check a proposed zero-based position from a file beside the caller.
emitFilePosition :: FilePath -> [Natural] -> Q Exp
emitFilePosition filename labels = do
    source <- loc_filename <$> location
    relative <- makeRelativeToProject (takeDirectory source </> filename)
    path <- runIO (makeAbsolute relative)
    addDependentFile path
    let count = genericLength labels :: Natural
        -- One optional newline and one extra byte to detect trailing data.
        limit = length (show count) + 2
    input <- runIO (withBinaryFile path ReadMode (`Bytes.hGet` limit))
    case Bytes.readInteger input of
        Just (index, _)
            | index >= 0
            , fromInteger index < count
            , let decimal = Bytes.pack (show index)
            , input == decimal || input == Bytes.snoc decimal '\n' ->
                emitPosition (fromInteger index)
        _ ->
            fail
                ( "Expected a zero-based field position in "
                    ++ filename
                    ++ " for an offer with "
                    ++ show count
                    ++ " fields."
                )

-- | Produce only the constructors that identify this position.
emitPosition :: Natural -> Q Exp
emitPosition 0 = [|Here|]
emitPosition index = [|There $(emitPosition (index - 1))|]
