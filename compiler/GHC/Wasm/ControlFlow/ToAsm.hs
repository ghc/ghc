{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Wasm.ControlFlow.ToAsm
  ( toIndentedAsm
  , noIndentation
  )
where

{-|
Module      : GHC.Wasm.ControlFlow.ToAsm
Description : Convert WebAssembly control-flow instructions to GNU assembler syntax.
-}

import GHC.Prelude

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import Data.List (intersperse)
import Data.Monoid

import GHC.Utils.Panic

import GHC.Wasm.ControlFlow hiding ((<>))

type Indentation = Builder

standardIndentation :: Indentation
standardIndentation = "  "

noIndentation :: Indentation
noIndentation = ""


-- | Assuming that the type of a construct can be rendered as inline
-- syntax, return the syntax.  For every type our translator
-- generates, the assumption should hold.
wasmFunctionType :: WasmFunctionType pre post -> Builder
wasmFunctionType (WasmFunctionType TypeListNil TypeListNil) = "void"
wasmFunctionType (WasmFunctionType TypeListNil (TypeListCons t TypeListNil)) = tagBuilder t
wasmFunctionType _ = panic "function type needs to be externalized"
  -- Anything other then [] -> [], [] -> [t] needs to be put into a
  -- type table and referred to by number.

-- | Tag used in GNU assembly to name a WebAssembly type
tagBuilder :: WasmTypeTag a -> Builder
tagBuilder TagI32 = "i32"
tagBuilder TagF32 = "f32"


type Printer a = Indentation -> a -> Builder

-- | Converts WebAssembly control-flow code into GNU (Clang) assembly
-- syntax, indented for readability.  For ease of combining with other
-- output, the result does not have a trailing newline or preceding
-- indentation.  (The indentation argument simply gives the blank
-- string that follows each emitted newline.)
--
-- The initial `Indentation` argument specifies the indentation of the
-- entire output; for most use cases it will likely be `mempty`.

toIndentedAsm :: forall s e pre post
               . Printer s -> Printer e -> Printer (WasmControl s e pre post)
toIndentedAsm ps pe indent s = print s
  where print, shift :: WasmControl s e pre' post' -> Builder
        newline :: Builder -> Builder -> Builder
        (<+>) :: Builder -> Builder -> Builder
        ty = wasmFunctionType

        -- cases meant to avoid generating any output for `WasmFallthrough`
        print (WasmFallthrough `WasmSeq` s) = print s
        print (s `WasmSeq` WasmFallthrough) = print s
        print (WasmIfTop t s WasmFallthrough) =
            "if" <+> ty t `newline` shift s `newline` "end_if"
        print (WasmIfTop t WasmFallthrough s) =
            "if" <+> ty t `newline` "else" `newline` shift s `newline` "end_if"

        -- all the other cases
        print (WasmPush _ e) = pe indent e
        print (WasmBlock t s) = "block" <+> ty t `newline` shift s `newline` "end_block"
        print (WasmLoop  t s) = "loop"  <+> ty t `newline` shift s `newline` "end_loop"
        print (WasmIfTop t ts fs) = "if" <+> ty t `newline` shift ts `newline`
                                    "else" `newline` shift fs `newline` "end_if"
        print (WasmBr l) = "br" <+> BS.intDec l
        print (WasmBrTable e _ ts t) =
          pe indent e `newline` "br_table {" <+>
          mconcat (intersperse ", " [BS.intDec i | i <- ts <> [t]]) <+>
          "}"
        print (WasmReturnTop _) = "return"
        print (WasmActions as) = ps indent as
        print (s `WasmSeq` s') = print s `newline` print s'

        print WasmFallthrough = "// fallthrough" -- rare

        newline s s' = s <> "\n" <> indent <> s'
        shift s = standardIndentation <> toIndentedAsm ps pe (indent <> standardIndentation) s
        s <+> s' = s <> " " <> s'
