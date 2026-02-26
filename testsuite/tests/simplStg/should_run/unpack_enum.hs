{-# LANGUAGE MagicHash #-}

-- | Test branchless enum unboxing.
-- See Note [UNPACK for enum types] in GHC.Types.Id.Make.
--
-- When a strict field has an enumeration type (all nullary constructors),
-- it should be unpacked to a single narrow primitive word (Word8#, Word16#,
-- or Word#) rather than an unboxed sum, using dataToTag#/tagToEnum# for
-- branchless conversion.

module Main where

import GHC.Exts.Heap.Closures (closureSize, asBox)
import Control.Exception (evaluate)

------------------------------------------------------------
-- Small enum (2 constructors, like Bool)
------------------------------------------------------------

data Toggle = Off | On
  deriving (Show, Eq)

data BoxToggle = BoxToggle {-# UNPACK #-} !Toggle
                           {-# UNPACK #-} !Toggle
  deriving (Show)

------------------------------------------------------------
-- Medium enum (5 constructors)
------------------------------------------------------------

data Color = Red | Green | Blue | Yellow | Purple
  deriving (Show, Eq)

data BoxColor = BoxColor {-# UNPACK #-} !Color
                         {-# UNPACK #-} !Color
  deriving (Show)

------------------------------------------------------------
-- Phantom type parameter enum
-- Tests that the boxer correctly substitutes type variables.
------------------------------------------------------------

data Proxy a = PA | PB | PC
  deriving (Show, Eq)

data BoxProxy = BoxProxy {-# UNPACK #-} !(Proxy Int)
                         {-# UNPACK #-} !(Proxy Char)
  deriving (Show)

------------------------------------------------------------
-- Enum with exactly 255 constructors (boundary for Word8#)
-- With 1-based tags, 255 constructors have tags 1-255 which
-- fit in Word8#.
------------------------------------------------------------

data E255
  = E255_0 | E255_1 | E255_2 | E255_3 | E255_4 | E255_5 | E255_6 | E255_7
  | E255_8 | E255_9 | E255_10 | E255_11 | E255_12 | E255_13 | E255_14 | E255_15
  | E255_16 | E255_17 | E255_18 | E255_19 | E255_20 | E255_21 | E255_22 | E255_23
  | E255_24 | E255_25 | E255_26 | E255_27 | E255_28 | E255_29 | E255_30 | E255_31
  | E255_32 | E255_33 | E255_34 | E255_35 | E255_36 | E255_37 | E255_38 | E255_39
  | E255_40 | E255_41 | E255_42 | E255_43 | E255_44 | E255_45 | E255_46 | E255_47
  | E255_48 | E255_49 | E255_50 | E255_51 | E255_52 | E255_53 | E255_54 | E255_55
  | E255_56 | E255_57 | E255_58 | E255_59 | E255_60 | E255_61 | E255_62 | E255_63
  | E255_64 | E255_65 | E255_66 | E255_67 | E255_68 | E255_69 | E255_70 | E255_71
  | E255_72 | E255_73 | E255_74 | E255_75 | E255_76 | E255_77 | E255_78 | E255_79
  | E255_80 | E255_81 | E255_82 | E255_83 | E255_84 | E255_85 | E255_86 | E255_87
  | E255_88 | E255_89 | E255_90 | E255_91 | E255_92 | E255_93 | E255_94 | E255_95
  | E255_96 | E255_97 | E255_98 | E255_99 | E255_100 | E255_101 | E255_102 | E255_103
  | E255_104 | E255_105 | E255_106 | E255_107 | E255_108 | E255_109 | E255_110 | E255_111
  | E255_112 | E255_113 | E255_114 | E255_115 | E255_116 | E255_117 | E255_118 | E255_119
  | E255_120 | E255_121 | E255_122 | E255_123 | E255_124 | E255_125 | E255_126 | E255_127
  | E255_128 | E255_129 | E255_130 | E255_131 | E255_132 | E255_133 | E255_134 | E255_135
  | E255_136 | E255_137 | E255_138 | E255_139 | E255_140 | E255_141 | E255_142 | E255_143
  | E255_144 | E255_145 | E255_146 | E255_147 | E255_148 | E255_149 | E255_150 | E255_151
  | E255_152 | E255_153 | E255_154 | E255_155 | E255_156 | E255_157 | E255_158 | E255_159
  | E255_160 | E255_161 | E255_162 | E255_163 | E255_164 | E255_165 | E255_166 | E255_167
  | E255_168 | E255_169 | E255_170 | E255_171 | E255_172 | E255_173 | E255_174 | E255_175
  | E255_176 | E255_177 | E255_178 | E255_179 | E255_180 | E255_181 | E255_182 | E255_183
  | E255_184 | E255_185 | E255_186 | E255_187 | E255_188 | E255_189 | E255_190 | E255_191
  | E255_192 | E255_193 | E255_194 | E255_195 | E255_196 | E255_197 | E255_198 | E255_199
  | E255_200 | E255_201 | E255_202 | E255_203 | E255_204 | E255_205 | E255_206 | E255_207
  | E255_208 | E255_209 | E255_210 | E255_211 | E255_212 | E255_213 | E255_214 | E255_215
  | E255_216 | E255_217 | E255_218 | E255_219 | E255_220 | E255_221 | E255_222 | E255_223
  | E255_224 | E255_225 | E255_226 | E255_227 | E255_228 | E255_229 | E255_230 | E255_231
  | E255_232 | E255_233 | E255_234 | E255_235 | E255_236 | E255_237 | E255_238 | E255_239
  | E255_240 | E255_241 | E255_242 | E255_243 | E255_244 | E255_245 | E255_246 | E255_247
  | E255_248 | E255_249 | E255_250 | E255_251 | E255_252 | E255_253 | E255_254
  deriving (Show, Eq, Enum, Bounded)

-- Two E255 fields should fit in a single word (2 x Word8# = 2 bytes),
-- so the closure should be header (1 word) + 1 payload word = size 2.
data BoxE255 = BoxE255 {-# UNPACK #-} !E255
                       {-# UNPACK #-} !E255
  deriving (Show)

------------------------------------------------------------
-- Enum with exactly 256 constructors (just over Word8# boundary).
-- With 1-based tags, tag 256 does not fit in Word8#,
-- so this should use Word16# instead.
------------------------------------------------------------

data E256
  = E256_0 | E256_1 | E256_2 | E256_3 | E256_4 | E256_5 | E256_6 | E256_7
  | E256_8 | E256_9 | E256_10 | E256_11 | E256_12 | E256_13 | E256_14 | E256_15
  | E256_16 | E256_17 | E256_18 | E256_19 | E256_20 | E256_21 | E256_22 | E256_23
  | E256_24 | E256_25 | E256_26 | E256_27 | E256_28 | E256_29 | E256_30 | E256_31
  | E256_32 | E256_33 | E256_34 | E256_35 | E256_36 | E256_37 | E256_38 | E256_39
  | E256_40 | E256_41 | E256_42 | E256_43 | E256_44 | E256_45 | E256_46 | E256_47
  | E256_48 | E256_49 | E256_50 | E256_51 | E256_52 | E256_53 | E256_54 | E256_55
  | E256_56 | E256_57 | E256_58 | E256_59 | E256_60 | E256_61 | E256_62 | E256_63
  | E256_64 | E256_65 | E256_66 | E256_67 | E256_68 | E256_69 | E256_70 | E256_71
  | E256_72 | E256_73 | E256_74 | E256_75 | E256_76 | E256_77 | E256_78 | E256_79
  | E256_80 | E256_81 | E256_82 | E256_83 | E256_84 | E256_85 | E256_86 | E256_87
  | E256_88 | E256_89 | E256_90 | E256_91 | E256_92 | E256_93 | E256_94 | E256_95
  | E256_96 | E256_97 | E256_98 | E256_99 | E256_100 | E256_101 | E256_102 | E256_103
  | E256_104 | E256_105 | E256_106 | E256_107 | E256_108 | E256_109 | E256_110 | E256_111
  | E256_112 | E256_113 | E256_114 | E256_115 | E256_116 | E256_117 | E256_118 | E256_119
  | E256_120 | E256_121 | E256_122 | E256_123 | E256_124 | E256_125 | E256_126 | E256_127
  | E256_128 | E256_129 | E256_130 | E256_131 | E256_132 | E256_133 | E256_134 | E256_135
  | E256_136 | E256_137 | E256_138 | E256_139 | E256_140 | E256_141 | E256_142 | E256_143
  | E256_144 | E256_145 | E256_146 | E256_147 | E256_148 | E256_149 | E256_150 | E256_151
  | E256_152 | E256_153 | E256_154 | E256_155 | E256_156 | E256_157 | E256_158 | E256_159
  | E256_160 | E256_161 | E256_162 | E256_163 | E256_164 | E256_165 | E256_166 | E256_167
  | E256_168 | E256_169 | E256_170 | E256_171 | E256_172 | E256_173 | E256_174 | E256_175
  | E256_176 | E256_177 | E256_178 | E256_179 | E256_180 | E256_181 | E256_182 | E256_183
  | E256_184 | E256_185 | E256_186 | E256_187 | E256_188 | E256_189 | E256_190 | E256_191
  | E256_192 | E256_193 | E256_194 | E256_195 | E256_196 | E256_197 | E256_198 | E256_199
  | E256_200 | E256_201 | E256_202 | E256_203 | E256_204 | E256_205 | E256_206 | E256_207
  | E256_208 | E256_209 | E256_210 | E256_211 | E256_212 | E256_213 | E256_214 | E256_215
  | E256_216 | E256_217 | E256_218 | E256_219 | E256_220 | E256_221 | E256_222 | E256_223
  | E256_224 | E256_225 | E256_226 | E256_227 | E256_228 | E256_229 | E256_230 | E256_231
  | E256_232 | E256_233 | E256_234 | E256_235 | E256_236 | E256_237 | E256_238 | E256_239
  | E256_240 | E256_241 | E256_242 | E256_243 | E256_244 | E256_245 | E256_246 | E256_247
  | E256_248 | E256_249 | E256_250 | E256_251 | E256_252 | E256_253 | E256_254 | E256_255
  deriving (Show, Eq, Enum, Bounded)

data BoxE256 = BoxE256 {-# UNPACK #-} !E256
                       {-# UNPACK #-} !E256
  deriving (Show)

------------------------------------------------------------
-- Boundary size comparison: 5 fields of E255 (Word8#) vs E256 (Word16#)
-- With 5 fields, Word8# fits in fewer payload words than Word16#
-- on all platforms, so we can verify the packing difference.
------------------------------------------------------------

data Box5xE255 = Box5xE255 {-# UNPACK #-} !E255
                            {-# UNPACK #-} !E255
                            {-# UNPACK #-} !E255
                            {-# UNPACK #-} !E255
                            {-# UNPACK #-} !E255

data Box5xE256 = Box5xE256 {-# UNPACK #-} !E256
                            {-# UNPACK #-} !E256
                            {-# UNPACK #-} !E256
                            {-# UNPACK #-} !E256
                            {-# UNPACK #-} !E256

------------------------------------------------------------
-- Test helpers
------------------------------------------------------------

test :: Show a => String -> a -> IO ()
test name value = do
    putStrLn $ "### " ++ name
    value' <- evaluate value
    print value'
    putStrLn ("size: " ++ show (closureSize $ asBox value'))
    putStrLn ""

main :: IO ()
main = do
    -- Small enum: all constructor combinations
    test "toggle_off_off" (BoxToggle Off Off)
    test "toggle_on_on"   (BoxToggle On  On)
    test "toggle_off_on"  (BoxToggle Off On)

    -- Medium enum: first, last, and middle constructors
    test "color_first"  (BoxColor Red    Red)
    test "color_last"   (BoxColor Purple Purple)
    test "color_mixed"  (BoxColor Green  Yellow)

    -- Phantom type parameter: tests boxer substitution
    test "proxy" (BoxProxy PB PC)

    -- 255-constructor enum (boundary): first and last tags
    test "e255_first" (BoxE255 E255_0   E255_0)
    test "e255_last"  (BoxE255 E255_254 E255_254)
    test "e255_mixed" (BoxE255 E255_0   E255_254)

    -- 256-constructor enum (just over Word8# boundary): first, last, and mixed
    test "e256_first" (BoxE256 E256_0   E256_0)
    test "e256_last"  (BoxE256 E256_255 E256_255)
    test "e256_mixed" (BoxE256 E256_0   E256_255)

    -- Boundary size comparison: E255 uses Word8# (1 byte per field),
    -- E256 uses Word16# (2 bytes per field). With 5 fields, the Word16#
    -- version needs more payload words than the Word8# version.
    b255 <- evaluate (Box5xE255 E255_0 E255_127 E255_254 E255_0 E255_127)
    b256 <- evaluate (Box5xE256 E256_0 E256_128 E256_255 E256_0 E256_128)
    let s255 = closureSize (asBox b255)
    let s256 = closureSize (asBox b256)
    putStrLn "### boundary_size_check"
    putStrLn $ "e256 (5 x Word16#) larger than e255 (5 x Word8#): " ++ show (s256 > s255)
    putStrLn ""
