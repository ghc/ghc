module Main where

import GHC.Exts.Heap.Closures
import Control.Exception (evaluate)
import Data.Word (Word32)
import Data.Int (Int8, Int16)

-- this should get a Word8 tag
data E1
  = E1_1 | E1_2 | E1_3 | E1_4 | E1_5 | E1_6 | E1_7 | E1_8
  | E1_9 | E1_10 | E1_11 | E1_12 | E1_13 | E1_14 | E1_15 | E1_16
  | E1_17 | E1_18 | E1_19 | E1_20 | E1_21 | E1_22 | E1_23 | E1_24
  | E1_25 | E1_26 | E1_27 | E1_28 | E1_29 | E1_30 | E1_31 | E1_32
  | E1_33 | E1_34 | E1_35 | E1_36 | E1_37 | E1_38 | E1_39 | E1_40
  | E1_41 | E1_42 | E1_43 | E1_44 | E1_45 | E1_46 | E1_47 | E1_48
  | E1_49 | E1_50 | E1_51 | E1_52 | E1_53 | E1_54 | E1_55 | E1_56
  | E1_57 | E1_58 | E1_59 | E1_60 | E1_61 | E1_62 | E1_63 | E1_64
  | E1_65 | E1_66 | E1_67 | E1_68 | E1_69 | E1_70 | E1_71 | E1_72
  | E1_73 | E1_74 | E1_75 | E1_76 | E1_77 | E1_78 | E1_79 | E1_80
  | E1_81 | E1_82 | E1_83 | E1_84 | E1_85 | E1_86 | E1_87 | E1_88
  | E1_89 | E1_90 | E1_91 | E1_92 | E1_93 | E1_94 | E1_95 | E1_96
  | E1_97 | E1_98 | E1_99 | E1_100 | E1_101 | E1_102 | E1_103 | E1_104
  | E1_105 | E1_106 | E1_107 | E1_108 | E1_109 | E1_110 | E1_111 | E1_112
  | E1_113 | E1_114 | E1_115 | E1_116 | E1_117 | E1_118 | E1_119 | E1_120
  | E1_121 | E1_122 | E1_123 | E1_124 | E1_125 | E1_126 | E1_127 | E1_128
  | E1_129 | E1_130 | E1_131 | E1_132 | E1_133 | E1_134 | E1_135 | E1_136
  | E1_137 | E1_138 | E1_139 | E1_140 | E1_141 | E1_142 | E1_143 | E1_144
  | E1_145 | E1_146 | E1_147 | E1_148 | E1_149 | E1_150 | E1_151 | E1_152
  | E1_153 | E1_154 | E1_155 | E1_156 | E1_157 | E1_158 | E1_159 | E1_160
  | E1_161 | E1_162 | E1_163 | E1_164 | E1_165 | E1_166 | E1_167 | E1_168
  | E1_169 | E1_170 | E1_171 | E1_172 | E1_173 | E1_174 | E1_175 | E1_176
  | E1_177 | E1_178 | E1_179 | E1_180 | E1_181 | E1_182 | E1_183 | E1_184
  | E1_185 | E1_186 | E1_187 | E1_188 | E1_189 | E1_190 | E1_191 | E1_192
  | E1_193 | E1_194 | E1_195 | E1_196 | E1_197 | E1_198 | E1_199 | E1_200
  | E1_201 | E1_202 | E1_203 | E1_204 | E1_205 | E1_206 | E1_207 | E1_208
  | E1_209 | E1_210 | E1_211 | E1_212 | E1_213 | E1_214 | E1_215 | E1_216
  | E1_217 | E1_218 | E1_219 | E1_220 | E1_221 | E1_222 | E1_223 | E1_224
  | E1_225 | E1_226 | E1_227 | E1_228 | E1_229 | E1_230 | E1_231 | E1_232
  | E1_233 | E1_234 | E1_235 | E1_236 | E1_237 | E1_238 | E1_239 | E1_240
  | E1_241 | E1_242 | E1_243 | E1_244 | E1_245 | E1_246 | E1_247 | E1_248
  | E1_249 | E1_250 | E1_251 | E1_252 | E1_253 | E1_254
  deriving (Enum, Bounded, Show)

-- this should get a Word8 tag
data E2
  = E2_1 | E2_2 | E2_3 | E2_4 | E2_5 | E2_6 | E2_7 | E2_8
  | E2_9 | E2_10 | E2_11 | E2_12 | E2_13 | E2_14 | E2_15 | E2_16
  | E2_17 | E2_18 | E2_19 | E2_20 | E2_21 | E2_22 | E2_23 | E2_24
  | E2_25 | E2_26 | E2_27 | E2_28 | E2_29 | E2_30 | E2_31 | E2_32
  | E2_33 | E2_34 | E2_35 | E2_36 | E2_37 | E2_38 | E2_39 | E2_40
  | E2_41 | E2_42 | E2_43 | E2_44 | E2_45 | E2_46 | E2_47 | E2_48
  | E2_49 | E2_50 | E2_51 | E2_52 | E2_53 | E2_54 | E2_55 | E2_56
  | E2_57 | E2_58 | E2_59 | E2_60 | E2_61 | E2_62 | E2_63 | E2_64
  | E2_65 | E2_66 | E2_67 | E2_68 | E2_69 | E2_70 | E2_71 | E2_72
  | E2_73 | E2_74 | E2_75 | E2_76 | E2_77 | E2_78 | E2_79 | E2_80
  | E2_81 | E2_82 | E2_83 | E2_84 | E2_85 | E2_86 | E2_87 | E2_88
  | E2_89 | E2_90 | E2_91 | E2_92 | E2_93 | E2_94 | E2_95 | E2_96
  | E2_97 | E2_98 | E2_99 | E2_100 | E2_101 | E2_102 | E2_103 | E2_104
  | E2_105 | E2_106 | E2_107 | E2_108 | E2_109 | E2_110 | E2_111 | E2_112
  | E2_113 | E2_114 | E2_115 | E2_116 | E2_117 | E2_118 | E2_119 | E2_120
  | E2_121 | E2_122 | E2_123 | E2_124 | E2_125 | E2_126 | E2_127 | E2_128
  | E2_129 | E2_130 | E2_131 | E2_132 | E2_133 | E2_134 | E2_135 | E2_136
  | E2_137 | E2_138 | E2_139 | E2_140 | E2_141 | E2_142 | E2_143 | E2_144
  | E2_145 | E2_146 | E2_147 | E2_148 | E2_149 | E2_150 | E2_151 | E2_152
  | E2_153 | E2_154 | E2_155 | E2_156 | E2_157 | E2_158 | E2_159 | E2_160
  | E2_161 | E2_162 | E2_163 | E2_164 | E2_165 | E2_166 | E2_167 | E2_168
  | E2_169 | E2_170 | E2_171 | E2_172 | E2_173 | E2_174 | E2_175 | E2_176
  | E2_177 | E2_178 | E2_179 | E2_180 | E2_181 | E2_182 | E2_183 | E2_184
  | E2_185 | E2_186 | E2_187 | E2_188 | E2_189 | E2_190 | E2_191 | E2_192
  | E2_193 | E2_194 | E2_195 | E2_196 | E2_197 | E2_198 | E2_199 | E2_200
  | E2_201 | E2_202 | E2_203 | E2_204 | E2_205 | E2_206 | E2_207 | E2_208
  | E2_209 | E2_210 | E2_211 | E2_212 | E2_213 | E2_214 | E2_215 | E2_216
  | E2_217 | E2_218 | E2_219 | E2_220 | E2_221 | E2_222 | E2_223 | E2_224
  | E2_225 | E2_226 | E2_227 | E2_228 | E2_229 | E2_230 | E2_231 | E2_232
  | E2_233 | E2_234 | E2_235 | E2_236 | E2_237 | E2_238 | E2_239 | E2_240
  | E2_241 | E2_242 | E2_243 | E2_244 | E2_245 | E2_246 | E2_247 | E2_248
  | E2_249 | E2_250 | E2_251 | E2_252 | E2_253 | E2_254 | E2_255
  deriving (Enum, Bounded, Show)

-- this needs a Word16 tag
data E3
  = E3_1 | E3_2 | E3_3 | E3_4 | E3_5 | E3_6 | E3_7 | E3_8
  | E3_9 | E3_10 | E3_11 | E3_12 | E3_13 | E3_14 | E3_15 | E3_16
  | E3_17 | E3_18 | E3_19 | E3_20 | E3_21 | E3_22 | E3_23 | E3_24
  | E3_25 | E3_26 | E3_27 | E3_28 | E3_29 | E3_30 | E3_31 | E3_32
  | E3_33 | E3_34 | E3_35 | E3_36 | E3_37 | E3_38 | E3_39 | E3_40
  | E3_41 | E3_42 | E3_43 | E3_44 | E3_45 | E3_46 | E3_47 | E3_48
  | E3_49 | E3_50 | E3_51 | E3_52 | E3_53 | E3_54 | E3_55 | E3_56
  | E3_57 | E3_58 | E3_59 | E3_60 | E3_61 | E3_62 | E3_63 | E3_64
  | E3_65 | E3_66 | E3_67 | E3_68 | E3_69 | E3_70 | E3_71 | E3_72
  | E3_73 | E3_74 | E3_75 | E3_76 | E3_77 | E3_78 | E3_79 | E3_80
  | E3_81 | E3_82 | E3_83 | E3_84 | E3_85 | E3_86 | E3_87 | E3_88
  | E3_89 | E3_90 | E3_91 | E3_92 | E3_93 | E3_94 | E3_95 | E3_96
  | E3_97 | E3_98 | E3_99 | E3_100 | E3_101 | E3_102 | E3_103 | E3_104
  | E3_105 | E3_106 | E3_107 | E3_108 | E3_109 | E3_110 | E3_111 | E3_112
  | E3_113 | E3_114 | E3_115 | E3_116 | E3_117 | E3_118 | E3_119 | E3_120
  | E3_121 | E3_122 | E3_123 | E3_124 | E3_125 | E3_126 | E3_127 | E3_128
  | E3_129 | E3_130 | E3_131 | E3_132 | E3_133 | E3_134 | E3_135 | E3_136
  | E3_137 | E3_138 | E3_139 | E3_140 | E3_141 | E3_142 | E3_143 | E3_144
  | E3_145 | E3_146 | E3_147 | E3_148 | E3_149 | E3_150 | E3_151 | E3_152
  | E3_153 | E3_154 | E3_155 | E3_156 | E3_157 | E3_158 | E3_159 | E3_160
  | E3_161 | E3_162 | E3_163 | E3_164 | E3_165 | E3_166 | E3_167 | E3_168
  | E3_169 | E3_170 | E3_171 | E3_172 | E3_173 | E3_174 | E3_175 | E3_176
  | E3_177 | E3_178 | E3_179 | E3_180 | E3_181 | E3_182 | E3_183 | E3_184
  | E3_185 | E3_186 | E3_187 | E3_188 | E3_189 | E3_190 | E3_191 | E3_192
  | E3_193 | E3_194 | E3_195 | E3_196 | E3_197 | E3_198 | E3_199 | E3_200
  | E3_201 | E3_202 | E3_203 | E3_204 | E3_205 | E3_206 | E3_207 | E3_208
  | E3_209 | E3_210 | E3_211 | E3_212 | E3_213 | E3_214 | E3_215 | E3_216
  | E3_217 | E3_218 | E3_219 | E3_220 | E3_221 | E3_222 | E3_223 | E3_224
  | E3_225 | E3_226 | E3_227 | E3_228 | E3_229 | E3_230 | E3_231 | E3_232
  | E3_233 | E3_234 | E3_235 | E3_236 | E3_237 | E3_238 | E3_239 | E3_240
  | E3_241 | E3_242 | E3_243 | E3_244 | E3_245 | E3_246 | E3_247 | E3_248
  | E3_249 | E3_250 | E3_251 | E3_252 | E3_253 | E3_254 | E3_255 | E3_256
  deriving (Enum, Bounded, Show)

data U_Bool = U_Bool {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !Bool
    deriving (Show)

data U_E1 = U_E1 {-# UNPACK #-} !E1
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
    deriving (Show)

{- In `data U_E`, the {-# UNPACK #-} !E1 gives rise to a pretty clumsy expression
   for the wrapper for U_E1. Here is what it looks like when ther are only 16
   data constructors in E1, and we have just
       data U_E1 = U_E1 {-# UNPACK #-} !E1
   Blimey!

Main.$WU_E1
  = \ (conrep_t1N4 [Occ=Once1!] :: Main.E1) ->
      case case conrep_t1N4 of {
             Main.E1_1 ->
               GHC.Internal.Types.(# _| | | | | | | | | | | | | | | #)
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 GHC.Internal.Types.(##);
             Main.E1_2 ->
               GHC.Internal.Types.(# |_| | | | | | | | | | | | | | #)
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 GHC.Internal.Types.(##);
             Main.E1_3 ->
               GHC.Internal.Types.(# | |_| | | | | | | | | | | | | #)
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @GHC.Internal.Types.ZeroBitRep
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 @(# #)
                 GHC.Internal.Types.(##);

       ... etc ....
-}

data U_E2 = U_E2 {-# UNPACK #-} !E2
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
    deriving (Show)

{-
  disabled to reduce memory consumption of test

data U_E3 = U_E3 {-# UNPACK #-} !E3
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
                 {-# UNPACK #-} !Int8
    deriving (Show)

data U_Mixed = U_Mixed {-# UNPACK #-} !E1
                       {-# UNPACK #-} !Int8
                       {-# UNPACK #-} !E2
                       {-# UNPACK #-} !Int16
                       {-# UNPACK #-} !Int16
                       {-# UNPACK #-} !Int16
                       {-# UNPACK #-} !Bool
                       {-# UNPACK #-} !Bool
    deriving (Show)
-}

data U_Maybe = U_Maybe {-# UNPACK #-} !(Maybe Bool)
                       {-# UNPACK #-} !(Maybe Bool)
                       {-# UNPACK #-} !(Maybe Bool)
                       {-# UNPACK #-} !(Maybe Bool)
                       {-# UNPACK #-} !(Maybe Bool)
                       {-# UNPACK #-} !(Maybe Bool)
                       {-# UNPACK #-} !(Maybe Bool)
                       {-# UNPACK #-} !(Maybe Bool)
    deriving (Show)


data MaybeW32 = NothingW32
              | JustW32 {-# UNPACK #-} !Word32
    deriving (Show)

data U_MaybeW32 = U_MaybeW32 {-# UNPACK #-} !MaybeW32
                             {-# UNPACK #-} !MaybeW32
                             {-# UNPACK #-} !MaybeW32
                             {-# UNPACK #-} !MaybeW32
                             {-# UNPACK #-} !MaybeW32
                             {-# UNPACK #-} !MaybeW32
                             {-# UNPACK #-} !MaybeW32
                             {-# UNPACK #-} !MaybeW32
    deriving (Show)

u_ba :: U_Bool
u_ba = U_Bool minBound maxBound minBound maxBound
              minBound maxBound minBound maxBound

u_e1a :: U_E1
u_e1a = U_E1 minBound maxBound minBound maxBound
             minBound maxBound minBound maxBound

u_e1b :: U_E1
u_e1b = U_E1 maxBound minBound maxBound minBound
             maxBound minBound maxBound minBound

u_e1c :: U_E1
u_e1c = U_E1 E1_1 126 127 0 1 2 3 4

u_e1d :: U_E1
u_e1d = U_E1 E1_254 126 127 0 1 2 3 4

u_e2a :: U_E2
u_e2a = U_E2 minBound maxBound minBound maxBound
             minBound maxBound minBound maxBound
{-
u_e3a :: U_E3
u_e3a = U_E3 minBound maxBound minBound maxBound
             minBound maxBound minBound maxBound

u_mixed :: U_Mixed
u_mixed = U_Mixed maxBound minBound maxBound minBound
                  maxBound minBound maxBound minBound
-}

u_maybe :: U_Maybe
u_maybe = U_Maybe Nothing (Just False) Nothing (Just True)
                  Nothing (Just False) Nothing (Just True)

u_maybeW32 :: U_MaybeW32
u_maybeW32 = U_MaybeW32 NothingW32 (JustW32 minBound)
                        NothingW32 (JustW32 maxBound)
                        NothingW32 (JustW32 minBound)
                        NothingW32 (JustW32 maxBound)

test :: Show a => String -> a -> IO ()
test name value = do
    putStrLn $ "\n### " ++ name
    value' <- evaluate value
    print value'
    putStrLn ("size: " ++ show (closureSize $ asBox value'))

main :: IO ()
main = do
    test "u_ba"       u_ba
    test "u_e1a"      u_e1a
    test "u_e1b"      u_e1b
    test "u_e1c"      u_e1c
    test "u_e1d"      u_e1d
    test "u_e2a"      u_e2a
    -- test "u_e3a"      u_e3a
    -- test "u_mixed"    u_mixed
    test "u_maybe"    u_maybe
    test "u_maybeW32" u_maybeW32
