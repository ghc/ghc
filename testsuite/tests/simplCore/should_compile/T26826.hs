{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}

module T26826 where

import Data.Kind (Type)

type data AstSpan =
  FullSpan | PrimalStepSpan AstSpan | PlainSpan

data SAstSpan (s :: AstSpan) where
  SFullSpan :: SAstSpan FullSpan
  SPrimalStepSpan :: SAstSpan s -> SAstSpan (PrimalStepSpan s)
  SPlainSpan :: SAstSpan PlainSpan

class KnownSpan (s :: AstSpan) where
  knownSpan :: SAstSpan s

instance KnownSpan FullSpan where
  knownSpan = SFullSpan

instance KnownSpan s => KnownSpan (PrimalStepSpan s) where
  knownSpan = SPrimalStepSpan (knownSpan @s)

instance KnownSpan PlainSpan where
  knownSpan = SPlainSpan

class ADReady target where
  ttlet :: target a -> (target a -> target b) -> target b
  ttletPrimal :: target a -> (target a -> target b) -> target b
  ttletPlain :: target a -> (target a -> target b) -> target b
  tplainPart :: target a -> target a
  tfromPlain :: target a -> target a
  tprimalPart :: target a -> target a
  tfromPrimal :: target a -> target a

type SpanTargetFam target (s :: AstSpan) (y :: Type) = target y

type AstEnv target = ()

data AstTensor (s :: AstSpan) (y :: Type) where
  AstLet
    :: forall a b s1 s2.
       KnownSpan s1
    => AstTensor s1 a
    -> AstTensor s2 b
    -> AstTensor s2 b

  AstPrimalPart :: KnownSpan s' => AstTensor s' a -> AstTensor (PrimalStepSpan s') a
  AstFromPrimal :: AstTensor (PrimalStepSpan s') a -> AstTensor s' a
  AstPlainPart :: KnownSpan s' => AstTensor s' a -> AstTensor PlainSpan a
  AstFromPlain :: AstTensor PlainSpan a -> AstTensor s' a

interpretAst
  :: forall target s y. (ADReady target, KnownSpan s)
  => AstEnv target -> AstTensor s y
  -> SpanTargetFam target s y
{-# INLINE [1] interpretAst #-}
interpretAst !env
  = \case
      AstLet @_ @_ @s1 @s2 u v ->
        case knownSpan @s1 of
          SFullSpan ->
            ttlet (interpretAst env u)
              (\_w -> interpretAst env v)
          SPrimalStepSpan _ ->
            ttletPrimal (interpretAst env u)
              (\_w -> interpretAst env v)
          SPlainSpan ->
            ttletPlain (interpretAst env u)
              (\_w -> interpretAst env v)
      AstPrimalPart a ->
        tprimalPart (interpretAst env a)
      AstFromPrimal a ->
        tfromPrimal (interpretAst env a)
      AstPlainPart a ->
        tplainPart (interpretAst env a)
      AstFromPlain a ->
        tfromPlain (interpretAst env a)
