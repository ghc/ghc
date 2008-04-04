module CmmTx where

data TxRes a = TxRes Bool a

instance Monad TxRes where
    return = TxRes False

{-  Here you can get a simplifier loop thus:
NOTE: Simplifier still going after 4 iterations; bailing out.  Size = 52
NOTE: Simplifier still going after 4 iterations; bailing out.  Size = 52
NOTE: Simplifier still going after 4 iterations; bailing out.  Size = 52
NOTE: Simplifier still going after 4 iterations; bailing out.  Size = 52

Reason: 'a' is inline (not pre/post unconditionally; just ordinary inlining)
Then, since ($dm>>) has arity 3, the rhs of (>>) is a PAP, so the arg is
floated out, past the big lambdas.

See Note [Unsaturated functions] in SimplUtils 

------------------------------------------------------------
a_s9f{v} [lid] =
  base:GHC.Base.:DMonad{v r5} [gid]
    @ main:CmmTx.TxRes{tc rd}
    >>={v a6E} [lid]
    >>{v a6H} [lid]
    return{v a6J} [lid]
    fail{v a6M} [lid]
>>{v a6H} [lid] [ALWAYS LoopBreaker Nothing] :: forall a{tv a6F} [tv]
                                                       b{tv a6G} [tv].
                                                main:CmmTx.TxRes{tc rd} a{tv a6F} [tv]
                                                -> main:CmmTx.TxRes{tc rd} b{tv a6G} [tv]
                                                -> main:CmmTx.TxRes{tc rd} b{tv a6G} [tv]
[Arity 2
 Str: DmdType LL]
>>{v a6H} [lid] =
  \ (@ a{tv a78} [sk] :: ghc-prim:GHC.Prim.*{(w) tc 34d})
    (@ b{tv a79} [sk] :: ghc-prim:GHC.Prim.*{(w) tc 34d}) ->
    base:GHC.Base.$dm>>{v r5f} [gid]
      @ main:CmmTx.TxRes{tc rd} a_s9f{v} [lid] @ a{tv a78} [sk] @ b{tv a79} [sk]
 -}
