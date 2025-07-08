{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

-- | Monadic front-end to Text.PrettyPrint
module Language.Haskell.TH.PprLib (
    ($$),
    ($+$),
    (<+>),
    (<>),
    arrow,
    braces,
    brackets,
    cat,
    char,
    colon,
    comma,
    dcolon,
    double,
    doubleQuotes,
    empty,
    equals,
    fcat,
    float,
    fsep,
    hang,
    hcat,
    hsep,
    int,
    integer,
    isEmpty,
    lbrace,
    lbrack,
    lparen,
    nest,
    parens,
    pprName,
    pprName',
    ptext,
    punctuate,
    quotes,
    rational,
    rbrace,
    rbrack,
    rparen,
    semi,
    sep,
    space,
    text,
    to_HPJ_Doc,
    vcat,
    Doc,
    PprM,
)
where

import Prelude hiding ((<>))
#if MIN_VERSION_ghc_boot_th(9,13,0)
import GHC.Boot.TH.PprLib
#else
import GHC.Internal.TH.PprLib
#endif
