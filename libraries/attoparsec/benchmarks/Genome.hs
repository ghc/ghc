{-# LANGUAGE OverloadedStrings #-}

module Genome
    (
      genome
    ) where

import Control.Applicative
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Attoparsec.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Attoparsec.Text as T
import qualified Data.Attoparsec.Text.Lazy as TL
import Common (rechunkBS, rechunkT)

genome :: Benchmark
genome = bgroup "genome" [
    bgroup "bytestring" [
        bench "s" $ nf (map (B.parse searchBS)) (B8.tails geneB)
      , bench "l" $ nf (map (BL.parse searchBS)) (L8.tails geneBL)
      , bgroup "CI" [
          bench "s" $ nf (map (B.parse searchBSCI)) (B8.tails geneB)
        , bench "l" $ nf (map (BL.parse searchBSCI)) (L8.tails geneBL)
      ]
    ]
  , bgroup "text" [
        bench "s" $ nf (map (T.parse searchT)) (T.tails geneT)
      , bench "l" $ nf (map (TL.parse searchT)) (TL.tails geneTL)
      , bgroup "CI" [
          bench "s" $ nf (map (T.parse searchTCI)) (T.tails geneT)
        , bench "l" $ nf (map (TL.parse searchTCI)) (TL.tails geneTL)
      ]
    ]
  ]
  where geneB  = B8.pack gene
        geneBL = rechunkBS 4 geneB
        geneT  = T.pack gene
        geneTL = rechunkT 4 geneT

searchBS :: B.Parser ByteString
searchBS = "caac" *> ("aaca" <|> "aact")

searchBSCI :: B.Parser ByteString
searchBSCI = B.stringCI "CAAC" *> (B.stringCI "AACA" <|> B.stringCI "AACT")

searchT :: T.Parser Text
searchT = "caac" *> ("aaca" <|> "aact")

searchTCI :: T.Parser Text
searchTCI = T.asciiCI "CAAC" *> (T.asciiCI "AACA" <|> T.asciiCI "AACT")

-- Dictyostelium discoideum developmental protein DG1094 (gacT) gene,
-- partial cds. http://www.ncbi.nlm.nih.gov/nuccore/AF081586.1

gene :: String
gene = "atcgatttagaaagatacaaagatagaaccatcaataataaacaagagaagagagcaagt\
       \agagatattaataaagagattgaaagagagattgaaaagaagagattatcaccaagagaa\
       \agattaaatttatttggtctttcttcctcatcttcatcagtgaattcaacattaacaaga\
       \tctacagcaaatattatctctacaatagacggtagtggaggtagtaatcgtaatagtaaa\
       \aattatggtaatggctcatcctcctcctcaaatagaagatatagtaatactattaatcaa\
       \caattacaaatgcaattacaacaacttcaaatccaacaacaacaatatcaacaaactcaa\
       \caatctcaaataccattacaatatcaacaacaacaacagcaacaacaacaacaaaccact\
       \acaactacaactacatcaagtggtagtaatagattctcttcaaatagatataaaccagtt\
       \gatcttacacaatcatcttcaaactttcgttattcacgtgaaatttatgatgatgattat\
       \tattcaaataataatttaatgatgtttggtaatgagcaaccaaatcaaacaccaatttct\
       \gtatcatcttcatctgcattcacacgtcaaagatctcaaagttgctttgaaccagagaat\
       \cttgtattgctacaacaacaatatcaacaatatcaacaacaacaacaacaacaacaacaa\
       \attccattccaagcaaatccacaatatagtaatgctgttattgaacaaaaattggatcaa\
       \attagagataccattaataatttacatagagataaccgagtctctaga"
