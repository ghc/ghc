module TopLevel (module Level1, module Level2.Level2) where

import Level1
import Level2.Level2


-- ASCII syms
(..-) = undefined :: ()
(..+) = undefined :: ()
(..=) = undefined :: ()
(..>>) = undefined :: ()
(..!>) = undefined :: ()

-- General Punctuation
(⁐) = undefined :: () -- U+2050
(⁐⁑) = undefined :: () -- ... U+2051

-- supAndSubScriptR
-- likely everything is this group is rejected
-- moved to failing T10550a test

-- currencySymbolR
(₽) = undefined :: () -- U+20BD
(₽€) = undefined :: () -- ... U+20AC

-- letterLikeSymbolR
(⅀) = undefined :: () -- U+2140
(⅀⅀) = undefined :: () -- ... U+2140

-- numberFormsR
-- likely everything is this group is rejected
-- moved to failing T10550b test

-- enclosedAlphanumericsR
(⒡) = undefined :: () -- U+24A1
(⒡Ⓞ) = undefined :: () -- ... U+24C4
-- some from this group is rejected, e.g. ③
-- added to failing T10550d test

-- enclosedAlphanumericSupplementR
(🄐) = undefined :: () -- U+1F110
(🄐🄐) = undefined :: () -- ... U+1F110
-- some from this group is rejected, e.g. 🄀
-- added failing test case T10550d

-- enclosedIdeographicSupplementR
(🈐) = undefined :: () -- U+1F210
(🈐🈭) = undefined :: () -- ... U+1F22D

-- arrowsR
(←) = undefined :: () -- U+2190
(←→) = undefined :: () -- ... U+2192

-- supplementalArrowsAR
(⟹) = undefined :: () -- U+27F9
(⟹⟿) = undefined :: () -- .. U+27FF

-- supplementalArrowsBR
(⤴) = undefined :: () -- U+2934
(⤴⤵) = undefined :: () -- ... U+2935

-- supplementalArrowsCR
(🡘) = undefined :: () -- U+1F858
(🡘🢕) = undefined :: () -- ... U+1F895

-- miscellaneousSymbolsAndArrowsR
(⬤) = undefined :: () -- U+2B24
(⬤⬱) = undefined :: () -- ... U+2B31

-- dingbatArrowsR
(➾) = undefined :: () -- U+27BE
(➾➔) = undefined :: () -- ... U+2794

-- mathematicalOperators
(∀) = undefined :: () -- U+2200
(∀⋙) = undefined :: () -- ... U+22D9

-- miscellaneousMathematicalSymbolsAR
(⟑) = undefined :: () -- U+27D1
(⟑⟑) = undefined :: () -- ... U+27E9
-- some from this group is rejected, e.g. ⟨
-- added failing test case T10550e

-- miscellaneousMathematicalSymbolsBR
(⧦) = undefined :: () -- U+29E6
(⧦⧵) = undefined :: () -- ... U+29F5

-- supplementalMathematicalOperatorsR
(⨶) = undefined :: () -- U+2A36
(⨶⫫) = undefined :: () -- ... U+2AEB

-- mathematicalAlphanumericSymbolsR
(𝛌) = undefined :: () -- U+1D6CC
(𝛌𝕘) = undefined :: () -- ... U+1D558

-- miscellaneousTechnicalR
(⌘) = undefined :: () -- U+2318
(⌘⌥) = undefined :: () -- ... U+2325

-- controlPicturesR
(␘) = undefined :: () -- U+2418
(␘␡) = undefined :: () -- ... U+2421

-- characterRecognitionR
(⑁) = undefined :: () -- U+2441
(⑁⑅) = undefined :: () -- ... U+2445

-- byzantineMusicalSymbolsR
(𝀐) = undefined :: () -- U+1DO1O
(𝀐𝃆) = undefined :: () -- ... U+1D0C6

-- musicalSymbolsR
(𝄢) = undefined :: () -- U+1D122
(𝄢𝇇) = undefined :: () -- ... U+1D1C7

-- ancientGreekMusicalNotationR
(𝉀) = undefined :: () -- U+1D240
(𝉀𝈒) = undefined :: () -- ... U+1D212

-- mahjongTilesR
(🀐) = undefined :: () -- U+1F010
(🀐🀢) = undefined :: () -- ... U+1F022

-- dominoTilesR
(🀱) = undefined :: () -- U+1F031
(🀱🁧) = undefined :: () -- ... U+1F067

-- playingCardsR
(🂿) = undefined :: () -- U+1F0BF
(🂿🃠) = undefined :: () -- ... U+1F0E0

-- miscellaneousSymbolsR
(☀) = undefined :: () -- U+2600
(☀☭) = undefined :: () -- ... U+262D

-- emoticonsR
-- likely everything is this group is rejected
-- added failing test case T10550f

-- miscellaneousSymbolsAndPictographsR
(🌓) = undefined :: () -- U+1F313
(🌓🐇) = undefined :: () -- ... U+1F407

-- transportAndMapSymbolsR
(🚭) = undefined :: () -- U+1F6AD
(🚭🚀) = undefined :: () -- ... U+1F680

-- dingbatsR
(✔) = undefined :: () -- U+2714
(✔✩) = undefined :: () -- ... U+2729

-- combiningDiacriticalMarksForSymbolsR
-- combining unicode symbols should be handled and tested in some smart manner
-- added failing T10550g test case

-- boxDrawingR
(━) = undefined :: () -- U+2501
(━╃) = undefined :: () -- ... U+2543

-- blockElementsR
(▙) = undefined :: () -- U+2599
(▙▟) = undefined :: () -- ... U+259F

-- geometricShapesR
(△) = undefined :: () -- U+25B3
(△◉) = undefined :: () -- ... U+25C9

-- geometricShapesExtendedR
(🞋) = undefined :: () -- U+1F78B
(🞋🞯) = undefined :: () -- ... U+1F7AF

-- ornamentalDingbatsR
(🙫) = undefined :: () -- U+1F66B
(🙫🙢) = undefined :: () -- ... U+1F662

-- arabicMathematicalAlphabeticSymbolsR
(𞺂) = undefined :: () -- U+1EE82
(𞺂𞹟) = undefined :: () -- ... U+1EE5F

-- alchemicalSymbolsR
(🜄) = undefined :: () -- U+1F704
(🜄🝪) = undefined :: () -- ... U+1F76A
