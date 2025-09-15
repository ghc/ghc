-- !!! Haskell-98 prefix negate operator

-- Make sure the parsing is actually the correct
-- one by running this after it's compiled.

negatedExpression = - (3 + 4)

negatedTightlyBinding = -3^4

negatedNonSection = (- 3)

negatedNonSectionWithHighPrecedenceOp =
  let { f = (+); infix 9 `f` } in ( -3 `f` 4 )

negatedNonSectionWithLowPrecedenceOp =
  let { f = (+); infix 1 `f` } in ( -3 `f` 4 )

negatedRightHandSide =
-- This is actually not legal syntax:  3 * - 4
-- However, lower-precedence binary ops work.
-- (see H98 syntax for exp, or imagine it's because it
--  would parse differently as 3 * 0 - 4)
  let { f = (+); infix 1 `f` } in ( 3 `f` - 4 )


subtractionNotNegation = 3 -4

negativePattern =
    case -3 of { (- 3) ->
    case -4 of { - 4 ->
    True } }
-- not legal H98 syntax:  case -4 of { _x @ -4 ->
-- (parentheses needed)    case -5 of { ~ -5 ->

subtractionNotNegationPattern =
    -- defines infix '-' (shadowing Prelude definition)
    let { 3 -4 = True } in (3 - 4)

precedenceOfNegationCantBeChanged =
    let { (-) = undefined; infix 9 - } in (- 3 * 4)

negationCantBeQualified =
    (Prelude.-3) 4

main = do
  print negatedExpression
  print negatedTightlyBinding
  print negatedNonSection
  print negatedNonSectionWithHighPrecedenceOp
  print negatedNonSectionWithLowPrecedenceOp
  print negatedRightHandSide
  print subtractionNotNegation
  print negativePattern
  print subtractionNotNegationPattern
  print precedenceOfNegationCantBeChanged
  print negationCantBeQualified

