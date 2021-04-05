main = print ⅯⅯⅩⅩ
    where ⅯⅯⅩⅩ = 11

-- ⅯⅯⅩⅩ is characters are in NumberLetter unicode category.
-- We now allow it to be used in identifiers, but they
-- are not lower or upper, so cannot be the first one.
--
-- Just like 'OtherNumber' (#4373), 'ModifierLetter' (#10196) and
-- NonSpacingMark (#7650).
--
-- > map generalCategory "ⅯⅯⅩⅩ"
-- [LetterNumber,LetterNumber,LetterNumber,LetterNumber]
--
-- > map show "ⅯⅯⅩⅩ"
-- ["'\\8559'","'\\8559'","'\\8553'","'\\8553'"]
