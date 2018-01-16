main = print spın̈alTap
    where spın̈alTap = 11

-- n̈ is a combining character sequence. We now allow it to be used in
-- identifiers (#7650).
--
-- > map generalCategory "n̈"
-- [LowercaseLetter,NonSpacingMark]
--
-- > map show "n̈"
-- ["'n'","'\776'"]
