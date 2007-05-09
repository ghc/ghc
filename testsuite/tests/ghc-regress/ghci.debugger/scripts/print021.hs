-- Test that we can recover unicode DataCons in :print
data T 
  = À		-- latin
  | Α		-- greek
  | Ⴀ		-- georgian
  | Ϣ		-- coptic
  | А		-- cyrillic
  | Ա		-- armenian
  deriving Show

test =
  [ À		-- latin
  , Α		-- greek
  , Ⴀ		-- georgian
  , Ϣ		-- coptic
  , А		-- cyrillic
  , Ա		-- armenian
  ]
