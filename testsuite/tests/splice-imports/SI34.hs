module SI34 where

-- Compiling SI34 @ R, requires SI34M2 @ R, which requires SI34M1 @ R,
-- but NOT SI34M1 @ C or SI34M2 @ C due to ImplicitStagePersistence + TemplateHaskellQuotes
import SI34M2

-- Uses the MkT constructor indirectly through SI34M2.makeMkT
foo = makeMkT 42

-- Uses the wrapper type from SI34M2
bar = wrapT (makeMkT 100)