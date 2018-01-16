-- !!! Honouring qualified imports lists
-- was: Known bug: Qualified import ignores import list
module M where
import qualified Prelude (map)
x = Prelude.Left 'a'
