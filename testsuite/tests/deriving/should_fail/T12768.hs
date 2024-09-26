class C a where { op :: D a => a -> a }
class D a

instance C a  => C [a] where { op = opList }

opList :: (C a, D [a]) => [a] -> [a]
opList = undefined

newtype N a = MkN [a] deriving( C )
