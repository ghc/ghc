module UnamedConstructorStrictFields where
-- See #15206

data A = A
data B = B

data Foo = MkFoo
  {-# UNPACK #-} !A -- ^ Unpacked strict field
                 B

data Bar =
  {-# UNPACK #-} !A -- ^ Unpacked strict field
    :%%
                 B
