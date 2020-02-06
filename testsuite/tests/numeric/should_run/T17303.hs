import Control.Monad (when)
import Data.Ratio (Ratio, (%))
import Numeric.Natural (Natural)

infix 4 ~=
(~=) :: (Integral a, Show a) => Ratio a -> a -> IO ()
ratio ~= expected = do
  let actual = round ratio
  when (actual /= expected) (fail (unwords
    ["round", show ratio, "expected", show expected, "but got", show actual]))

main :: IO ()
main = do

  -12 % 6 ~= (-2 :: Integer)
  -10 % 6 ~= (-2 :: Integer)
  -9  % 6 ~= (-2 :: Integer)
  -8  % 6 ~= (-1 :: Integer)
  -6  % 6 ~= (-1 :: Integer)
  -4  % 6 ~= (-1 :: Integer)
  -3  % 6 ~= (0  :: Integer)
  -2  % 6 ~= (0  :: Integer)
  0   % 6 ~= (0  :: Integer)
  2   % 6 ~= (0  :: Integer)
  3   % 6 ~= (0  :: Integer)
  4   % 6 ~= (1  :: Integer)
  6   % 6 ~= (1  :: Integer)
  8   % 6 ~= (1  :: Integer)
  9   % 6 ~= (2  :: Integer)
  10  % 6 ~= (2  :: Integer)
  12  % 6 ~= (2  :: Integer)

  0   % 6 ~= (0  :: Natural)
  2   % 6 ~= (0  :: Natural)
  3   % 6 ~= (0  :: Natural)
  4   % 6 ~= (1  :: Natural)
  6   % 6 ~= (1  :: Natural)
  8   % 6 ~= (1  :: Natural)
  9   % 6 ~= (2  :: Natural)
  10  % 6 ~= (2  :: Natural)
  12  % 6 ~= (2  :: Natural)
