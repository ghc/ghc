{-# LANGUAGE CPP #-}
module System.Environment.CompatSpec (main, spec) where

import           Test.Hspec
#if __GLASGOW_HASKELL__ >= 702
import           Test.QuickCheck
#endif

import qualified Control.Exception as E
import           GHC.IO.Exception (IOErrorType (InvalidArgument))
import           System.Environment.Compat
import           System.IO.Error

main :: IO ()
main = hspec spec

withEnv :: String -> String -> IO a -> IO a
withEnv k v action = E.bracket save restore $ \_ -> do
  setEnv k v >> action
  where
    save    = lookupEnv k
    restore = maybe (unsetEnv k) (setEnv k)

withoutEnv :: String -> IO a -> IO a
withoutEnv k action = E.bracket save restore $ \_ -> do
  unsetEnv k >> action
  where
    save    = lookupEnv k
    restore = maybe (unsetEnv k) (setEnv k)

spec :: Spec
spec = do
  describe "lookupEnv" $ do
    it "returns specified environment variable" $ do
      withEnv "FOOBAR" "23" $ do
        lookupEnv "FOOBAR" `shouldReturn` Just "23"

    it "returns Nothing if specified environment variable is not set" $ do
      withoutEnv "FOOBAR" $ do
        lookupEnv "FOOBAR" `shouldReturn` Nothing

  describe "unsetEnv" $ do
    it "removes specified environment variable" $ do
      setEnv "FOO" "foo"
      unsetEnv "FOO"
      getEnv "FOO" `shouldThrow` isDoesNotExistError

    it "does nothing if specified environment variable is not set" $ do
      unsetEnv "BAR"
      unsetEnv "BAR"
      getEnv "BAR" `shouldThrow` isDoesNotExistError

    it "throws an exception if key is the empty string" $ do
      unsetEnv "" `shouldThrow` (== InvalidArgument) . ioeGetErrorType

    it "throws an exception if key contains '='" $ do
      unsetEnv "some=key" `shouldThrow` (== InvalidArgument) . ioeGetErrorType

#if __GLASGOW_HASKELL__ >= 702
    it "works for arbitrary keys" $
      property $ \k -> ('\NUL' `notElem` k && '=' `notElem` k && (not . null) k) ==> do
        setEnv k "foo"
        unsetEnv k
        getEnv k `shouldThrow` isDoesNotExistError
#endif

  describe "setEnv" $ do
    it "sets specified environment variable to given value" $ do
      unsetEnv "FOO"
      setEnv "FOO" "foo"
      getEnv "FOO" `shouldReturn` "foo"

    it "resets specified environment variable, if it is already set" $ do
      unsetEnv "FOO"
      setEnv "FOO" "foo"
      setEnv "FOO" "bar"
      getEnv "FOO" `shouldReturn` "bar"

    it "removes specified environment variable when value is the empty string" $ do
      setEnv "FOO" "foo"
      setEnv "FOO" ""
      getEnv "FOO" `shouldThrow` isDoesNotExistError

    it "removes specified environment variable when first character of value is NUL" $ do
      setEnv "FOO" "foo"
      setEnv "FOO" "\NULfoo"
      getEnv "FOO" `shouldThrow` isDoesNotExistError

    it "truncates value at NUL character" $ do
      unsetEnv "FOO"
      setEnv "FOO" "foo\NULbar"
      getEnv "FOO" `shouldReturn` "foo"

    it "truncates key at NUL character" $ do
      unsetEnv "FOO"
      setEnv "FOO\NULBAR" "foo"
      getEnv "FOO" `shouldReturn` "foo"

#if __GLASGOW_HASKELL__ >= 702
    it "works for unicode" $ do
      unsetEnv "FOO"
      setEnv "FOO" "foo-\955-bar"
      getEnv "FOO" `shouldReturn` "foo-\955-bar"

    it "works for arbitrary values" $
      property $ \v -> ('\NUL' `notElem` v && (not . null) v) ==> do
        setEnv "FOO" v
        getEnv "FOO" `shouldReturn` v
#endif

    it "works for unicode keys" $ do
      setEnv "foo-\955-bar" "foo"
      getEnv "foo-\955-bar" `shouldReturn` "foo"

    it "throws an exception if key is the empty string" $ do
      setEnv "" "foo" `shouldThrow` (== InvalidArgument) . ioeGetErrorType

    it "throws an exception if key contains '='" $ do
      setEnv "some=key" "foo" `shouldThrow` (== InvalidArgument) . ioeGetErrorType

#if __GLASGOW_HASKELL__ >= 702
    it "works for arbitrary keys" $
      property $ \k -> ('\NUL' `notElem` k && '=' `notElem` k && (not . null) k) ==> do
        setEnv k "foo"
        getEnv k `shouldReturn` "foo"
#endif
