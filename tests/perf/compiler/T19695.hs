{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module T19695 where

import Prelude
import Control.Monad (liftM)
import Control.Monad.Trans.RWS.Lazy
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Monoid (Any (..))
import Data.String (IsString (..))
import Data.Char (toUpper, toLower, isAlpha)
import Data.List (intersperse)
import qualified Data.Map as Map
import GHC.Generics

type Handler = ReaderT () IO
type MForm = RWST (Maybe ([(String, Text)], ()), (), ()) Any [Int]
type Text = ByteString

data FormResult a = FormMissing
                  | FormFailure [Text]
                  | FormSuccess a
    deriving Show
instance Functor FormResult where
    fmap _ FormMissing = FormMissing
    fmap _ (FormFailure errs) = FormFailure errs
    fmap f (FormSuccess a) = FormSuccess $ f a
instance Applicative FormResult where
    pure = FormSuccess
    (FormSuccess f) <*> (FormSuccess g) = FormSuccess $ f g
    (FormFailure x) <*> (FormFailure y) = FormFailure $ x ++ y
    (FormFailure x) <*> _ = FormFailure x
    _ <*> (FormFailure y) = FormFailure y
    _ <*> _ = FormMissing
instance Monoid m => Monoid (FormResult m) where
    mempty = pure mempty
instance Semigroup m => Semigroup (FormResult m) where
    x <> y = (<>) <$> x <*> y

mreq :: MonadIO m => String -> MForm m (FormResult Text, ())
mreq v = mhelper v (\_ _ -> FormFailure ["fail"]) FormSuccess
mcountry :: MonadIO m => String -> MForm m (FormResult CountryCode, ())
mcountry v = mhelper v (\_ _ -> FormFailure ["fail"]) go where
  go t = let
    fanl f x= (f x , x)
    m = Map.fromList $ map (fanl $ fromString . countryNameFromCode) [minBound..maxBound]
    in maybe (FormFailure ["fail"]) FormSuccess $ Map.lookup t m

askParams :: Monad m => MForm m (Maybe [(String, Text)])
askParams = do
    (x, _, _) <- ask
    return $ liftM fst x

mhelper
    :: MonadIO m
    => String
    -> (() -> () -> FormResult b)
    -> (Text -> FormResult b)
    -> MForm m (FormResult b, ())
mhelper v onMissing onFound = do
    tell (Any True)
    mp <- askParams
    (res, x) <- case mp of
        Nothing -> return (FormMissing, ())
        Just p -> do
            return $ case lookup v p of
                Nothing -> (onMissing () (), ())
                Just t -> (onFound t, ())
    return (res, x)

data ShippingForm = ShippingForm
  { shCustomerName :: CountryCode
  , shCountry :: CountryCode
  , shPostalCode :: CountryCode
  , shAddress1 :: CountryCode
  , shAddress2 :: CountryCode
  , shCity :: CountryCode
  , shCountyState :: CountryCode
  , shContact :: CountryCode
  , shTelephone :: CountryCode
  , shNotificationEmail :: CountryCode
  , shNotificationCountryCode :: CountryCode
  , shNoOfPackages :: CountryCode
  , shWeight :: CountryCode
  , shGenerateCustomData :: CountryCode
  , shTaxId :: CountryCode
  , shServiceCode :: CountryCode
  } deriving Show

data Match = Match

shippingForm :: Maybe ShippingForm
              -> MForm Handler (FormResult ShippingForm)
shippingForm _ =  do
    customerName <- mcountry "Customer Name"
    country <- mcountry  "Country"
    postalCode <- mcountry "Postal/Zip Code"
    address1 <- mcountry "Address 1"
    address2 <- mcountry "Address 2"
    city <- mcountry "City"
    countyState <- mcountry "County/State"
    contact <- mcountry "Contact"
    telephone <- mcountry "Telephone"
    notificationEmail <- mcountry "Notification Email"
    notificationText <- mcountry "Notification Text"
    noOfPackages <- mcountry "No of Packages"
    weight <- mcountry "Weight"
    generateCustomData <- mcountry "Custom Data"
    taxId <- mcountry "EORI"
    serviceCode <- mcountry "Service"
    return (ShippingForm <$> fst  customerName
                 <*> fst  country
                 <*> fst  postalCode
                 <*> fst  address1
                 <*> fst  address2
                 <*> fst  city
                 <*> fst  countyState
                 <*> fst  contact
                 <*> fst  telephone
                 <*> fst  notificationEmail
                 <*> fst  notificationText
                 <*> fst  noOfPackages
                 <*> fst  weight
                 <*> fst  generateCustomData
                 <*> fst  taxId
                 <*> fst  serviceCode
          )

data CountryCode = CC
   deriving (Eq,Read,Show,Enum,Bounded,Ord,Generic)

countryNameFromCode:: CountryCode -> String
countryNameFromCode CC = "CC"
