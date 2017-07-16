module Flag.Types (Countries, Country(..)) where

import Data.Newtype (class Newtype)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Prelude

newtype Country = Country { name :: String, flag :: String }
type Countries = Array Country

derive instance newtypeCountry :: Newtype Country _
derive instance eqCountry :: Eq Country

instance showCountry :: Show Country where
  show (Country { name, flag }) = "(Country { name: " <> name <> ", flag: " <> flag <> " })"

instance decodeJsonCountry :: DecodeJson Country where
  decodeJson json = do
    model <- decodeJson json
    name <- model .? "name"
    flag <- model .? "flag"
    pure $ Country { name, flag }