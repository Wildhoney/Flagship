module Flag.Types (Countries, Country(..), Name, State, Event(..)) where

import Prelude (bind, ($), pure)
import Data.Either (Either)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
  
type Name = String
type Countries = Array Country
type State = { countries ∷ Countries, correct ∷ Int, incorrect ∷ Int }

data Event = Respond Name | Request | Receive (Either String Countries)
newtype Country = Country { name :: Name, flag :: String }

instance decodeJson ∷ DecodeJson Country where
  decodeJson json = do
    model <- decodeJson json
    name  <- model .? "name"
    flag  <- model .? "flag"
    pure $ Country { name, flag }
