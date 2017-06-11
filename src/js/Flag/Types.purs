module Flag.Types (Countries, Country(..), Name, State, Event(..)) where

import Prelude (bind, ($), pure)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Maybe (Maybe)
  
type Name = String
type Countries = Array Country
type State = { countries ∷ Maybe Countries, correct ∷ Int, incorrect ∷ Int }

data Event = Respond Name | Request | Receive (Maybe Countries)
newtype Country = Country { name :: Name, flag :: String }

instance decodeJson ∷ DecodeJson Country where
  decodeJson json = do
    model <- decodeJson json
    name  <- model .? "name"
    flag  <- model .? "flag"
    pure $ Country { name, flag }
