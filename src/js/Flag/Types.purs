module Flag.Types (Countries, Country(..), Name, State, Event(..)) where

import Prelude (bind, ($), pure)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Maybe (Maybe)
  
type Name = String
type Countries = Array Country
type State = { countries ∷ Maybe (Eff (random :: RANDOM) Countries), correct ∷ Int, incorrect ∷ Int }

data Event = Respond Name | Request | Receive (Maybe (Eff (random :: RANDOM) Countries))
newtype Country = Country { name :: Name, flag :: String }

instance decodeJson ∷ DecodeJson Country where
  decodeJson json = do
    model <- decodeJson json
    name  <- model .? "name"
    flag  <- model .? "flag"
    pure $ Country { name, flag }
