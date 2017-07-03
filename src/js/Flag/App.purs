module Flag.App (Event, Country(..), init, foldp, view) where

import Data.Array (length)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)

type State = { countries :: Maybe Countries, correct :: Int, incorrect :: Int }
type Countries = Array Country

data Event = RequestCountries | ReceiveCountries (Maybe Countries)

newtype Country = Country { name :: String, flag :: String }
derive instance newtypeCountry :: Newtype Country _
instance showCountry :: Show Country where
  show (Country { name, flag }) = "(Country { name: " <> name <> ", flag: " <> flag <> " })"

init :: State
init = { countries: Nothing, correct: 0, incorrect: 0 }

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX)
foldp (ReceiveCountries xs) state = noEffects state { countries = xs }
foldp (RequestCountries) state = onlyEffects state [do
  pure $ Just (ReceiveCountries (Just [Country { name: "Russia", flag: "russia.svg" }]))  
]

view :: State → HTML Event
view state =
  div ! className "flagship" $ do
    div ! className "total" $ text $ show (maybe 0 length state.countries)
    div ! className "correct" $ text $ show state.correct
    div ! className "incorrect" $ text $ show state.incorrect
    button #! onClick (const RequestCountries) $ text "Start!"
