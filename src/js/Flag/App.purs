module Flag.App (Event, Country(..), init, foldp, view) where

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, header, nav, section, a, img, ul, li)
import Text.Smolder.HTML.Attributes (className, src)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)

type State = { countries :: Countries, correct :: Int, incorrect :: Int }
type Countries = Array Country

data Event = RequestCountries | ReceiveCountries Countries | GuessCountry String

newtype Country = Country { name :: String, flag :: String }
derive instance newtypeCountry :: Newtype Country _
instance showCountry :: Show Country where
  show (Country { name, flag }) = "(Country { name: " <> name <> ", flag: " <> flag <> " })"

init :: State
init = { countries: [], correct: 0, incorrect: 0 }

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX)
foldp (GuessCountry name) state = noEffects state { correct = state.correct + 1 }
foldp (ReceiveCountries xs) state = noEffects state { countries = xs }
foldp (RequestCountries) state = { state: state { correct = 0, incorrect = 0 }, effects: [ do
  pure $ Just (ReceiveCountries [Country { name: "Russia", flag: "russia.svg" }]) 
] }

view :: State → HTML Event
view state = do
  nav do
    header $ text "Flagship"
    div ! className "total" $ text $ show (length state.countries)
    div ! className "correct" $ text $ show state.correct
    div ! className "incorrect" $ text $ show state.incorrect
    a #! onClick (const RequestCountries) $ text $ case (length state.countries) of
      0 -> "Start"
      _ -> "Restart"
  section do
    img ! src "/images/flags/indonesia.svg"
    ul do
      li #! onClick (const $ GuessCountry "Ecuador") $ text "Ecuador"
      li #! onClick (const $ GuessCountry "Slovenia") $ text "Slovenia"
      li #! onClick (const $ GuessCountry "Aruba")$ text "Aruba"
      li #! onClick (const $ GuessCountry "Indonesia")$ text "Indonesia"
