module Flag.Core (Event, view, foldp, init) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (length)
import Pux (EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (h1, img, button)
import Text.Smolder.Markup (text, (!), (#!))
import Text.Smolder.HTML.Attributes (src, alt)

type Name = String
type Countries = Array Name

type State = { name ∷ Name, countries ∷ Countries }
data Event = Country Name | RequestCountries | ReceiveCountries Countries

init ∷ State
init = { name: "Japan", countries: [] }

foldp ∷ ∀ fx. Event → State → EffModel State Event fx
foldp (Country x)           st = noEffects $ st { name = x }
foldp (ReceiveCountries xs) st = noEffects $ st { countries = xs }
foldp (RequestCountries)    st = { state: st, effects: [do
  pure $ Just $ ReceiveCountries ["Russia", "Japan", "Thailand"]
]}

view ∷ State → HTML Event
view state =
  do
    h1 $ text ("Which country has this flag?" <> (show $ length state.countries))
    img ! src flag ! alt "Flag"
    button #! onClick (const $ Country "Russia") $ text state.name
    button #! onClick (const RequestCountries) $ text "Start"
      where
        flag = ("images/flags/" <> state.name <> ".svg")
