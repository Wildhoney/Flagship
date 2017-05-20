module Flag.Core (Event, view, foldp, init) where

import Prelude hiding (div)
import Data.Maybe (Maybe(..))
import Data.Array (uncons, length)
import Data.Foldable (for_)
import Pux (EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (h1, img, button, ul, li, div)
import Text.Smolder.Markup (text, (!), (#!))
import Text.Smolder.HTML.Attributes (src, alt, className, disabled)

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
view state = div ! className "flagship" $ case uncons state.countries of
  Nothing -> toolbar state
  Just _  -> toolbar state <> flag state

toolbar ∷ State → HTML Event
toolbar state = ul ! className "header" $ do
  li $ h1 $ text "Flagship"
  li $ button ! disabled isDisabled #! onClick (const RequestCountries) $ text "Start"
    where
      isDisabled = if (length state.countries == 0) then "" else "disabled"

flag ∷ State → HTML Event
flag state = div ! className "prompt" $ do
  img ! src path ! alt "Flag"
  ul ! className "countries" $ for_ state.countries choice
    where
      path = ("images/flags/" <> state.name <> ".svg")

choice ∷ Name → HTML Event
choice name =
  li $ button #! onClick (const $ Country name) $ text name
