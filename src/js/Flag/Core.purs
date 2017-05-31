module Flag.Core (view, foldp, init) where

import Prelude hiding (div)
import Flag.Types (Countries, Country(..), State, Event(..))
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Random (RANDOM)
import Data.Argonaut (decodeJson)
import Data.Array (uncons, length, drop, head, take)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (h1, img, button, ul, li, div)
import Text.Smolder.HTML.Attributes (src, alt, className, disabled)
import Text.Smolder.Markup (text, (!), (#!))

init ∷ State
init = { countries: [], correct: 0, incorrect: 0 }

foldp ∷ Event → State → EffModel State Event (random ∷ RANDOM, ajax ∷ AJAX)
foldp (Respond name)       state = noEffects $ case ((_ == name) <<< \(Country model) -> model.name) <$> head state.countries of
  (Just true)  -> state { countries = drop 1 state.countries, correct = state.correct + 1 }
  (Just false) -> state { countries = drop 1 state.countries, incorrect = state.incorrect + 1 }
  Nothing      -> state { countries = [], correct = 0, incorrect = 0 }
foldp (Receive (Left err)) state = noEffects $ state { countries = [] }
foldp (Receive (Right xs)) state = noEffects $ state { countries = xs }
foldp (Request)            state = { state: state, effects: [fetchCountries]}
  where
    decode transfer = decodeJson transfer.response :: Either String Countries
    fetchCountries  = do
      response <- attempt $ get "/countries.json"
      let countries = either (Left <<< show) decode response
      pure $ Just $ Receive countries

view ∷ State → HTML Event
view state = div ! className "flagship" $ case uncons state.countries of
  Nothing -> toolbar state
  Just _  -> toolbar state <> country state

toolbar ∷ State → HTML Event
toolbar state = ul ! className "header" $ do
  li $ h1 $ text "Flagship"
  li $ button ! disabled isDisabled #! onClick (const Request) $ text "Start"
  li ! className "score correct" $ text $ show state.correct
  li ! className "score incorrect" $ text $ show state.incorrect
  li ! className "score" $ text $ show $ state.correct + state.incorrect
    where
      isDisabled = if (length state.countries == 0) then "" else "disabled"

country ∷ State → HTML Event
country state = div ! className "prompt" $ do
  case head state.countries of
    Nothing              -> div $ text "All done!"
    Just (Country model) -> do
      img ! src ("images/flags/" <> model.flag) ! alt "Flag"
      ul ! className "countries" $ for_ (take 4 state.countries) $ answer

answer ∷ Country → HTML Event
answer (Country model) =
  li $ button #! onClick (const $ Respond model.name) $ text model.name
