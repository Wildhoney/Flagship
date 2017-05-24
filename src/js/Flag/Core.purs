module Flag.Core (Event, Country, view, foldp, init) where

import Prelude hiding (div)
import Data.Maybe (Maybe(..))
import Data.Array (uncons, length, replicate, zip, sortBy, drop, head, take)
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..), fst)
import Data.Traversable (sequence)
import Control.Monad.Eff.Exception (Error)
import Data.Foldable (for_)
import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Control.Monad.Aff (attempt, liftEff')
import Control.Monad.Eff.Random (RANDOM, random)
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (h1, img, button, ul, li, div)
import Text.Smolder.Markup (text, (!), (#!))
import Text.Smolder.HTML.Attributes (src, alt, className, disabled)

type Name = String
type Countries = Array Country
type State = { countries ∷ Countries, correct ∷ Int, incorrect ∷ Int }

data Event = Answer Name | RequestCountries | ReceiveCountries (Either String Countries)
newtype Country = Country { name :: Name, flag :: String }

instance decodeJson ∷ DecodeJson Country where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    flag <- obj .? "flag"
    pure $ Country { name, flag }

init ∷ State
init = { countries: [], correct: 0, incorrect: 0 }

foldp ∷ Event → State → EffModel State Event (random ∷ RANDOM, ajax ∷ AJAX)
foldp (Answer name) st = noEffects $ case head st.countries of
  Nothing              -> st { countries = [], correct = 0, incorrect = 0 }
  Just (Country model) -> if name == model.name then isCorrect else isIncorrect
    where
      countries   = drop 1 st.countries
      isCorrect   = st { countries = countries, correct = st.correct + 1 }
      isIncorrect = st { countries = countries, incorrect = st.incorrect + 1 }
foldp (ReceiveCountries (Left err)) st = noEffects $ st { countries = [] }
foldp (ReceiveCountries (Right xs)) st = noEffects $ st { countries = xs }
foldp (RequestCountries)            st = { state: st, effects: [do
  response <- attempt $ get "/countries.json"
  let countries = either (Left <<< show) decode response
  pure $ Just $ ReceiveCountries countries
]}
  where
    decode transfer = decodeJson transfer.response :: Either String Countries
    -- compareSnd (Tuple _ a) (Tuple _ b) = compare a b
    -- sort (Left err)        = Left <<< show $ err
    -- sort (Right countries) = Right $ liftEff' $ map (Country <<< fst) <$> sortBy compareSnd <$>
    --                                  zip countries <$> (sequence $ replicate (length countries) random)

view ∷ State → HTML Event
view state = div ! className "flagship" $ case uncons state.countries of
  Nothing -> toolbar state
  Just _  -> toolbar state <> country state

toolbar ∷ State → HTML Event
toolbar state = ul ! className "header" $ do
  li $ h1 $ text "Flagship"
  li $ button ! disabled isDisabled #! onClick (const RequestCountries) $ text "Start"
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
  li $ button #! onClick (const $ Answer model.name) $ text model.name
