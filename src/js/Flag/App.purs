module Flag.App (Event, Country(..), init, foldp, view) where

import Data.Array (length, uncons, head, take, drop)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for_)
import Data.Argonaut (class DecodeJson, decodeJson, Json, (.?))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, header, nav, section, a, img, ul, li)
import Text.Smolder.HTML.Attributes (className, src)
import Text.Smolder.Markup (text, (!), (#!))
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax (AJAX, get)
import Prelude hiding (div)

answers :: Int
answers = 4

type State = { all :: Countries, current :: Array String, correct :: Int, incorrect :: Int }
type Countries = Array Country
data Event = RequestCountries | ReceiveCountries (Tuple Countries (Array String)) | GuessCountry String
newtype Country = Country { name :: String, flag :: String }

derive instance newtypeCountry :: Newtype Country _

instance showCountry :: Show Country where
  show (Country { name, flag }) = "(Country { name: " <> name <> ", flag: " <> flag <> " })"

instance decodeJsonCountry :: DecodeJson Country where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    flag <- obj .? "flag"
    pure $ Country { name, flag }

init :: State
init = { all: [], current: [], correct: 0, incorrect: 0 }

decode ∷ ∀ r. { response :: Json | r } → Either String (Array Country)
decode request = decodeJson request.response :: Either String Countries

fetch :: forall e. Aff (ajax :: AJAX | e) (Tuple Countries (Array String))
fetch = get "/countries.json" >>= \xs -> case decode xs of
  Left _          -> pure $ Tuple [] []
  Right countries -> pure $ Tuple countries (take answers $ _.name <<< unwrap <$> countries)

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX)
foldp (GuessCountry name) state = noEffects $ case (_ == name) <<< maybe "" (_.name <<< unwrap) $ head state.all of
  true -> state { all = drop 1 state.all, correct = state.correct + 1 }
  _    -> state { all = drop 1 state.all, incorrect = state.incorrect + 1 }
foldp (ReceiveCountries (Tuple all current)) state = noEffects state { all = all, current = current }
foldp (RequestCountries) state = {
  state: state { correct = 0, incorrect = 0 },
  effects: [Just <<< ReceiveCountries <$> fetch]
}

view :: State → HTML Event
view state = do
  nav do
    header $ text "Flagship"
    div ! className "total" $ text $ show (length state.all)
    div ! className "correct" $ text $ show state.correct
    div ! className "incorrect" $ text $ show state.incorrect
    a #! onClick (const RequestCountries) $ text $ case (length state.all) of
      0 -> "Start"
      _ -> "Restart"
  section $ case (uncons state.all) of
    Just { head } -> do
      img ! (src <<< ("/images/flags/" <> _) <<< _.flag <<< unwrap $ head)
      ul $ for_ state.current $ \name -> li #! onClick (const $ GuessCountry name) $ text name
    Nothing       -> div $ text "..."
