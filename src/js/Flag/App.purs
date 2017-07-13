module Flag.App (Event, Country(..), init, foldp, view) where

import CSS.Transform (offset)
import Data.Array (length, uncons)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, header, nav, section, a, img, ul, li)
import Text.Smolder.HTML.Attributes (className, src)
import Text.Smolder.Markup (text, (!), (#!))
import Prelude hiding (div)

type State = { all :: Countries, current :: Array String, correct :: Int, incorrect :: Int }
type Countries = Array Country

data Event = RequestCountries | ReceiveCountries (Tuple Countries (Array String)) | GuessCountry String

newtype Country = Country { name :: String, flag :: String }
derive instance newtypeCountry :: Newtype Country _
instance showCountry :: Show Country where
  show (Country { name, flag }) = "(Country { name: " <> name <> ", flag: " <> flag <> " })"

init :: State
init = { all: [], current: [], correct: 0, incorrect: 0 }

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX)
foldp (GuessCountry name) state = noEffects state { correct = state.correct + 1 }
foldp (ReceiveCountries (Tuple all current)) state = noEffects state { all = all, current = current }
foldp (RequestCountries) state = { state: state { correct = 0, incorrect = 0 }, effects: [ do
  pure $ Just(ReceiveCountries $ Tuple
    [Country { name: "Ecuador", flag: "ecuador.svg" }]
    ["Ecuador", "Slovenia", "Aruba", "Indonesia"]
  ) 
] }

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
