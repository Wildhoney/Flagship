module Flag.App (Event, init, foldp, view) where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array (drop, head, length, uncons)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Text.Smolder.HTML (div, header, nav, section, a, img, ul, li)
import Text.Smolder.HTML.Attributes (className, src)
import Text.Smolder.Markup (text, (!), (#!))
import Flag.Types (Countries)
import Flag.Helpers (fetch, pick)
import Prelude hiding (div)

type State = { all :: Countries, remaining :: Countries, answers :: Countries, correct :: Int, incorrect :: Int }
data Event = RequestCountries | ReceiveCountries (Tuple Countries Countries) | ReceiveAnswer String | ReceiveAnswers Countries

init :: State
init = { all: [], remaining: [], answers: [], correct: 0, incorrect: 0 }

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX, random :: RANDOM)
foldp (ReceiveAnswer name) state = {
  state: case (_ == name) <<< maybe mempty (_.name <<< unwrap) $ head state.remaining of
           true -> state { remaining = drop 1 state.remaining, correct = state.correct + 1 }
           _    -> state { remaining = drop 1 state.remaining, incorrect = state.incorrect + 1 },
  effects: [pure <<< Just <<< ReceiveAnswers <=< liftEff $ pick state.all (drop 1 state.remaining)]
}
foldp (ReceiveAnswers answers) state = noEffects state { answers = answers }
foldp (ReceiveCountries (Tuple all answers)) state = noEffects state { all = all, remaining = all, answers = answers }
foldp (RequestCountries) state = {
  state: state { correct = 0, incorrect = 0 },
  effects: [Just <<< ReceiveCountries <$> fetch]
}

view :: State -> HTML Event
view state = do
  nav do
    header $ text "Flagship"
    div ! className "total" $ text $ show (length state.remaining) <> "/" <> show (length state.all)
    div ! className "correct" $ text $ show state.correct
    div ! className "incorrect" $ text $ show state.incorrect
    a #! onClick (const RequestCountries) $ text $ case (length state.all) of
      0 -> "Start"
      _ -> "Restart"
  section $ case (uncons state.remaining), (state.correct + state.incorrect) of
    Just { head }, _ -> do
      img ! (src <<< ("/images/flags/" <> _) <<< _.flag <<< unwrap $ head)
      ul $ for_ (_.name <<< unwrap <$> state.answers) $ \name ->
        li ! key name #! onClick (const $ ReceiveAnswer name) $ text name
    Nothing, 0 -> div $ text mempty
    Nothing, _ -> do
      section ! className "results" $ do
        div ! className "percentage" $ text $ (show percentage) <> "%"
        div ! className "score" $ text $ "You scored " <> (show state.correct) <> " out of " <> (show total) <> "."
        a #! onClick (const RequestCountries) $ text "Play Again"
      where total = state.correct + state.incorrect
            percentage = round $ toNumber state.correct / (toNumber $ state.correct + state.incorrect) * 100.0
