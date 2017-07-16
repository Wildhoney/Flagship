module Flag.Helpers (answerCount, decode, fetch, shuffle, pick) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Argonaut (decodeJson, Json)
import Data.Array (filter, head, length, replicate, sortBy, take, zip)
import Data.Either (Either, either)
import Data.Maybe (maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Network.HTTP.Affjax (AJAX, get)
import Flag.Types (Countries, Country(..))
import Prelude hiding (div)
  
answerCount :: Int
answerCount = 4

decode :: forall r. { response :: Json | r } -> Either String (Array Country)
decode request = decodeJson request.response :: Either String Countries

fetch :: forall e. Aff (ajax :: AJAX, random :: RANDOM | e) (Tuple Countries Countries)
fetch = get "/countries.json"
  >>= pure <<< decode
  >>= liftEff <<< shuffle <<< either (const []) id
  >>= \countries -> pure <<< Tuple countries <=< liftEff $ pick countries countries

shuffle :: forall e a. Array a -> Eff (random :: RANDOM | e) (Array a)
shuffle xs = do
  randoms <- sequence $ replicate (length xs) random
  pure $ map snd $ sortBy compareFst $ zip randoms xs
  where compareFst (Tuple a _) (Tuple b _) = compare a b

pick :: forall e. Countries -> Countries -> Eff (random :: RANDOM | e) Countries
pick all answers = shuffle <<< (_ <> [answer]) <<< take (answerCount - 1) <<< filter (_ /= answer) <=< shuffle $ all
  where answer = maybe (Country { name: "_", flag: "_" }) id $ head answers
