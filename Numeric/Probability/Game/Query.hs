-- | A module with functions for querying the probabilities of various outcomes.
module Numeric.Probability.Game.Query (chancePred, chanceRel, chanceTrue) where

import Control.Applicative ((<$>), liftA2)
import Data.Maybe (fromMaybe)

import Numeric.Probability.Game.Event (EventM, outcomes)

-- | Gets the probability that the outcome will satisfy the given predicate.  For
-- example:
--
-- > chancePred (<= 2) d6 == 1/3   -- The chance of getting 2 or less on a d6
-- > chancePred even d6 == 1/2     -- The chance of rolling an event number on a d6
chancePred :: (a -> Bool) -> EventM a -> Rational
chancePred f e = fromMaybe 0 $ lookup True (outcomes (f <$> e))

-- | Gets the probability that the given relation will hold between the two events.
--  For example:
--
-- > chanceRel (==) d6 d6 == 1/6   -- The chance of rolling doubles on d6
-- > chanceRel (>) (2*d6) d12      -- The chance of beating a d12 with two d6
chanceRel :: (a -> a -> Bool) -> EventM a -> EventM a -> Rational
chanceRel f a b = chanceTrue (liftA2 f a b)

-- | Gets the probability that the given boolean-outcome event will give a True
-- outcome.  For example:
--
-- > chanceTrue coinToss == 1/2
-- > chanceTrue ((== 3) <$> d6) == 1/6
--
-- (For the latter example, 'chancePred' is more concise.)
chanceTrue :: EventM Bool -> Rational
chanceTrue = chancePred id
