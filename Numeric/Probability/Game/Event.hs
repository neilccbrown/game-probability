-- | A module containing the central type of the library, 'EventM', and various
-- related helper functions.
module Numeric.Probability.Game.Event (EventM, makeEvent, makeEventProb, outcomes, enact,
  coinToss, subst, compareEvent) where

import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Arrow (second)
import Control.Monad (ap, replicateM)
import Data.Map (Map, fromListWith)
import Data.Ratio (denominator)
import Numeric.Probability.Distribution (T(..), certainly, decons, fromFreqs, norm, selectP, uniform)
import System.Random (randomRIO)

-- | A probabilistic event with an outcome of type 'a'.  See the 'enact' function
-- to actually run the event and randomly pick an outcome. 
--
-- For an explanation of the 'Num' instance, see the DieRoll type in the "Numeric.Probability.Game.Dice"
-- module.
--
-- The 'Eq' instance compares the two distributions to see if they are equal.
-- This looks at all the outcomes and sees if their probabilities are equal on
-- the left-hand side and the right-hand side.  For example,
-- @coinToss == fmap (>= 4) d6@, but @d12 /= d6 + d6@.
--
-- The 'Show' instance will display a horizontal bar-chart of relative outcome
-- probability.  Note: this really is a relative probability -- common factors
-- are cancelled, and is not a count of the different outcomes.  If you wish to
-- show the raw numbers, use @show . outcomes@ instead.
-- 
-- The 'Functor' instance allows you to modify the outcome values without changing
-- their associated probabilities.  For example, @fmap show d6@ changes the outcomes
-- into their String representations.
--
-- The 'Applicative' instance allows you to join together the results of two events
-- in a predetermined manner.  For example, @makeEvent [id, (* 2)] \<*\> d6@ allows
-- you to roll a d6 that has a 50% chance of being doubled.  Note that @pure
-- 6@ is an event that is certain to produce the outcome 6.
--
-- The 'Monad' instance allows you to base the choice of the next event on the
-- result of the previous event.  For example, @coinToss >>= \x -> if x then d6
-- else d4@ will roll a d4 50% of the time and a d6 the other 50%.  Note that @return
-- 6@ is an event that is certain to produce the outcome 6.
newtype EventM a = EventM (T Rational a)
  deriving (Monad, Functor)

instance Ord a => Eq (EventM a) where
-- Eq not defined properly for T prob a, work-around for now:
--(===) :: Ord a => EventM a -> EventM a -> Bool
  (==) (EventM a) (EventM b) = decons (norm a) == decons (norm b)

normEventM :: Ord a => EventM a -> EventM a
normEventM (EventM dc) = EventM (norm dc)

instance Num (EventM Int) where
  (+) x y = normEventM $ liftA2 (+) x y
  (-) x y = normEventM $ liftA2 (-) x y
  negate = fmap negate
  abs = normEventM . fmap abs
  signum = normEventM . fmap signum
  (*) = lotsOf
    where
      lotsOf :: EventM Int -> EventM Int -> EventM Int
      lotsOf x y = do n <- x
                      sum <$> replicateM n y
      -- a `lotsOf` b = [(bx,ap*bp) | (ax,ap)<-a,(bx,bp)<- sum (replicate ax b)]
      -- b `lotsOf` c = [(cx,bp*cp) | (bx,bp)<-b,(cx,cp)<- sum (replicate bx c)]
      -- a `lotsOf` (b `lotsOf` c)
      --  = [(bcx,ap*bcp) | (ax,ap)<-a,(bcx,bcp)<- sum (replicate ax [(cx,bp*cp) | (bx,bp)<-b,(cx,cp)<- sum (replicate bx c)])]
  fromInteger = EventM . certainly . fromInteger

instance (Show a, Ord a) => Show (EventM a) where
  show (EventM dc) = showBars (decons (norm dc))

showBars :: Show a => [(a, Rational)] -> String
showBars xs = unlines (map (makeBar . scale) xs)
  where
    den = fromIntegral $ foldr lcm 1 (map (denominator . snd) xs)
    scale (x, r) = (x, floor (r * den)) -- should be integral anyway

    width = maximum (map (length . show . fst) xs)

    makeBar (x, n) = pad (show x) ++ ": " ++ replicate n '#'
      where
        pad s = s ++ replicate (width - length s) ' '

instance Applicative EventM where
  pure = return
  (<*>) = ap

-- | Gets a list of all the outcomes of the event and their associated probability.
--  You can be sure that the probabilities will all sum to 1, and that there will
-- only be one item in the list per outcome.  It is possible that some of the outcomes
-- in the list will have zero probability. 
outcomes :: Ord a => EventM a -> [(a, Rational)]
outcomes (EventM dc) = decons (norm dc)

-- | Makes an event that has an equal chance of taking on the value of each
-- entry in the list.  Note that duplicates in the list are permitted and do
-- have an effect: @makeEvent [True, False]@ has a 50% chance of giving a True
-- result, but @makeEvent [True, True, False, False, False]@ only has a 40%
-- chance of giving a True result.  If you do not want this behaviour, use
-- @makeEvent . nub@ to remove duplicates. 
--
-- The result of passing the empty list is undefined.
makeEvent :: [a] -> EventM a
makeEvent = EventM . uniform

-- | Given a list of events and their associated probabilities, forms a
-- corresponding event.  The probabilities must be non-negative.  If the
-- probabilities do not sum to one, they are all scaled linearly so that their
-- sum is one.  Duplicate items will have their probabilities added.
--
-- The result of passing the empty list, a list containing negative probabilities,
-- or a list where all the probabilities are zero is undefined.
makeEventProb :: (Ord a, Real prob) => [(a, prob)] -> EventM a
makeEventProb = EventM . norm . fromFreqs . map (second toRational)

-- | An event with a 50% chance of giving True, and a 50% chance of giving False.
coinToss :: EventM Bool
coinToss = makeEvent [True, False]

-- | Actually enacts the event and produces a single result according to the probabilities
-- in the @EventM a@ parameter.
enact :: EventM a -> IO a
enact (EventM dc) = selectP (toDouble dc) <$> randomRIO (0, 1)
  where
    toDouble :: T Rational a -> T Double a
    toDouble = Cons . map (second fromRational) . decons

-- | If the @EventM a@ parameter returns a result equal to the first parameter,
-- it is changed to be the second parameter; otherwise it is left untouched.  For
-- example @replace 4 8 d4@ has an equal chance of producing the outcomes 1, 2,
-- 3 and 8, @replace 10 0 d10 == z10@, and @replace 10 20 d6 == d6@.
subst :: Eq a => a -> a -> EventM a -> EventM a
subst x y = fmap (\n -> if x == n then y else n)

-- | Compares the outcomes of the two events, and works out the probability associated
-- with the first outcome being greater than, equal to or less than the second
-- outcome.  The probabilites for each are returned in an associative map.
--
-- Added in version 1.1.
compareEvent :: Ord a => EventM a -> EventM a -> Map Ordering Rational
compareEvent ex ey = fromListWith (+)
  [(compare x y, px * py) | (x, px) <- outcomes ex, (y, py) <- outcomes ey]
    
