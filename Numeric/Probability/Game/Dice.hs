
-- | A module containing various definitions of dice as random events, and a few
-- associated helper functions.  See 'DieRoll', which is really a synonym for 'EventM'
-- Int.
module Numeric.Probability.Game.Dice (
  -- * Main die roll type
  DieRoll, roll,
  -- * Dice definitions
  d4, d6, d8, z10, d10, d12, d20, d100, z100, d, z,
  -- * Dice helper functions
  rerollOn) where

import Numeric.Probability.Game.Event (EventM, enact, makeEvent, makeEventProb, outcomes)

-- | A type synonym for events with an integer outcome (i.e. all standard die rolls).
--
-- The 'Num' instance for @EventM Int@ allows you to add the results of two die
-- rolls, or subtract them (if it helps, @(+) = liftA2 (+)@).
--
-- Multiplication works as follows.  @d * e@ evaluates the first die roll,
-- then sums that many rolls of the second.  So @2 * d6@ rolls two d6 and adds
-- the outcomes.  However, this definition means that @d6 * 2@ rolls one d6,
-- then effectively scales the result by 2.  And @d6 * d4@ rolls one d6, then
-- rolls that number of @d4@, adding their results together.  The simple rule
-- when one of the terms is a constant is: use the constant on the left-hand
-- side to get more dice, and use the constant on the right-hand side to scale
-- the result.
type DieRoll = EventM Int


-- | A die with an equal chance of rolling 1, 2, 3 or 4.
d4 :: DieRoll
d4 = d 4

-- | A die with an equal chance of rolling 1, 2, 3, 4, 5 or 6.
d6 :: DieRoll
d6 = d 6

-- | A die with an equal chance of rolling 1, 2, 3, 4, 5, 6, 7 or 8.
d8 :: DieRoll
d8 = d 8

-- | A die with an equal chance of rolling 0, 1, 2, 3, 4, 5, 6, 7, 8 or 9.
z10 :: DieRoll
z10 = z 10

-- | A die with an equal chance of rolling 1, 2, 3, 4, 5, 6, 7, 8, 9 or 10.
d10 :: DieRoll
d10 = d 10

-- | A die with an equal chance of rolling 1 to 12 inclusive.
d12 :: DieRoll
d12 = d 12

-- | A die with an equal chance of rolling 1 to 20 inclusive.
d20 :: DieRoll
d20 = d 20

-- | A die with an equal chance of rolling 1 to 100 inclusive.
d100 :: DieRoll
d100 = d 100

-- | A die with an equal chance of rolling 0 to 99 inclusive.
z100 :: DieRoll
z100 = z 100

-- | Makes a die that has an equal chance of achieving the numbers 1 through the
-- number given.  @d 4@ has an equal chance of producing the outcomes 1, 2, 3 and
-- 4, @d 1@ is equivalent to @return 1@ (a certain result of 1), and
-- @d@ is undefined for any number below 1.  For convenience, all the standard dice are
-- provided, e.g. @d6 = d 6@.
d :: Int -> DieRoll
d n = makeEvent [1..n]

-- | Makes a die that has an equal chance of achieving the numbers 0 through
-- the one less than the number given.  @z 4@ has an equal chance of producing
-- the outcomes 0, 1, 2 and 3, while @z 1@ is equivalent to @return 0@ (a
-- certain result of 0), and @z@ is undefined for any number below 1.  For
-- convenience, several standard dice that can be interpreted with a lower
-- result of 0 are provided, e.g. @z10 = z 10@.
z :: Int -> DieRoll
z n = makeEvent [0 .. (n - 1)]

-- | Rerolls the die when the specified outcome(s) occur.  This has the effect
-- of removing the outcomes from the set of outcomes and rescaling all the other
-- probabilities linearly to sum to 1.  For example:
--
-- > d6 `rerollOn` [5,6] == d4
-- > chancePred (== 12) ((2*d6) `rerollOn` [7]) == 1/30
--
-- With the latter example, the standard chance of 12 on 2d6 is 1\/36, which
-- is rescaled by 36\/30, the reciprocal of the chance of /not/ hitting
-- a 7.
rerollOn :: DieRoll -> [Int] -> DieRoll
rerollOn dc ns = makeEventProb $ filter ((`notElem` ns) . fst) $ outcomes dc

-- | A nice synonym for 'enact': actually rolls the die and produces a single result according to the probabilities
-- in the @EventM a@ parameter.
roll :: DieRoll -> IO Int
roll = enact

