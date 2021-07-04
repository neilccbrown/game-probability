-- | Functions for efficiently calculating the probability of drawing cards.  Here
-- are some examples of using the module.
--
-- In the game Dominion you start out with a deck consisting of 7 \"Copper\" cards
-- and 3 \"Estate\" cards.  On your first turn you draw five cards from this deck.
--  To calculate the chances of drawing the different number of \"Copper\" cards
-- (as a map from number of \"Copper\" cards to probability), you can use this code:
--
-- > copperChances :: Map Int Rational
-- > copperChances = chanceMap startingDeck (drawCount (== "Copper") 5)
-- >   where startingDeck = makeCards $ replicate 7 "Copper " ++ replicate 3 "Estate"
--
-- You could equally define a data-type for the cards rather than use Strings,
-- but often Strings are easiest for one-off queries.
-- 
-- As a different example, in the game Ticket To Ride: Europe, the deck of cards consists of 12 cards of each
-- of eight colours and 14 multi-colour cards.  We can describe it using a custom
-- data-type this time:
--
-- > data TTRECard = Purple | White | Blue | Yellow | Orange | Black | Red | Green | MultiColour
--
-- > ttreDeck :: Cards TTRECard
-- > ttreDeck = replicate 14 MultiColour ++ concatMap (replicate 12) [Purple, White, Blue, Yellow, Orange, Black, Red, Green]
--
-- In the game, there are always 5 communal cards visible.  Imagine you wanted
-- to calculate the probability of receiving a particular colour when drawing from
-- the deck.  You must first remove the cards in your hand and those visible communal
-- cards (we'll ignore the discards), then calculate the probability for drawing
-- one card with the 'draw' function:
--
-- > colourChances :: Map TTRECard Rational
-- > colourChances = chanceMap (ttreDeck `minusCards` (myHand `mappend` communal)) draw
--
-- This will give you a map from TTRECard (i.e. colour) to probability.
--
-- To continue with that example, when you build tunnels in the game, you must
-- lay out the required number of coloured cards, then draw three from the
-- deck.  If any of the three match the colour of tunnel you are building, you
-- must pay that many additional cards.  You may want a function that, given
-- your hand (we'll ignore the communal cards to keep the example shorter) and
-- the length of the tunnel, works out if you are likely to make it.  One way to
-- do this is:
--
-- > tunnel :: Cards TTRECard -> Int -> Rational
-- > tunnel myHand n = chance (ttreDeck `minusCards` myHand)
-- >                          (drawCount match 3 >>= ensure . (<= spare))
-- >   where
-- >     spare = length (filter match $ sortedCards myHand) - n
-- >     match a = a == MultiColour || a == tunnelColour
--
-- That should be fairly fast.  But to illustrate how to speed up these calculations,
-- here is another, faster way to do this: pre-process the cards into those that
-- match and those that don't, using 'chanceOn':
--
-- > tunnel :: Cards TTRECard -> Int -> Rational
-- > tunnel myHand n = chanceOn match (ttreDeck `minusCards` myHand)
-- >                          (drawCount (== True) 3 >>= ensure . (<= spare))
-- >   where
-- >     spare = length (filter match $ sortedCards myHand) - n
-- >     match a = a == MultiColour || a == tunnelColour
--
-- This may seem like a relatively small difference, and indeed it is a small change
-- to the code.  However, it will execute much faster, because the 'chanceOn' function
-- only has two different card values to consider: True, and False, so it just
-- considers those two.  Previously it had to consider the nine different types
-- of card separately, even though only two would match (the function has no way
-- of knowing that a priori).
module Numeric.Probability.Game.Cards.Hand
  (-- * The DrawM type and helper functions.
   DrawM, ensure, badHand, interleave,
   -- * Drawing cards
   draw, drawAny, drawWhere, drawUntil, drawCount, drawSame, drawSameOn, drawGroups, drawGroupsOn,
   -- * Calculating chances
   chance, chanceOn, chanceMap, chanceMapOn,
   -- * Drawing as a random event
   eventDraw, eventDrawOn, eventDrawMaybe, eventDrawMaybeOn) where

import Control.Applicative as A (Alternative(..), Applicative(..), (<$>))
import Control.Arrow ((&&&), first, second)
import Control.Monad (ap, liftM, replicateM)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Map as M (Map, elems, keys, singleton, empty, insertWith, 
  fromList, lookup, toList, unionsWith, mapKeysMonotonic, mapWithKey, unionWith, size, update)
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F (foldr, sum)
import Data.Ratio ((%))

import Numeric.Probability.Game.Cards
import Numeric.Probability.Game.Event

-- | A monad for describing drawing cards.
--
-- The first parameter is the type of the card (this must match the deck you end
-- up drawing from), the second parameter is the monadic return type as normal.
--
-- Each action in the monad is the drawing of a card, see 'draw' and similar functions.
--  There is the notion of failure: 'badHand' makes the current draw fail, as does
-- 'drawWhere' if no cards satisfy the criteria, and attempting to 'draw' when
-- there are no more cards will also fail.
--
-- The 'Alternative' instance allows you to choose between two sequences of draws.
--  If the LHS of '(\<|\>)' fails, the right-hand side is used instead.  'A.empty'
-- is the same as 'badHand'.
data DrawM card a = DrawFail | DrawOne (card -> DrawM card a) | Done a | DrawAny (DrawM card a)

instance Monad (DrawM card) where
  return x = Done x
  (>>=) DrawFail _ = DrawFail
  (>>=) (Done x) k = k x
  (>>=) (DrawAny m) k = DrawAny (m >>= k)
  (>>=) (DrawOne f) k = DrawOne (\x -> f x >>= k)
  fail _ = DrawFail

instance Applicative (DrawM card) where
  pure = Done
  (<*>) = ap

instance Functor (DrawM card) where
  fmap = liftM

instance Alternative (DrawM card) where
  empty = DrawFail
  (<|>) DrawFail x = x
  (<|>) (Done x) _ = Done x
  (<|>) (DrawAny m) (DrawAny n) = DrawAny $ m <|> n
  (<|>) (DrawOne f) (DrawOne g) = DrawOne $ \x -> f x <|> g x
  (<|>) (DrawAny m) (DrawOne g) = DrawOne $ \x -> m <|> g x
  (<|>) (DrawOne f) (DrawAny n) = DrawOne $ \x -> f x <|> n
  -- Done or DrawFail on RHS:
  (<|>) (DrawAny m) n = DrawAny $ m <|> n
  (<|>) (DrawOne f) n = DrawOne $ \x -> f x <|> n

-- | Tries to perform the two draws interleaved with each other in any sequence,
-- favouring those where the left-hand side acts first.
--
-- As an example:
--
-- > interleave (replicateM 2 (drawWhere (== "a"))) (replicateM 3 (drawWhere (== "b")))
--
-- will attempt to draw two \"a\" cards and three \"b\" cards, in any order and return them
-- as a pair.  If you want to draw identical groupings like this where the exact
-- values of the cards can vary, look at 'drawGroups'.
interleave :: DrawM card a -> DrawM card b -> DrawM card (a, b)
interleave DrawFail _ = DrawFail
interleave _ DrawFail = DrawFail
interleave (Done x) n = do y <- n
                           return (x, y)
interleave m (Done y) = do x <- m
                           return (x, y)
interleave (DrawAny m) (DrawAny n)
  = DrawAny $ interleave m (DrawAny n) <|> interleave (DrawAny m) n
interleave (DrawAny m) (DrawOne g)
  = DrawOne $ \x -> interleave m (DrawOne g) <|> interleave (DrawAny m) (g x)
interleave (DrawOne f) (DrawAny n)
  = DrawOne $ \x -> interleave (f x) (DrawAny n) <|> interleave (DrawOne f) n
interleave (DrawOne f) (DrawOne g)
  = DrawOne $ \x -> interleave (f x) (DrawOne g) <|> interleave (DrawOne f) (g x)

-- | Draws a single card and returns it.
--
-- If you are not interested in the value of the returned card, 'drawAny' is much
-- more efficient.  If you want to constrain which card might be drawn, use 'drawWhere'.
draw :: DrawM card card
draw = DrawOne Done

-- | Draws any card from the deck. In cases where you are not interested in what
-- the card is, this is much more efficient than 'draw'.
drawAny :: DrawM card ()
drawAny = DrawAny (Done ())

-- | Draws a single card that matches the given criteria (i.e. where the given
-- function returns True for the card).
--
-- For example:
--
-- > drawWhere (/= "c")
--
-- will draw any card that is not @\"c\"@.  Note that:
--
-- > (draw >>= ensure f) == (drawWhere f >> return ())
drawWhere :: (card -> Bool) -> DrawM card card
drawWhere f = DrawOne (\x -> if f x then Done x else DrawFail)

-- | Draws the given number of cards and then counts how many meet the given criteria.
--  The definition is:
--
-- > drawCount f n = length . filter f <$> replicateM n draw
--
-- Note that this is definitely /NOT/ the same as @replicateM n (drawWhere f)@.
--  The @drawWhere@ code makes sure that it draws n cards that meet the given criteria
-- (and fails in other cases), whereas this function draws the given number then
-- checks how many meet the criteria.  Therefore this function will only fail if
-- there are insufficient cards to draw that many.
drawCount :: (card -> Bool) -> Int -> DrawM card Int
drawCount f n = length . filter f <$> replicateM n draw

-- | Draws cards until it draws a card that satisfies the given condition or it
-- hits the optional limit of cards.  If the limit is zero, the function will fail
-- every time, 1 will only draw a single card, 2 will draw up to 2 and so on.
--
-- All the cards drawn will be returned in order, therefore you can be guaranteed
-- that the last card in the list (and only that card) satisfies the given function.
drawUntil :: (card -> Bool) -> Maybe Int -> DrawM card [card]
drawUntil f Nothing = DrawOne (\x -> if f x then Done [x] else (x:) <$> drawUntil f Nothing)
drawUntil f (Just n)
  | n < 1 = DrawFail
  | otherwise = DrawOne (\x -> if f x then Done [x] else (x:) <$> drawUntil f (Just (n-1)))

-- | Draws the given number of identical cards from the deck.
--
-- This corresponds to drawing one card from the deck with 'draw' and then using 'drawWhere'
-- to make sure the rest of the cards match.  The card that was drawn is returned
-- (since all of them are identical, only a single example is returned rather than
-- a list).
drawSame :: Eq card => Int -> DrawM card card
drawSame n = head <$> drawSameOn id n

-- | Draws the given number of identical (by the given aspect) cards from the deck.
--
-- This corresponds to drawing one card from the deck with 'draw' and then using 'drawWhere'
-- with the given mapping function to make sure the rest of the cards match on
-- the aspect specified.  The card that was drawn is returned
-- (since all of them are identical, only a single example is returned rather than
-- a list).  The order of the returned list is arbitrary.
--
-- For example:
--
-- > drawSameOn (map toLower) 5
--
-- will draw 5 cards (where the card type is simply String) that have matching
-- names when compared case-insensitive.  The return list you get might be something
-- like @[\"a\",\"A\",\"A\",\"a\",\"a\"]@.
drawSameOn :: Eq aspect => (card -> aspect) -> Int -> DrawM card [card]
drawSameOn f n | n < 1 = DrawFail
               | otherwise = do c <- draw
                                cs <- replicateM (n-1) $ drawWhere ((== f c) . f)
                                return (c : cs)

-- | Draws cards in groups of identical cards (but in any order) according to the given sizes.
--
-- This function is best explained by example:
--
-- * @drawGroups [2]@ will draw two identical cards, much as @drawSame 2@ does.
--
-- * @drawGroups [2,1]@ will draw two identical cards, and a third card that is
-- guaranteed not to be equal to the two identical cards.
--
-- * @drawGroups [2,2]@ will draw two different lots of two identical cards (i.e.
-- it cannot return 4 of the same card).
--
-- It is perhaps helpful to think of this function in terms of poker hands.  @drawGroups
-- [4,1]@ looks for 4-of-a-kind in a hand of 5, @drawGroups [3,2]@ looks for a
-- full house, @drawGroups [2,2,1]@ looks for two-pair, while @drawGroups [2,1,1,1]@
-- looks for exactly one pair.
--
-- The order of groups requested corresponds to the returns.  Thus, for example,
-- this code should never fail a pattern match:
--
-- > do [[a1,a2], [b1,b2,b3]] <- drawGroups [2,3]
--
-- The groups have no correspondence to the order in which the cards were drawn.
--  So although the groups above and returned together, those 5 cards could have
-- been drawn in any order, for example: @[b2, a1, b3, b2, a2]@.  This function is intended
-- for cases when you want particular identical groups but don't mind about the
-- order.  That is surprisingly fiddly to write without this helper function.
drawGroups :: Ord card => [Int] -> DrawM card [[card]]
drawGroups = drawGroupsOn id

-- Picks items from the list that match the given size.
--
-- If this isn't possible, the function will give an error
pick :: forall card. [Int] -> [[card]] -> [[card]]
pick ns groups = fst $ foldr pick' ([], fromList $ map (length &&& return) groups) ns
  where
    pick' :: Int -> ([[card]], Map Int [[card]]) -> ([[card]], Map Int [[card]])
    pick' n (r, m) = ((maybe (error "Internal error in drawGroupsOn") head $
      M.lookup n m) : r, update tailOrRemove n m)

    tailOrRemove [] = Nothing
    tailOrRemove (_:xs) = Just xs

-- | Like 'drawGroups', but considers them equal if their given aspect is equal.
drawGroupsOn :: forall card aspect. (Ord aspect) => (card -> aspect) -> [Int] -> DrawM card [[card]]
drawGroupsOn _ [] = return []
drawGroupsOn f ns
  | any (<= 0) ns = DrawFail
  | otherwise = pick ns . elems <$> drawGroupsOn' M.empty
  where
    sortedns :: [Int]
    sortedns = sortBy (flip compare) ns

    end = replicate (length ns) EQ

    drawGroupsOn' :: Map aspect [card] -> DrawM card (Map aspect [card])
    drawGroupsOn' m
      | comparison == end = return m
--      | any (== GT) comparison = badHand
      -- All are <= their intended result
      | otherwise = do c <- if all (== freeNewSlot) (elems space)
                              then draw
                              else drawWhere (\k -> fromMaybe freeNewSlot $ M.lookup (f k) space)
                       drawGroupsOn' $ insertWith (++) (f c) [c] m
      where
        -- A group has room to grow if the current number in the group is strictly
        -- less than the head of the targets (which has the highest target value)
        space :: Map aspect Bool
        space = fromList $ concat
          [ zip (map fst groupedKeysAndCounts) (repeat $ uncurry (<) $ snd $ head groupedKeysAndCounts)
          | groupedKeysAndCounts <- groupBy ((==) `on` (fst . snd)) itemsWithTarget]

        freeNewSlot = length sortedns > size m

        comparison :: [Ordering]
        comparison = map (uncurry compare . snd) itemsWithTarget

        itemsWithTarget :: [(aspect, (Int, Int))]
        itemsWithTarget = zipWith (\(x,y) z -> (x,(y,z)))
          (sortBy (flip compare `on` snd) (map (second length) $ toList m))
          sortedns

-- | Indicates that the current draw should not be continued.
badHand :: DrawM card a
badHand = DrawFail

-- | Checks that the given property holds, otherwise fails the current draw.  Its
-- definition is simple:
--
-- > ensure b = if b then return () else badHand
ensure :: Bool -> DrawM a ()
ensure True = return ()
ensure False = badHand

-- Map is depth to count, starting at zero depth
chance' :: Ord a => Int -> Cards a -> DrawM a z -> Map Int Integer
chance' n cards (Done {})
  | cardCount cards >= n = singleton 0 1
  | otherwise = M.empty
chance' _ _ DrawFail = M.empty
chance' n deck (DrawAny m) = chance' (n+1) deck m
chance' n deck (DrawOne f) = F.foldr ((/=) . (> 0)) True r `seq` r
  where
    r = mapKeysMonotonic (+1) $ unionsWith (+) [
      if firstCount == 0 then M.empty
      else if firstCount == 1 then chance' n (removeOneCard firstCard deck) (f firstCard)
      else fmap (toInteger firstCount*) $ chance' n (removeOneCard firstCard deck) (f firstCard)
      | (firstCard, firstCount) <- toList $ cardsMap deck]

-- | Calculates the chance of the given draw succeeding (i.e. not failing) with
-- the given deck.  Note that the return value of the draw is ignored; this function
-- is only interested in whether the draw succeeds.
--
-- Note that if you are only interested in partial aspects of the cards (e.g. just
-- the rank in a deck of playing cards), 'chanceOn' is much more efficient.  See
-- 'chanceOn' for more details.
--
-- Examples:
--
-- > chance deck (return ()) == 1
-- > chance (makeCards ["a", "a", "b"]) (drawWhere (== "a")) == 2 % 3
-- > chance (makeCards ["a", "a", "b"]) (drawSame 2) == 1 % 3
chance :: Ord card => Cards card -> DrawM card a -> Rational
chance deck m = F.sum (mapWithKey (\k v -> (permutes !! k) * v) depthToCount) % head permutes
  where
    depthToCount = chance' 0 deck m
    maxDepth = maximum (0 : keys depthToCount)
    deckSize = cardCount deck

    -- A lookup for (deckSize `permute`) . (maxDepth -)
    permutes | maxDepth == 0 = [1, 1]
             | otherwise = reverse $ 1 : scanl1 (*) [toInteger (deckSize - maxDepth + 1) .. toInteger deckSize]


-- | Calculates the chance of the given draw succeeding (i.e. not failing) with
-- the given deck.  Note that the return value of the draw is ignored; this function
-- is only interested in whether the draw succeeds.
--
-- The given function is used to transform the cards for drawing.  This can make
-- the function much more efficient if the transform maps several cards onto the
-- same aspect.  Consider if you wanted the probability of
-- drawing two aces from a deck of playing cards.  If you use 'chance', it will
-- check all 52 distinct cards in the deck separately to see if they are aces when you are
-- drawing.  However if you use @chanceOn rank@, it can collapse the 52 playing
-- cards into 13 distinct cards (one per rank) with frequency 4, and only check
-- each of the 13 cards separately.  Since this saving is made across repeated
-- draws, using 'chanceOn' rather than 'chance' can reduce queries from taking
-- many seconds into being instant.  This also applies to all the other chance..On and
-- event..On variants of functions in this module.
--
-- Examples:
--
-- > chanceOn id deck m == chance deck m
-- > chanceOn (map toLower) (makeCards ["a", "a", "A", "A", "b"]) (drawWhere (== "a")) == 4 % 5
chanceOn :: (Ord aspect) => (card -> aspect) -> Cards card -> DrawM aspect a -> Rational
chanceOn f = chance . mapCards f

-- | Turns the successful outcomes of the given draw into an 'EventM' type, which will return
-- the different values of the successful draw with their corresponding relative probabilities.  Note
-- that only successful draws are included; a failed draw will have a probability
-- of zero.  To incorporate the possibility of a failed draw, use 'eventDrawMaybe'
-- instead.
--
-- As with other functions, 'eventDrawOn' can be much more efficient; see 'chanceOn'
-- for details.
--
-- For example:
--
-- > outcomes (eventDraw (makeCards ["a","b"]) (drawWhere (== "a"))) == [("a", 1)]
-- 
-- > outcomes (eventDraw (makeCards ["a","a","a","b","b"]) (drawSame 2)
-- >   == [("a", 3 % 5), ("b", 2 % 5)]
eventDraw :: (Ord a, Ord card) => Cards card -> DrawM card a -> EventM a
eventDraw c d = makeEventProb $ toList $ chanceMap c d

-- | Like 'eventDraw' but can be much more efficient.  See 'chanceOn' for an
-- explanation of why.
eventDrawOn :: (Ord a, Ord aspect) => (card -> aspect) -> Cards card -> DrawM aspect a -> EventM a
eventDrawOn f c d = makeEventProb $ toList $ chanceMapOn f c d

-- | Turns the outcomes of the given draw into an 'EventM' type, which will return
-- the different values of the draw with their corresponding probabilities.  Successful
-- draws are the Just values; Nothing indicates an unsuccessful draw, with its
-- corresponding probability.
--
-- As with other functions, 'eventDrawMaybeOn' can be much more efficient; see 'chanceOn'
-- for details.
--
-- For example:
--
-- > outcomes (eventDraw (makeCards ["a","b"]) (drawWhere (== "a"))) == [(Just "a", 1 % 2), (Nothing, 1 % 2)]
-- 
-- > outcomes (eventDraw (makeCards ["a","a","a","b","b"]) (drawSame 2)
-- >   == [(Just "a", 3 % 10), (Just "b", 1 % 5), (Nothing, 1 % 2)]
--
-- > eventDrawMaybe cards m == eventDraw cards (optional m)
eventDrawMaybe :: (Ord a, Ord card) => Cards card -> DrawM card a -> EventM (Maybe a)
eventDrawMaybe c d = makeEventProb $ (Nothing, q) : map (first Just) (toList m)
  where
    m = chanceMap c d
    q = 1 - F.sum m

-- | Like 'eventDrawMaybe' but can be much more efficient.  See 'chanceOn' for an
-- explanation of why.
eventDrawMaybeOn :: (Ord a, Ord aspect) => (card -> aspect) -> Cards card -> DrawM aspect a -> EventM (Maybe a)
eventDrawMaybeOn f c d  = makeEventProb $ (Nothing, q) : map (first Just) (toList m)
  where
    m = chanceMapOn f c d
    q = 1 - F.sum m

-- | Like 'chanceMap' but can be much more efficient.  See 'chanceOn' for an
-- explanation of why.
chanceMapOn :: (Ord a, Ord aspect) => (card -> aspect) -> Cards card -> DrawM aspect a -> Map a Rational
chanceMapOn f = chanceMap . mapCards f

-- | Calculates the probability of each result of the given draw with the given
-- deck.  The probabilities will exclude the chance of a failed draw; therefore
-- the chance of a failed draw is @1 - sum (elems $ chanceMap ..)@.  Alternatively
-- you can incorporate the possibility of a failed draw with a Maybe wrapper using
-- @chanceMap cards (optional m)@.
--
-- Examples:
--
-- > chanceMap (makeCards ["a","b"]) (drawWhere (== "a"))) == singleton "a" (1 % 2)
--
-- > outcomes (eventDraw (makeCards ["a","a","a","b","b"]) (drawSame 2)
-- >   == fromList [("a", 3 % 10), ("b", 1 % 5)]
chanceMap :: (Ord card, Ord a) => Cards card -> DrawM card a -> Map a Rational
chanceMap deck m = fmap (% head permutes) $ unionsWith (+) $ elems $
  mapWithKey (\k v -> fmap ((permutes !! k) *) v) depthToCount
  where
    depthToCount = chanceMap' 0 deck m
    maxDepth = maximum (0 : keys depthToCount)
    deckSize = cardCount deck

    permutes :: [Integer]
    -- A lookup for (deckSize `permute`) . (maxDepth -)
    permutes | maxDepth == 0 = [1, 1]
             | otherwise = reverse $ 1 : scanl1 (*) [toInteger (deckSize - maxDepth + 1) .. toInteger deckSize]


-- Map is depth to (return value to count), starting at zero depth
chanceMap' :: (Ord a, Ord b) => Int -> Cards a -> DrawM a b -> Map Int (Map b Integer)
chanceMap' n cards (Done x)
  | cardCount cards >= n = singleton 0 (singleton x 1)
  | otherwise = M.empty
chanceMap' _ _ DrawFail = M.empty
chanceMap' n deck (DrawAny m) = chanceMap' (n+1) deck m
chanceMap' n deck (DrawOne f)
  = mapKeysMonotonic (+1) $ unionsWith (unionWith (+)) [
      if firstCount == 0 then M.empty
      else if firstCount == 1 then chanceMap' n (removeOneCard firstCard deck) (f firstCard)
      else fmap (fmap (toInteger firstCount*)) $ chanceMap' n (removeOneCard firstCard deck) (f firstCard)
      | (firstCard, firstCount) <- toList $ cardsMap deck]
