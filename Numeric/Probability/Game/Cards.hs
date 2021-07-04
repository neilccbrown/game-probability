module Numeric.Probability.Game.Cards (
  -- * Cards datatype
  Cards,
  -- ** Accessing
  cardsMap, sortedCards, cardCount,
  -- ** Creating
  makeCards, makeCardsMap,
  -- ** Modifying
  addCard, removeArbitrary, removeOneCard, mapCards, minusCards,
  -- * Drawing Cards
  drawOne, drawReplace, drawNoReplace) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.Function (on)
import Data.Map as M (Map, update, toAscList, update, empty, filter, insert,
  toList, differenceWith, mapKeysWith, unionWith, insertWith, minViewWithKey)
import Data.Monoid (Monoid(..))
import qualified Data.Foldable as F (sum)

import Numeric.Probability.Game.Event (EventM, makeEventProb)

-- | A collection\/deck of cards.  The collection of cards has no implicit order,
-- and each card is deemed to be equally likely to be drawn.
--
-- So, for example, @makeCards [\"a\",\"a\",\"a\",\"b\",\"c\"]@ is a collection of cards
-- with a 3\/5 chance of drawing an @\"a\"@.
--
-- Note that in 'Cards' and all functions using it, the 'Ord' instance is considered
-- to be authoritative.  Imagine you have some type like:
--
-- > data MyCard = MyCard {cardType :: String, cardDescription :: String}
-- > instance Ord MyCard where compare = comparing cardType
--
-- If you then create a collection of cards, all those with the same @cardType@ will
-- be considered the same, and differences in @cardDescription@ will be collapsed.  So,
-- for example you may find that:
--
-- > cardsMap (makeCards [MyCard "Sword" "Long Sword", MyCard "Sword" "Legendary Sword of the Ancient King of Rak'Tharr", MyCard "Shield" "Buckler"])
-- >  == fromList [(MyCard "Sword" "Long Sword", 2), (MyCard "Shield" Buckler", 1)]
--
-- The two sword cards are indistinguishable from each other by the Ord
-- instance, so an arbitrary card for the two is kept in the collection to
-- represent them both -- the legendary sword is treated the same as the long
-- sword (so, equally, you might get two legendary swords in the deck and no
-- long sword).
--
-- If you want the difference to matter, use an 'Ord' instance that recognises
-- the difference.  If you want the difference to matter some of the time, and
-- not matter at other times, you may want to use 'mapCards' to either pick
-- out just the aspects you are interested in, or to use a default value
-- (e.g. empty description) for the aspects you are not interested in.
-- 
-- The 'Monoid' instance can be used to get an empty 'Cards' object, and to add
-- two collections of cards together.
data Cards a = Cards {
  -- | Gets a map from card to frequency for the given 'Cards' item.
  cardsMap :: Map a Int, cacheCardsSize :: Int }

instance Ord a => Eq (Cards a) where
  (==) = (==) `on` cardsMap

instance Ord a => Ord (Cards a) where
  compare = compare `on` cardsMap

instance Show a => Show (Cards a) where
  show = show . sortedCards

instance Ord a => Monoid (Cards a) where
  mempty = Cards M.empty 0
  mappend (Cards mx nx) (Cards my ny) = Cards (unionWith (+) mx my) (nx + ny)

-- | Makes a 'Cards' item using a 'Map' from card to frequency.  Any card with
-- a frequency of 0 or less will be ignored.
makeCardsMap :: Ord a => Map a Int -> Cards a
makeCardsMap m = Cards m' s
  where
    s = F.sum m'
    m' = M.filter (> 0) m

-- Used internally when you know there's no non-positive frequencies
makeCardsMap' :: Map a Int -> Cards a
makeCardsMap' m = Cards m s
  where
    s = F.sum m


-- | Gets a sorted list of cards.  For example:
--
-- > ["a","a","a","b","c","c"] == sortedCards (makeCardsMap (fromList [("c", 2), ("b", 1), ("a", 3)]))
sortedCards :: Cards a -> [a]
sortedCards = makeList . toAscList . cardsMap

-- | Removes the cards in the second parameter from the cards in the first parameter.
-- If the frequency of a card in the second parameter is greater than or equal
-- to the frequency of a card in the first parameter, all of them are removed.
--  Negative frequencies are not possible.
--
-- Example:
--
-- > makeCardsMap (fromList [("a", 3), ("b", 1), ("c", 2)]) `minusCards` (makeCards ["a","b","b","c"]) == makeCards ["a","a","c"]
minusCards :: Ord a => Cards a -> Cards a -> Cards a
minusCards c d = makeCardsMap' $ differenceWith maybeMinus (cardsMap c) (cardsMap d)

-- Nothing if the result would be 0 or less
maybeMinus :: Int -> Int -> Maybe Int
maybeMinus x y | x <= y = Nothing
               | otherwise = Just (x - y)

-- | Applies a function to the cards.  Like 'fmap' for 'Cards', but we can't use
-- 'Functor' because of the 'Ord' constraint.
--
-- If this function maps two old cards to the same single new card, their frequencies
-- are added together, but otherwise the frequencies are left untouched.
--
-- This function is particularly useful for narrowing the number of distinct cards;
-- see functions in the "Numeric.Probability.Game.Cards.Hand" module.
--
-- Example:
--
-- > mapCards (map toUpper) (makeCardsMap (fromList [("a", 2), ("A", 3), ("b", 2)])) == makeCardsMap (fromList [("A", 5), ("B", 2)])
mapCards :: (Ord b) => (a -> b) -> Cards a -> Cards b
mapCards f (Cards m s) = Cards (mapKeysWith (+) f m) s

--makeCardsAsc :: Eq a => [a] -> Cards a
--makeCardsAsc = makeCardsMap . fromAscList . map (head &&& length) . group

-- | Makes a collection of cards from the given list.  The order of the list does
-- not matter, but duplicates are important: if a card occurs multiple times in
-- the list, it will appear multiple times in the collection.  So @makeCards ["a","b"]@
-- has one card named \"a\" and one named \"b\", but @makeCards ["a","a","b","a"]@
-- has three cards named \"a\" and one named \"b\".
makeCards :: Ord a => [a] -> Cards a
makeCards = makeCardsMap' . foldr add M.empty
  where
    add k m = insertWith (+) k 1 m

-- | Removes one of the given cards from the collection.  This only reduces the
-- frequency by one; it does not remove all of the given card from the collection.
--  If the card is not in the collection, this has no effect.
--
-- Example:
--
-- > removeOneCard "a" (makeCards ["a","a","a","b"]) == makeCards ["a","a","b"]
-- > removeOneCard "c" (makeCards ["a","a","a","b"]) == makeCards ["a","a","a","b"]
removeOneCard :: Ord a => a -> Cards a -> Cards a
removeOneCard x (Cards m s) = Cards (update (`maybeMinus` 1) x m) (s - 1)

-- | Removes a given number of cards that match the given criteria.
--
-- As the name suggests, the choice of cards removed is arbitrary.  This function
-- is mainly useful if you later want to check for the odds of finding a card that /does/
-- match the given criteria, but first want to express that you know of many cards
-- that don't meet the criteria that aren't in the deck.
--
-- If not enough cards meet the criteria in the collection, all that don't
-- meet the criteria will be removed.
removeArbitrary :: Ord a => (a -> Bool) -> Int -> Cards a -> Cards a
removeArbitrary f total = makeCardsMap . remove total . cardsMap
  where
    remove n m = case minViewWithKey m of
      Just ((k, v), m') -> if f k
                             then case compare v n of
                                    LT -> remove (n-v) m'
                                    EQ -> m'
                                    GT -> insert k (v-n) m'
                             else insert k v $ remove n m'
      Nothing -> M.empty

-- | Adds the given card and frequency to the collection of cards.  If the card
-- is already in the collection, the frequencies are added.
--
-- Example:
--
-- > addCard ("c", 2) (makeCards ["a","a","b"]) == makeCards ["a","a","b","c","c"]
-- > addCard ("b", 1) (makeCards ["a","a","b"]) == makeCards ["a","a","b","b"]
addCard :: Ord a => (a, Int) -> Cards a -> Cards a
addCard (c, n) (Cards m s) = Cards (insertWith (+) c n m) (s + n)

makeList :: [(a, Int)] -> [a]
makeList [] = []
makeList ((x,n):xs)
  | n <= 0 = makeList xs
  | otherwise = x : makeList ((x, n-1):xs)

-- | Draws one card from the given collection of cards at random.  Returns the
-- card, and the collection of cards after the card has been drawn (i.e. with one
-- of that card removed).  If the collection is empty, the result is undefined.
--
-- Note that using this function repeatedly to draw a hand of cards can be quite
-- computationally intensive; for more efficient methods, see the "Numeric.Probability.Game.Cards.Hand"
-- module.
--
-- Example:
--
-- > outcomes (drawOne (makeCards ["a","a","a","b"])) == [(("a", makeCards ["a","a","b"]), 3 % 4), (("b", makeCards ["a","a","a"]), 1 % 4)]
drawOne :: Ord a => Cards a -> EventM (a, Cards a)
drawOne cards = makeEventProb [((c, removeOneCard c cards), n) | (c, n) <- toList $ cardsMap cards]

-- TODO could probably be a bit cleverer about calculating drawReplace and drawNoReplace

-- | Draws the given number of cards from the given deck of cards at random,
-- without replacement.  Returns the collection of cards that were drawn (the
-- first part of the result pait), and the corresponding remaining deck of
-- cards.  If the deck is empty or does not contain enough cards to draw the
-- specified number, the result is undefined.
--
-- Note that using this function to draw a hand of cards can be quite
-- computationally intensive; for more efficient methods, see the "Numeric.Probability.Game.Cards.Hand"
-- module.
--
-- Note that @makeCards n cards == swap \<$\> makeCards (cardCount cards - n) cards@;
-- this method will be much more efficient with a smaller number as parameter than
-- a larger number. 
-- 
-- Example:
--
-- > outcomes (drawNoReplace 2 (makeCards ["a","a","a","a", "b"])) ==
-- >   [((makeCards ["a","a"], makeCards ["a","a", "b"]), 3 % 5), ((makeCards ["a","b"], makeCards ["a,"a","a"]), 2 % 5)]
drawNoReplace :: Ord a => Int -> Cards a -> EventM (Cards a, Cards a)
drawNoReplace n cards
  | n <= 0 = return (mempty, cards)
  | otherwise = do (c, rest) <- drawOne cards
                   first (addCard (c, 1)) <$> drawNoReplace (n-1) rest

-- | Draws the given number of cards from the given collection with replacement.
--  Returns the collection of cards that will be drawn (and thus you can be sure
-- that: @cardsCount \<$\> drawReplace n cards@ will be @n@, provided @cards@ is not
-- empty).  If the given collection of cards is empty, the result is undefined.
--
-- Note that using this function to draw a hand of cards can be quite
-- computationally intensive; for more efficient methods, see the "Numeric.Probability.Game.Cards.Hand"
-- module.
--
-- Example:
--
-- > outcomes (drawReplace 2 (makeCards ["a","a","a","b"])) ==
-- >   [(makeCards ["a","a"], 9 % 16), (makeCards ["a","b"], 3 % 8), (makeCards ["b","b"], 1 % 16)]
drawReplace :: Ord a => Int -> Cards a -> EventM (Cards a)
drawReplace n cards = makeCards . map fst <$> replicateM n (drawOne cards)

-- | Counts the number of cards (i.e. the sum of the frequencies of each distinct
-- card) in the collection.  @cardCount cards == length (sortedCards count)@
--
-- If you want the number of distinct cards in a collection, use @size . cardsMap@.
cardCount :: Cards a -> Int
cardCount = cacheCardsSize

