-- | A module containing some simple data definitions for a deck of playing cards.
module Numeric.Probability.Game.PlayingCards (PlayingCard(..),Suit(..),Rank(..),AceLowRank(..),deck,
  sameSuit, sameRank) where

import Data.Function (on)

import Numeric.Probability.Game.Cards

-- | The rank of playing cards.  The ranking is specified ace-high, as this is
-- how many games operate.  If you wish to have an ace-low ordering you can use
-- the 'AceLowRank' newtype.
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack
  | Queen | King | Ace
  deriving (Bounded, Enum, Eq, Ord, Show, Read)

-- | A wrapper for 'Rank' where the Ord, Enum and Bounded instances are adjusted
-- to list Ace as the lowest item rather than the highest.
newtype AceLowRank = AceLow {alRank :: Rank}
  deriving (Eq, Show, Read)

instance Bounded AceLowRank where
  minBound = AceLow Ace
  maxBound = AceLow King

instance Enum AceLowRank where
  fromEnum (AceLow Ace) = 0
  fromEnum (AceLow x) = fromEnum x + 1
  toEnum 0 = AceLow Ace
  toEnum n = AceLow $ toEnum (n - 1)

instance Ord AceLowRank where
  compare = compare `on` fromEnum

-- | The standard four suits of playing cards.  The ordering on them is arbitrary
-- (alphabetical, in fact).
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Show, Read)

-- | A playing card with a rank and suit.  The ordering on them is arbitrary (by
-- rank then by suit).
data PlayingCard = PlayingCard {rank :: Rank, suit :: Suit}
  deriving (Eq, Ord, Show, Read)

-- | The standard full deck of 52 playing cards.
deck :: Cards PlayingCard
deck = makeCards [PlayingCard r s | r <- [minBound .. maxBound], s <- [Clubs,Diamonds,Hearts,Spades]]

-- | Checks if the two cards have the same suit.
sameSuit :: PlayingCard -> PlayingCard -> Bool
sameSuit = (==) `on` suit

-- | Checks if the two cards have the same rank.
sameRank :: PlayingCard -> PlayingCard -> Bool
sameRank = (==) `on` rank
