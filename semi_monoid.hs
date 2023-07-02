import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (sort, sortOn, foldl', group)
import Data.Maybe (fromJust)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Coerce
import Data.Monoid
import Data.Semigroup

newtype Deck = Deck (Set.Set Card) deriving (Show)
newtype Hand = Hand (Set.Set Card) deriving (Show)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Show, Eq, Ord)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Enum, Bounded, Ord)
data Rank =  Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum, Bounded)

instance Semigroup Deck where
    (<>) = (<>)

instance Semigroup Hand where
    (<>) = (<>)

instance Monoid Deck where
    mempty = Deck Set.empty

instance Monoid Hand where
    mempty = Hand Set.empty

generateDeck :: Deck
generateDeck = Deck $ Set.fromList [Card suit value | suit <- [minBound ..], value <- [minBound ..]]

-- Having trouble importing System.Random
-- randomizeDeck :: Deck -> Deck
-- randomizeDeck deck = map fst $ sortOn snd $ zip deck (randoms gen)

-- Takes a Deck and puts five cards in one hand, returns Hand and remaining Deck
-- handAndNewDeck = generateHand generateDeck
-- newHand = fst handAndNewDeck
-- newDeck = snd handAndNewDeck
generateHand :: Deck -> (Hand, Deck)
generateHand (Deck deck) = 
    let 
        splitAt5 = Set.splitAt 5 deck
        hand = fst splitAt5
        newDeck = snd splitAt5
    in (Hand hand, Deck newDeck)

class Display a where
    display :: a -> String

instance Display Rank where
    display = show

instance Display Suit where
    display = show

instance Display Card where
    display card = display (rank card) ++ " of " ++ display (suit card)

isFlush (Hand cards) = Set.size (Set.map suit cards) == 1
-- We map all the cards to their suits, then check if the size of the set is 1
isStraight (Hand cards) = consecutiveRanks (sortedRanks (Hand cards))
isStraightFlush (Hand cards) = isFlush (Hand cards) && isStraight (Hand cards)
isRoyalFlush :: Hand -> Bool
isRoyalFlush (Hand cards) = isStraightFlush (Hand cards) && isRoyal (Set.toList cards)
 where
  isRoyal [] = False
  isRoyal (x : xs) = case highCard (x :| xs) of
    Card _ Ace -> True
    _ -> False
isTwoPair (Hand cards) = isTwoPairRanks (sortedRanks (Hand cards))
isPair (Hand cards) = isPairRanks (sortedRanks (Hand cards))

isTwoPairRanks :: [Rank] -> Bool
isTwoPairRanks [] = False
isTwoPairRanks [x] = False
isTwoPairRanks (x:y:xs) = 
  if x == y
    then isPairRanks xs
    else isTwoPairRanks (y:xs)

isPairRanks :: [Rank] -> Bool
isPairRanks [] = False
isPairRanks [x] = False
isPairRanks (x:y:xs) = x == y || isPairRanks (y:xs)

isEqSuit :: Card -> Card -> Bool
isEqSuit (Card s1 _) (Card s2 _) = s1 == s2

sortedRanks :: Hand -> [Rank]
sortedRanks = sortRanks . getRanks

getRanks :: Hand -> [Rank]
getRanks (Hand cards) = Set.toList (Set.map rank cards)

sortRanks :: [Rank] -> [Rank]
sortRanks = sort

consecutiveRanks :: [Rank] -> Bool
consecutiveRanks [] = True
consecutiveRanks [x] = True
consecutiveRanks [x, y] = fromEnum x + 1 == fromEnum y
consecutiveRanks (x:y:xs) = fromEnum x + 1 == fromEnum y && consecutiveRanks (y:xs)

lowCard :: NonEmpty Card -> Card
lowCard (x :| xs) = foldl' (\x x' -> if rank x < rank x' then x else x') x xs

highCard :: NonEmpty Card -> Card
highCard (x :| xs) = foldl' (\x x' -> if rank x > rank x' then x else x') x xs

-- Goose's functions from missing lab
hasNAlike :: (Eq a, Ord a) => Int -> [a] -> Bool
hasNAlike n = any ((>= n) . length) . group . sort

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind = hasNAlike 3 . getRanks

isFourOfAKind :: Hand -> Bool
isFourOfAKind = hasNAlike 4 . getRanks

isFullHouse :: Hand -> Bool
isFullHouse (Hand cs) = groups == [2, 3]
 where
  groups = sort . map length . group . sort $ Set.toList (Set.map rank cs)