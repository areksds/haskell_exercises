import Data.List (sort, sortOn, foldl')
import Data.Maybe (fromJust)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Coerce

newtype Deck = Deck [Card] deriving (Show)
newtype Hand = Hand [Card] deriving (Show)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Show, Eq)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Enum, Bounded)
data Rank =  Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum, Bounded)

generateDeck :: Deck
generateDeck = Deck [Card suit value | suit <- [minBound ..], value <- [minBound ..]]

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
        splitAt5 = splitAt 5 deck
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

isFlush (Hand cards) = all (isEqSuit (head cards)) cards
isStraight (Hand cards) = consecutiveRanks (sortedRanks (Hand cards))
isStraightFlush (Hand cards) = isFlush (Hand cards) && isStraight (Hand cards)
isRoyalFlush :: Hand -> Bool
isRoyalFlush hand = isStraightFlush hand && isRoyal (coerce hand)
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
getRanks (Hand cards) = map rank cards

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