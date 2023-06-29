import Data.List (sort, sortOn)
import Data.Maybe (fromJust)

newtype Deck = Deck [Card] deriving (Show)
newtype Hand = Hand [Card] deriving (Show)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Show, Eq)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Enum, Bounded)
data Rank =  Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum, Bounded)

generateDeck :: Deck
generateDeck = Deck [Card value suit | value <- [minBound ..], suit <- [minBound ..]]

-- Having trouble importing System.Random
-- randomizeDeck :: Deck -> Deck
-- randomizeDeck deck = map fst $ sortOn snd $ zip deck (randoms gen)

-- Takes a Deck and puts five cards in one hand, returns Hand and remaining Deck
-- handAndNewDeck = fromJust (generateHand generateDeck)
-- newHand = fst handAndNewDeck
-- newDeck = snd handAndNewDeck
generateHand :: Deck -> Maybe (Hand, Deck)
-- Note: is there a better way to handle the rest of the cases?
generateHand (Deck []) = Nothing
generateHand (Deck [x]) = Nothing
generateHand (Deck [x,y]) = Nothing
generateHand (Deck [x,y,z]) = Nothing
generateHand (Deck [x,y,z,t]) = Nothing
generateHand (Deck deck) = Just (Hand (take 5 deck), Deck (drop 5 deck))

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
isRoyalFlush (Hand cards) = isStraightFlush (Hand cards) && (rank (lowCard (Hand cards)) == Ten) && (rank (highCard (Hand cards)) == Ace)
isTwoPair (Hand cards) = isTwoPairRanks (sortedRanks (Hand cards))
isPair (Hand cards) = isPairRanks (sortedRanks (Hand cards))

isTwoPairRanks :: [Rank] -> Bool
isTwoPairRanks [] = False
isTwoPairRanks [x] = False
isTwoPairRanks (x:y:xs) = 
    if x == y
    then
        isPairRanks xs
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
sortRanks rank = sort rank

consecutiveRanks :: [Rank] -> Bool
consecutiveRanks [] = True
consecutiveRanks [x] = True
consecutiveRanks (x:y:xs) = (succ x == y) && consecutiveRanks (y:xs)

lowCard :: Hand -> Card
lowCard (Hand []) = error "Empty deck"
lowCard (Hand [x]) = x
lowCard (Hand (x:xs))
    | rank x < rank (lowCard (Hand xs)) = x
    | otherwise = lowCard (Hand xs)

highCard :: Hand -> Card
highCard (Hand []) = error "Empty deck"
highCard (Hand [x]) = x
highCard (Hand (x:xs))
    | rank x > rank (highCard (Hand xs)) = x
    | otherwise = highCard (Hand xs)