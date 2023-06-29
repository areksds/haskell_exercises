data Deck = Deck [Card]
data Card = Card { suit :: Suit, rank :: Rank }
data Suit = Clubs | Diamonds | Hearts | Spades
data Rank =  Number Int | Jack | Queen | King | Ace

deriving instance Show Card
deriving instance Show Suit
deriving instance Show Rank

-- Does it make more sense to have Bool, or another type?
-- Idea: data AceHigh = True | False

cardVal :: Card -> Bool -> Int
cardVal (Card _ (Number n)) _ = n
cardVal (Card _ Jack) _ = 11
cardVal (Card _ Queen) _ = 12
cardVal (Card _ King) _ = 13
cardVal (Card _ Ace) True = 14
cardVal (Card _ Ace) False = 1

highCard :: Deck -> Bool -> Maybe Card
highCard (Deck []) _ = Nothing
highCard (Deck [x]) _ = Just x
highCard (Deck (x:xs)) aceHigh
    | cardVal x aceHigh > cardVal (highCard (Deck xs) aceHigh) aceHigh = Just x
    | otherwise = highCard (Deck xs) aceHigh