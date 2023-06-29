data Deck = Deck { cards :: [Card] }
data Card = Card { suit :: Suit, rank :: Rank }
data Suit = Clubs | Diamonds | Hearts | Spades
data Rank =  Number Int | Jack | Queen | King | Ace

deriving instance Show Card
deriving instance Show Suit
deriving instance Show Rank