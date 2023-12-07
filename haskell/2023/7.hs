import Util
import Data.List
import Data.Ord

main = do
  pairs <- map parsePair . lines <$> readFile "7.txt"
  print pairs
  let sorted = sortBy (comparing (judgeHand . fst)) pairs
  mapM_ print sorted
  let ranks = zip [1..] sorted
  mapM_ print ranks
  let winnings = map (\(r,(h,b)) -> r*b) ranks
  print $ sum winnings

data Card = Card Int
  deriving (Show, Eq, Ord)
data Hand = Hand [Card]
  deriving (Eq, Ord)

instance Show Hand where
    show (Hand cards) = concatMap showCard cards

showCard (Card 14) = "A"
showCard (Card 13) = "K"
showCard (Card 12) = "Q"
showCard (Card 1) = "J"
showCard (Card 10) = "T"
showCard (Card x) = show x

charToCard :: Char -> Card
charToCard 'A' = Card 14
charToCard 'K' = Card 13
charToCard 'Q' = Card 12
charToCard 'J' = Card 1
charToCard 'T' = Card 10
charToCard c = Card (read [c])


parsePair :: String -> (Hand, Integer)
parsePair l = let [a,b] = words l
              in (readHand a, read b)

readHand = Hand . map charToCard

judgeHand hand = Judgement (judgeHand' hand) hand

data Rung = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Show, Eq, Ord)

data Judgement = Judgement Rung Hand
  deriving (Show, Eq, Ord)

judgeHand' hand = maximum $ map judgeHand'' $ explodeHands hand

explodeHands (Hand cards) = map Hand (mapM f cards)
  where
    f (Card 1) = [Card 2, Card 3, Card 4, Card 5, Card 6, Card 7, Card 8, Card 9, Card 10, Card 12, Card 13, Card 14]
    f (Card x) = [Card x]

judgeHand'' :: Hand -> Rung
judgeHand'' (Hand cards) =
    let counts = map snd (count cards)
    in case sort counts of
         [5] -> FiveOfAKind
         [1,4] -> FourOfAKind
         [2,3] -> FullHouse
         [1,1,3] -> ThreeOfAKind
         [1,2,2] -> TwoPair
         [1,1,1,2] -> OnePair
         [1,1,1,1,1] -> HighCard
