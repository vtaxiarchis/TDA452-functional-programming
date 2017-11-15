{-
Lab 2 - Vaios Taxiarchis , Sarkhan Ibayev
-}
module BlackJack where
import Cards
import RunGame

import System.Random
import Test.QuickCheck hiding (shuffle)

-- 3.2 Properties --
{-
size hand2  = size (Add (Card (Numeric 2) Hearts)
              (Add (Card Jack Spades) Empty))
            = 1 + 1 + 0 = 2
-}

-- 3.4 Functions --
-- Function to return an empty hand
empty :: Hand
empty = Empty

-- Function to calculate the value of a Rank
valueRank :: Rank -> Integer
valueRank (Numeric n) = n -- Numeric 2-9
valueRank Ace = 11 -- Ace (1)
valueRank _ = 10 -- Jack | Queen | King

-- Function to calculate the value of a Card
valueCard :: Card -> Integer
valueCard (Card rank _) = valueRank rank

-- Function to calculate the number of Aces in a given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand) = numberOfAces hand

-- Function to calculate the value of a current hand and Ace equals to 11 points
currentHand :: Hand -> Integer
currentHand Empty = 0
currentHand (Add card hand) = valueCard card + currentHand hand

-- Function to calculate the value of the hand
value :: Hand -> Integer
value hand  | totalScore > 21 = totalScore - (10 * totalAces)
            | otherwise = totalScore
            where totalScore = currentHand hand
                  totalAces = numberOfAces hand

-- Function to check if the player is busted
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Function to determine the winner
winner :: Hand -> Hand -> Player
winner guestHand bankHand | not (gameOver guestHand) && value guestHand > value bankHand = Guest
                          | not (gameOver guestHand) && gameOver bankHand = Guest
                          | otherwise = Bank

-- Operator to add one hand to another
(<+) :: Hand -> Hand -> Hand
(<+) hand Empty = hand
(<+) Empty hand = hand
(<+) (Add card rest) secondHand = Add card (rest <+ secondHand)

-- Property to check associativity of adding hands
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- Property to check size of adding hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = (size p1 + size p2) == size (p1 <+ p2)

allCardOfSuit :: Suit -> Hand
allCardOfSuit suit =
  let ranks = Ace:King:Queen:Jack:[Numeric x | x <- [2..10]]
      cards = [Card rank suit | rank <- ranks]
      adder [] = Empty
      adder (card:xs) = (Add card (adder xs))
  in adder cards

-- Function to return a full deck of cards
fullDeck :: Hand
fullDeck = (allCardOfSuit Hearts) <+ (allCardOfSuit Spades) <+
           (allCardOfSuit Diamonds) <+ (allCardOfSuit Clubs)

-- Property to check size of the full deck
prop_size_fullDeck :: Bool
prop_size_fullDeck = (size fullDeck) == 52

-- Function to draw a card from a deck
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add card rest) hand = (rest, Add card hand)

-- Function to play a hand for a bank: continue until 16 or higher is achieved
playBank :: Hand -> Hand
playBank deck = snd (playBank' deck Empty)

-- Helper function that starts with a hand and draws till 16 is achieved
playBank' :: Hand -> Hand -> (Hand, Hand)
playBank' deck bankHand | currentHand bankHand >= 16 = (deck, bankHand)
                        | otherwise                  = playBank' deck' bankHand'
          where (deck',bankHand') = draw deck bankHand


-- Function that picks the card in specified position starting with given initial number
chooseCard' :: Hand -> Int ->  Int -> (Card, Hand)
chooseCard' Empty _ _ = error "chooseCard': cant choose from empty."
chooseCard' (Add card rest) num choose | num == choose = (card, rest)
                                       | otherwise     = (chosen, (Add card rest'))
          where (chosen, rest') = chooseCard' rest (num + 1) choose


-- Function that given a randomly picks cards from first hand and adds them to second hand
shuffle' :: StdGen -> Hand -> Hand -> Hand
shuffle' _ Empty hand = hand
shuffle' g hand accum = shuffle' g' hand' (Add card accum)
          where handSize      = size hand
                (random, g')  = randomR (1::Int, handSize) g
                (card, hand') = chooseCard' hand 1 random

-- Function to shuffle given hand
shuffle :: StdGen -> Hand -> Hand
shuffle g hand = shuffle' g hand Empty


-- Function to check if a card belongs to a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Property to check card exists in shuffled hand
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

-- Property to check size of shuffled hands match
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = (size h) == (size (shuffle g h))


implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation

