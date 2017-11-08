{-
Lab 2 - Vaios Taxiarchis , Sarkhan Ibayev
-}
module BlackJack where
import Cards
import RunGame

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
