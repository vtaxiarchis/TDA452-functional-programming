{-
Lab 2 - Vaios Taxiarchis
-}
module BlackJack where
import Cards
import RunGame

-- 3.1 - Recursive types -
{-
  Calculate "size hand2" by hand:
    size hand2  = Add (Card (Numeric 2) Hearts)
                    (Add (Card Jack Spades) Empty)
                = 1 + (Add (Card Jack Spades) Empty)
                = 1 + 1 + size Empty
                = 1 + 1 + 0
                = 2
-}
-- 3.2 - Properties -
-- 3.3 - Documentation -

-- 3.4 - Functions -

-- Function to return an empty hand
empty :: Hand
empty = Empty
