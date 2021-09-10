module Blackjack where
import Cards 
import RunGame
import Test.QuickCheck
import Data.String (String)
import GHC.Base (String, eqString)
import Data.List ( isInfixOf )
import GHC.Runtime.Heap.Layout (card)
import GHC.Prelude (String)
import System.Directory.Internal.Prelude (String)
-- Task A1

-- Steps in hand
{- size hand2
  = size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
  = 1 + size (Card Jack Spades : [])
  = 2 + size []
  = 2
-}

aCard1 :: Card
aCard1 = Card (Numeric 8) Hearts

aCard2 :: Card
aCard2 = Card Ace Spades

{-
aCard3 :: Card 
aCard3 = Card Ace Clubs 
-}

ahand :: Hand
ahand = (aCard1 : aCard2: [])

hand2 :: Hand
hand2 = (Card (Numeric 10) Hearts : (Card Jack Spades : (Card King Hearts : [])))


-- Prints all the steps in size hand2
sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 2 + size []
            , 2
            ]

-- Task A2

-- displays the hand if used with putStr()
display :: Hand -> String
display hand =  unlines([displayCard (hand !! k) |k <- [0.. size hand-1]])

--putStr method
displayTest :: Hand -> IO()
displayTest hand = putStr(display hand)

-- converts Card to String
displayCard :: Card -> String
displayCard (Card (Numeric rank) suit) = show rank ++ " of " ++ show suit
displayCard card = show (rank card) ++ " of " ++ show (suit card)


value :: Hand -> Int
value hand
  | sum' hand > 21 = sum' hand - (10*numberOfAces hand)
  | otherwise = sum' hand

sum' :: Hand -> Int
sum' [] = 0
sum' (card:hand) = valueCard card + sum' hand

valueRank :: Rank -> Int 
valueRank = undefined 


valueCard :: Card -> Int
valueCard (Card (Numeric rank) suit) = rank
valueCard (Card Ace suit) = 11
valueCard card = 10

numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces ((Card Ace suit): hand) = numberOfAces hand + 1
numberOfAces (card:hand) = numberOfAces hand 

gameOver :: Hand -> Bool 
gameOver hand
  | value hand > 21 = True
  | otherwise = False 


winner :: Hand -> Hand -> Player
winner guest bank
  | gameOver guest == False && gameOver bank == False && (value guest) > (value bank) = Guest 
  | gameOver guest == False = Guest
  | otherwise = Bank
