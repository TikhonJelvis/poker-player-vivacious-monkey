{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
module Player where

import           Control.Lens

import           Data.Aeson           as JS
import           Data.Aeson.Lens
import qualified Data.List            as List
import           Data.Maybe           (fromMaybe, mapMaybe)
import           Data.Text            (unpack)

import qualified Data.ByteString.Lazy as LBS

import           Text.Printf          (printf)


version :: String
version = "Default Haskell folding player"

betRequest :: JS.Object -> IO Int
betRequest obj = case fromJSON (Object obj) of
          Success gs -> let bet = tryCheck (multiple strats) gs in
                        printf "Betting %d\n." bet >> return bet
          Error msg  -> putStrLn msg >> return 0

strats = [ seeFlush
         , pocketPair
         , seePair
         , highBeforeFlop
         -- , highCard
         , stick
         ]

showdown :: JS.Object -> IO ()
showdown gameState = return ()

getUs :: GameState -> Player
getUs GameState { players } =
  let Just us = List.find (\ x -> name x == "Vivacious Monkey") players in us

multiple :: [GameState -> Maybe Int] -> GameState -> Int
multiple strats gs = case mapMaybe ($ gs) strats of
  (res : _) -> res
  []        -> 0

tryCheck :: (GameState -> Int) -> GameState -> Int
tryCheck f gs@(GameState { current_buy_in })
  | current_buy_in == 0 = 0
  | otherwise = f gs

allIn :: GameState -> Int
allIn gs = stack (getUs gs)

-- | Bets all in if we have a pair, half otherwise?
pocketPair :: GameState -> Maybe Int
pocketPair gs = [ allIn gs | rank c_1 == rank c_2 ]
  where [c_1, c_2] = hole_cards (getUs gs)
        quality = floor $ fromIntegral (allIn gs `div` 2) * rankScore (rank c_1)

highCard :: GameState -> Maybe Int
highCard gs = [ stack (getUs gs) `div` 4 | high c_1 || high c_2 ]
  where [c_1, c_2] = hole_cards (getUs gs)
        high Card { rank } = rank `elem` [A, K, Q, J]

stick :: GameState -> Maybe Int
stick gs@GameState { current_buy_in } = [ toBet | toBet < stack `div` 6 ]
  where Player { bet, stack } = getUs gs
        toBet = current_buy_in - bet

highBeforeFlop :: GameState -> Maybe Int
highBeforeFlop gs@GameState { current_buy_in, community_cards, minimum_raise } =
  [ toBet | highCard && beforeFlop ]
  where toBet = min (current_buy_in - bet) (stack `div` 4)
        Player { bet, stack, hole_cards } = getUs gs
        highCard = any (\ x -> rank x `elem` [J, Q, K, A]) hole_cards
        beforeFlop = community_cards == []

seePair :: GameState -> Maybe Int
seePair gs@GameState { community_cards } = [ allIn gs | havePair ]
  where havePair = List.any (\ (a, b) -> rank a == rank b) allPairs
        allPairs = (,) <$> hole_cards us <*> community_cards 
        us@Player { hole_cards = [c_1, c_2] } = getUs gs
        quality = floor $ fromIntegral (allIn gs `div` 2) * rankScore (rank c_1)

seeFlush :: GameState -> Maybe Int
seeFlush gs@GameState { community_cards } =
  [ allIn gs | haveFlush ]
  where haveFlush = suit c_1 == suit c_2 && length sameSuit >= 3
        sameSuit = filter ((== suit c_1) . suit) community_cards
        us@Player { hole_cards = [c_1, c_2] } = getUs gs



data GameState = GameState { players         :: [Player]
                           , round           :: Int
                           , pot             :: Int
                           , small_blind     :: Int
                           , community_cards :: [Card]
                           , current_buy_in  :: Int
                           , minimum_raise   :: Int
                           } deriving (Show, Eq)

instance FromJSON GameState where
  parseJSON (Object v) = GameState <$> v .: "players"
                                   <*> v .: "round"
                                   <*> v .: "pot"
                                   <*> v .: "small_blind"
                                   <*> v .: "community_cards"
                                   <*> v .: "current_buy_in"
                                   <*> minRaise
    where minRaise = fromMaybe 0 <$> v .:? "minimum_raise"
  parseJSON _          = error "Not a valid game!"

data Player = Player { name       :: String
                     , status     :: String
                     , stack      :: Int
                     , hole_cards :: [Card]
                     , bet        :: Int
                     } deriving (Show, Eq)

instance FromJSON Player where
  parseJSON (Object v) = Player <$> v .: "name"
                                <*> v .: "status"
                                <*> v .: "stack"
                                <*> hole_cards
                                <*> v .: "bet"
    where hole_cards =  fromMaybe [] <$> (v .:? "hole_cards")
  parseJSON _          = error "Not a valid player!"

data Card = Card { rank :: Rank
                 , suit :: Suit
                 } deriving (Show, Eq)

instance FromJSON Card where
  parseJSON (Object v) = Card <$> v .: "rank" <*> v .: "suit"
  parseJSON _          = error "Not a valid card!"

data Rank = N Int | J | Q | K | A deriving (Show, Eq, Ord)

rankScore :: Rank -> Double
rankScore (N n) = (fromIntegral n - 1) / 9
rankScore _     = 1

data Suit = S | H | D | C deriving (Show, Eq, Ord)

instance FromJSON Rank where
  parseJSON (String "J") = return J
  parseJSON (String "Q") = return Q
  parseJSON (String "K") = return K
  parseJSON (String "A") = return A
  parseJSON (String n)   = return . N . read $ unpack n
  parseJSON _ = error "Not a valid rank!"

instance FromJSON Suit where
  parseJSON (String "spades")   = return S
  parseJSON (String "hearts")   = return H
  parseJSON (String "diamonds") = return D
  parseJSON (String "clubs")    = return C
  parseJSON _ = error "Not a valid suit!"
