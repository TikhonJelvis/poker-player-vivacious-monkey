{-# LANGUAGE NamedFieldPuns #-}
module Player where

import Control.Lens

import Data.Aeson as JS
import Data.Aeson.Lens
import qualified Data.List as List
import Data.Text (unpack)

import qualified Data.ByteString.Lazy as LBS


version :: String
version = "Default Haskell folding player"

betRequest :: JS.Object -> IO Int
betRequest obj = return current_buy_in
  where Success (GameState { current_buy_in }) = fromJSON (Object obj)

showdown :: JS.Object -> IO ()
showdown gameState = return ()

data GameState = GameState { players :: [Player]
                           , round :: Int
                           , pot :: Int
                           , small_blind :: Int
                           , community_cards :: [Card]
                           , current_buy_in :: Int
                           } deriving (Show, Eq)

instance FromJSON GameState where
  parseJSON (Object v) = GameState <$> v .: "players"
                                   <*> v .: "round"
                                   <*> v .: "pot"
                                   <*> v .: "small_blind"
                                   <*> v .: "community_cards"
                                   <*> v .: "current_buy_in"
  parseJSON _          = error "Not a valid game!"

data Player = Player { name :: String
                     , status :: String
                     , stack :: Int
                     , hole_cards :: [Card]
                     } deriving (Show, Eq)

instance FromJSON Player where
  parseJSON (Object v) = Player <$> v .: "name" <*> v .: "status" <*> v .: "stack" <*> v .: "hole_cards"
  parseJSON _          = error "Not a valid player!"

data Card = Card { rank :: Rank
                 , suit :: Suit
                 } deriving (Show, Eq)

instance FromJSON Card where
  parseJSON (Object v) = Card <$> v .: "rank" <*> v .: "suit"
  parseJSON _          = error "Not a valid card!"

data Rank = N Int | K | Q | J | A deriving (Show, Eq)

data Suit = S | H | D | C deriving (Show, Eq)

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
