module Player where

import Control.Lens

import Data.Aeson as JS
import Data.Aeson.Lens
import qualified Data.List as List


version :: String
version = "Default Haskell folding player"

betRequest :: JS.Object -> IO Int
betRequest gameState = return 1000

showdown :: JS.Object -> IO ()
showdown gameState = return ()

data GameState = GameState {players :: [Player]
                           , round :: Int
                           , pot :: Int
                           , small_blind :: Int
                           , big_blind :: Int
                           , community_cards :: [Card]
                           }

data Player = Player { name :: String
                     , status :: String
                     , stack :: Int
                     }

data Card = Card { rank :: Rank
                 , suid :: Suit
                 }

data Rank = N Int | K | Q | J | A deriving (Show, Eq)

data Suit = S | H | D | C deriving (Show, Eq)

instance FromJSON Rank where
  parseJSON (String "J") = return J
  parseJSON (String "Q") = return Q
  parseJSON (String "K") = return K
  parseJSON (String "A") = return A
  parseJSON (String n)   = undefined
  parseJSON _ = error "Not a valid rank!" 

instance FromJSON Suit where
  parseJSON (String "spades")   = return S
  parseJSON (String "hearts")   = return H
  parseJSON (String "diamonds") = return D
  parseJSON (String "clubs")    = return C
  parseJSON _ = error "Not a valid suit!" 
