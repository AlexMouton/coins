module Main where

import Data.List
import Prelude

data Coin = Coin Int [Coin]
  deriving Show

coins = [25, 10, 5, 1]

change :: Int -> [Coin]
change i = cc i i

cc :: Int -> Int -> [Coin]
cc b n = (coin n) <$> (filter (fit n b) coins)
  where
  fit n b a = (n >= a) && (a <= b)
  coin n c = Coin c (cc c (n - c))

main = do
  putStrLn $ show $ change 5
 

