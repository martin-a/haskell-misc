module Main where

import Data.List
import Control.Monad
import Data.Maybe
import Treasure.Split

main = forever $ do
    putStrLn $ "Input a treasure (Eg: [1,4,3]):"
    input <- getLine
    let treasure = (read input :: [Int])
    let res = findAllSplits treasure
    putStrLn $ "Here are the splits for "++ (show treasure)
    forM res ( \r-> do
        putStrLn   $  (show $ length r) ++ " hunters:" ++ show r)
