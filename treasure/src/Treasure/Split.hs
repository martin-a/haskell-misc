module Treasure.Split where

import Data.List
import Control.Monad
import Data.Maybe

-- |find all possible splits for the specified treasure
findAllSplits :: [Int] -> [[[Int]]]
findAllSplits gems = catMaybes $ fmap (\n-> findSplitsForHunters n gems) [2..(length $ gems)]

-- |find all splits for the specified nb of hunters
findSplitsForHunters :: Int -> [Int] -> Maybe [[Int]]
findSplitsForHunters huntersNb gems
    | (rem (sum gems) $ huntersNb) /= 0 = Nothing
    | otherwise = findSplitsForGemsPerHunter (quot (sum gems) huntersNb) $ reverse $ sort gems

-- |find all splits for the specified gems/hunters
findSplitsForGemsPerHunter :: Int -> [Int] -> Maybe [[Int]]
findSplitsForGemsPerHunter nbPerHunter gems
 | sum gems == nbPerHunter = return [gems]
 | sum gems < nbPerHunter = Nothing
 | otherwise = do
   a <- headCombiSumming nbPerHunter gems
   fromMaybe Nothing $ find isJust $ fmap (\xs -> liftM (xs:) $ findSplitsForGemsPerHunter nbPerHunter (gems \\ xs)) a

-- |find all combinations that include the head of the list and that sum up to the specified number
headCombiSumming :: Int -> [Int] -> Maybe [[Int]]
headCombiSumming nbPerHunter (x:xs)
    | x > nbPerHunter = Nothing
headCombiSumming nbPerHunter (x:xs) = do
    r <- combiSumming (nbPerHunter-x) xs
    return $ fmap (x:) r


-- |find all combinations that sum up to the specified number
combiSumming :: Int -> [Int] -> Maybe [[Int]]
combiSumming nbPerHunter _
    | nbPerHunter < 0 = Nothing
combiSumming 0 [] = Just [[]]
combiSumming _ [] = Nothing
combiSumming nbPerHunter (x:xs) =
    let res = concat $ catMaybes $ [(liftM ( \xs -> fmap (x:) xs) $ combiSumming (nbPerHunter - x) xs) , (combiSumming nbPerHunter xs)]
    in if res == [] then Nothing else Just res