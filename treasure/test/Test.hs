import Data.List
import Data.Foldable
import Control.Monad
import Data.Monoid
import Data.Maybe
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Treasure.Split


main = $(defaultMainGenerator)


case_testTreasure1 :: Assertion
case_testTreasure1 =  findAllSplits [27,7,20] @?=  [[[27],[20,7]]]


case_testTreasure2 :: Assertion
case_testTreasure2 =  findAllSplits [6,3,2,4,1] @?=  [[[6, 2], [4,3,1]]]

case_testTreasure3 :: Assertion
case_testTreasure3 =  findAllSplits [3,2,7,7,14,5,3,4,9,2] @?=  [[[14,9,5],[7,7,4,3,3,2,2]],[[14],[9,5],[7,7],[4,3,3,2,2]]]


data SplitableTreasure = SplitableTreasure {
    treasure :: [Int],
    hunters :: Int,
    gemsPerHunter :: Int
} deriving (Show)

instance Arbitrary SplitableTreasure where
  arbitrary = do
   hunters <- choose (2,10)
   gemsPerHunter <- choose (1,100)
   let trsr = liftM (take hunters) (infiniteListOf $ generateListSumming gemsPerHunter)
   liftM (\x -> SplitableTreasure (concat x) hunters gemsPerHunter) trsr

generateListSumming :: Int -> Gen [Int]
generateListSumming 0 = elements [[]]
generateListSumming sum = do
    chosen <-  liftM (: []) $ choose (1,sum)
    liftM (head chosen:) $ generateListSumming $ sum - head chosen


prop_splitableTreasureContainsExpectedSplit tr =
    let splits = findAllSplits $ treasure tr
    in isJust $ do
    index <- findIndex (\xs -> length xs == hunters tr) $ splits
    foldlM (\ignore xs -> if (sum xs == gemsPerHunter tr) then Just [] else Nothing) [] $ splits!!index
  where types = tr::SplitableTreasure