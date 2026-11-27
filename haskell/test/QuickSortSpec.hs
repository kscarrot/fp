module QuickSortSpec where

import Test.Hspec
import QuickSort

spec :: Spec
spec = do
    describe "QuickSort" $ do
        it "应该对空列表返回空列表" $ do
            quicksort ([] :: [Int]) `shouldBe` ([] :: [Int])
            quicksort' ([] :: [Int]) `shouldBe` ([] :: [Int])
            三路快速排序 ([] :: [Int]) `shouldBe` ([] :: [Int])
            quicksortFoldr ([] :: [Int]) `shouldBe` ([] :: [Int])
        
        it "应该对单元素列表返回相同列表" $ do
            quicksort ([1] :: [Int]) `shouldBe` ([1] :: [Int])
            quicksort' ([1] :: [Int]) `shouldBe` ([1] :: [Int])
            三路快速排序 ([1] :: [Int]) `shouldBe` ([1] :: [Int])
            quicksortFoldr ([1] :: [Int]) `shouldBe` ([1] :: [Int])
        
        it "应该对已排序列表返回相同列表" $ do
            quicksort ([1,2,3,4,5] :: [Integer]) `shouldBe` ([1,2,3,4,5] :: [Integer])
            quicksort' ([1,2,3,4,5] :: [Integer]) `shouldBe` ([1,2,3,4,5] :: [Integer])
            三路快速排序 ([1,2,3,4,5] :: [Integer]) `shouldBe` ([1,2,3,4,5] :: [Integer])
            quicksortFoldr ([1,2,3,4,5] :: [Integer]) `shouldBe` ([1,2,3,4,5] :: [Integer])
        
        it "应该对逆序列表正确排序" $ do
            quicksort ([5,4,3,2,1] :: [Integer]) `shouldBe` ([1,2,3,4,5] :: [Integer])
            quicksort' ([5,4,3,2,1] :: [Integer]) `shouldBe` ([1,2,3,4,5] :: [Integer])
            三路快速排序 ([5,4,3,2,1] :: [Integer]) `shouldBe` ([1,2,3,4,5] :: [Integer])
            quicksortFoldr ([5,4,3,2,1] :: [Integer]) `shouldBe` ([1,2,3,4,5] :: [Integer])
        
        it "应该对包含重复元素的列表正确排序" $ do
            quicksort ([3,1,4,1,5,9,2,6,5,3,5] :: [Integer]) `shouldBe` ([1,1,2,3,3,4,5,5,5,6,9] :: [Integer])
            quicksort' ([3,1,4,1,5,9,2,6,5,3,5] :: [Integer]) `shouldBe` ([1,1,2,3,3,4,5,5,5,6,9] :: [Integer])
            三路快速排序 ([3,1,4,1,5,9,2,6,5,3,5] :: [Integer]) `shouldBe` ([1,1,2,3,3,4,5,5,5,6,9] :: [Integer])
            quicksortFoldr ([3,1,4,1,5,9,2,6,5,3,5] :: [Integer]) `shouldBe` ([1,1,2,3,3,4,5,5,5,6,9] :: [Integer])
        
        it "应该对字符串列表正确排序" $ do
            quicksort ["banana", "apple", "cherry"] `shouldBe` ["apple", "banana", "cherry"]
            quicksort' ["banana", "apple", "cherry"] `shouldBe` ["apple", "banana", "cherry"]
            三路快速排序 ["banana", "apple", "cherry"] `shouldBe` ["apple", "banana", "cherry"]
            quicksortFoldr ["banana", "apple", "cherry"] `shouldBe` ["apple", "banana", "cherry"]
        
        it "所有版本应该产生相同结果" $ do
            let testList = ([3,1,4,1,5,9,2,6,5,3,5] :: [Integer])
            let sorted = quicksort testList
            quicksort' testList `shouldBe` sorted
            三路快速排序 testList `shouldBe` sorted
            quicksortFoldr testList `shouldBe` sorted 