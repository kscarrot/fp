module LinearSpec (spec) where

import Linear
import Test.Hspec

spec :: Spec
spec = do
  describe "加法运算" $ do
    it "标量加" $ do
      标量加 1 2 `shouldBe` 3
    it "向量加" $ do
      向量加 [1, 2, 3] [4, 5, 6] `shouldBe` [5, 7, 9]
    it "矩阵加" $ do
      矩阵加 [[1, 2], [3, 4]] [[5, 6], [7, 8]] `shouldBe` [[6, 8], [10, 12]]

  describe "转置" $ do
    it "空矩阵转置" $ do
      转置 [] `shouldBe` []
    it "矩阵方阵转置" $ do
      转置 [[1, 2], [3, 4]] `shouldBe` [[1, 3], [2, 4]]
      转置 [[1, 0], [0, 1]] `shouldBe` [[1, 0], [0, 1]]
      转置 [[1, 0, 0], [0, 2, 0], [0, 0, 3]] `shouldBe` [[1, 0, 0], [0, 2, 0], [0, 0, 3]]
    it "矩阵非方阵转置" $ do
      转置 [[1, 2, 3], [4, 5, 6]] `shouldBe` [[1, 4], [2, 5], [3, 6]]
      转置 [[1, 2, 3]] `shouldBe` [[1], [2], [3]]

  describe "标量乘" $ do
    it "标量乘" $ do
      标量乘 2 3 `shouldBe` 6
    it "标量乘向量" $ do
      标量乘向量 2 [1, 2, 3] `shouldBe` [2, 4, 6]
    it "标量乘矩阵" $ do
      标量乘矩阵 2 [[1, 2], [3, 4]] `shouldBe` [[2, 4], [6, 8]]

  describe "向量乘" $ do
    it "向量乘" $ do
      向量乘 [1, 2, 3] [4, 5, 6] `shouldBe` 32
    it "向量乘矩阵" $ do
      向量乘矩阵 [1, 2, 3] [[4, 5], [6, 7], [8, 9]] `shouldBe` [40, 46]
    it "矩阵乘向量" $ do
      矩阵乘向量 [[1, 2], [3, 4]] [5, 6] `shouldBe` [17, 39]

  describe "矩阵乘" $ do
    it "矩阵乘" $ do
      矩阵乘 [[1, 2], [3, 4]] [[5, 6], [7, 8]] `shouldBe` [[19, 22], [43, 50]]
    it "非方阵矩阵乘" $ do
      矩阵乘 [[1, 2, 3], [4, 5, 6]] [[7, 8], [9, 10], [11, 12]] `shouldBe` [[58, 64], [139, 154]]
    it "矩阵乘PointFree" $ do
      矩阵乘PointFree [[1, 2], [3, 4]] [[5, 6], [7, 8]] `shouldBe` [[19, 22], [43, 50]]

  describe "零矩阵" $ do
    it "零矩阵" $ do
      零矩阵 2 3 `shouldBe` [[0, 0, 0], [0, 0, 0]]
      零矩阵 1 1 `shouldBe` [[0]]
      零矩阵 3 2 `shouldBe` [[0, 0], [0, 0], [0, 0]]
      矩阵乘 (零矩阵 3 2) [[1, 2], [3, 4], [5, 6]] `shouldBe` 零矩阵 3 2
  describe "单位矩阵" $ do
    it "单位矩阵" $ do
      单位矩阵 1 `shouldBe` [[1]]
      单位矩阵 2 `shouldBe` [[1, 0], [0, 1]]
      单位矩阵 3 `shouldBe` [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
      矩阵乘 (单位矩阵 2) [[1, 2], [3, 4]] `shouldBe` [[1, 2], [3, 4]]
      矩阵乘 (单位矩阵 3) [[1, 2], [3, 4], [5, 6]] `shouldBe` [[1, 2], [3, 4], [5, 6]]