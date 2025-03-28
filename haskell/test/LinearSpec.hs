module LinearSpec (spec) where

import Linear
import Test.Hspec

spec :: Spec
spec = do
  describe "标量运算" $ do
    it "数字加法" $
      Num 1.0 `标量加` Num 2.0 `shouldBe` Num 3.0

    it "字符串加法" $
      Str "hello" `标量加` Str "world" `shouldBe` Str "hello+world"

    it "空字符串加法" $ do
      Str "" `标量加` Str "test" `shouldBe` Str "test"
      Str "test" `标量加` Str "" `shouldBe` Str "test"

    it "数字乘法" $
      Num 2.0 `标量乘` Num 3.0 `shouldBe` Num 6.0

    it "字符串乘法" $
      Str "hello" `标量乘` Str "world" `shouldBe` Str "helloworld"

    it "空字符串乘法" $ do
      Str "" `标量乘` Str "test" `shouldBe` Str ""
      Str "test" `标量乘` Str "" `shouldBe` Str ""

  describe "向量运算" $ do
    let v1 = [Num 1.0, Num 2.0]
    let v2 = [Num 3.0, Num 4.0]
    let sv1 = [Str "a", Str "b"]
    let sv2 = [Str "c", Str "d"]

    it "数字向量加法" $
      v1 `向量加` v2 `shouldBe` [Num 4.0, Num 6.0]

    it "字符串向量加法" $
      sv1 `向量加` sv2 `shouldBe` [Str "a+c", Str "b+d"]

    it "数字向量点积" $
      v1 `向量乘` v2 `shouldBe` Num 11.0

    it "字符串向量点积" $
      sv1 `向量乘` sv2 `shouldBe` Str "ac+bd"

    it "标量乘数字向量" $
      Num 2.0 `标量乘向量` v1 `shouldBe` [Num 2.0, Num 4.0]

    it "标量乘字符串向量" $
      Str "x" `标量乘向量` sv1 `shouldBe` [Str "xa", Str "xb"]

  describe "矩阵运算" $ do
    let m1 = [[Num 1.0, Num 2.0], [Num 3.0, Num 4.0]]
    let m2 = [[Num 5.0, Num 6.0], [Num 7.0, Num 8.0]]
    let v1 = [Num 1.0, Num 2.0]

    it "矩阵转置" $
      转置 m1 `shouldBe` [[Num 1.0, Num 3.0], [Num 2.0, Num 4.0]]

    it "矩阵乘法" $
      m1 `矩阵乘` m2 `shouldBe` [[Num 19.0, Num 22.0], [Num 43.0, Num 50.0]]

    it "标量乘矩阵" $
      Num 2.0 `标量乘矩阵` m1 `shouldBe` [[Num 2.0, Num 4.0], [Num 6.0, Num 8.0]]

    it "矩阵加法" $
      m1 `矩阵加` m2 `shouldBe` [[Num 6.0, Num 8.0], [Num 10.0, Num 12.0]]

    it "向量乘矩阵" $
      v1 `向量乘矩阵` m1 `shouldBe` [Num 7.0, Num 10.0]

  describe "空矩阵和向量处理" $ do
    it "空矩阵转置" $
      转置 [] `shouldBe` []

  describe "字符串矩阵乘法" $ do
    let m1 = [[Str "a", Str "b"], [Str "c", Str "d"]]
    let m2 = [[Str "e", Str "f"], [Str "g", Str "h"]]

    it "字符串矩阵乘法" $
      m1 `矩阵乘` m2 `shouldBe` [[Str "ae+bg", Str "af+bh"], [Str "ce+dg", Str "cf+dh"]]
