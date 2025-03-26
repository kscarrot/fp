module LinearSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Linear

spec :: Spec
spec = do
  describe "标量运算" $ do
    describe "标量加" $ do
      it "浮点数相加应该返回正确的和" $ do
        Num 1.5 `标量加` Num 2.5 `shouldBe` Num 4.0

      it "浮点数相加兼容整形 返回浮点数" $ do
        Num 5.0 `标量加` Num 5 `shouldBe` Num 10.0

      it "整形数相加应该返回正确的浮点数结果" $ do
        Num 1 `标量加` Num 2 `shouldBe` Num 3.0

      it "字符串相加应该返回正确的连接结果" $ do
        Str "Hello" `标量加` Str "World" `shouldBe` Str "Hello+World"

      it "字符串相加空串处理" $ do
        Str "" `标量加` Str "World" `shouldBe` Str "World"
        Str "" `标量加` Str "" `shouldBe` Str ""

      it "不同类型相加应该抛出异常" $ do
        evaluate (Num 1.5 `标量加` Str "World") `shouldThrow` anyException

    describe "标量乘" $ do
      it "浮点数相乘应该返回正确的积" $ do
        Num 1.5 `标量乘` Num 2.0 `shouldBe` Num 3.0

      it "字符串相乘应该返回正确的连接结果" $ do
        Str "Hello" `标量乘` Str "World" `shouldBe` Str "HelloWorld"

      it "空字符串相乘应该返回空字符串" $ do
        Str "" `标量乘` Str "World" `shouldBe` Str ""

      it "不同类型相乘应该抛出异常" $ do
        evaluate (Num 1.5 `标量乘` Str "World") `shouldThrow` anyException

  describe "向量运算" $ do
    describe "向量加" $ do
      it "向量相加应该返回正确的和" $ do
        let x = [Num 1.0, Num 2.0, Num 3.0]
            y = [Num 4.0, Num 5.0, Num 6.0]
            result = x `向量加` y
        result `shouldBe` [Num 5.0, Num 7.0, Num 9.0]

      it "不同长度向量相加应该抛出异常" $ do
        let x = [Num 1.0, Num 2.0]
            y = [Num 3.0, Num 4.0, Num 5.0]
        evaluate (x `向量加` y) `shouldThrow` anyException

      it "字符串向量相加应该返回正确的连接结果" $ do
        let x = [Str "Hello", Str "World"]
            y = [Str " ", Str "!"]
            result = x `向量加` y
        result `shouldBe` [Str "Hello+ ", Str "World+!"]

    describe "向量乘" $ do
      it "向量相乘应该返回正确的元素积" $ do
        let x = [Num 1.0, Num 2.0, Num 3.0]
            y = [Num 4.0, Num 5.0, Num 6.0]
            -- 1⋅4+2⋅5+3⋅6 = 32
        x `向量乘` y `shouldBe` Num 32.0

      it "字符串向量相乘应该返回正确的连接结果" $ do
        let x = [Str "x_1", Str "y_1", Str "z_1"]
            y = [Str "x_2", Str "y_2", Str "z_2"]
        x `向量乘` y `shouldBe` Str "x_1x_2+y_1y_2+z_1z_2"

      it "不同长度向量相乘应该抛出异常" $ do
        let x = [Num 1.0, Num 2.0]
            y = [Num 3.0, Num 4.0, Num 5.0]
        evaluate (x `向量乘` y) `shouldThrow` anyException 