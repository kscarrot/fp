module Main where

import Linear

main :: IO ()
main = do
  putStrLn "线性代数模块示例："
  putStrLn ""

  putStrLn "1. 标量运算示例："
  putStrLn "   1.1 数字相加："
  putStrLn $ "      1.5 + 2.5 = " ++ show (标量加 (Num 1.5) (Num 2.5))
  putStrLn $ "      5 + 5 = " ++ show (标量加 (Num 5) (Num 5))
  
  putStrLn "\n   1.2 字符串相加："
  putStrLn $ "      \"Hello\" + \"World\" = " ++ show (标量加 (Str "Hello") (Str "World"))
  
  putStrLn "\n   1.3 数字相乘："
  putStrLn $ "      1.5 * 2.0 = " ++ show (标量乘 (Num 1.5) (Num 2.0))
  
  putStrLn "\n   1.4 字符串相乘："
  putStrLn $ "      \"Hello\" * \"World\" = " ++ show (标量乘 (Str "Hello") (Str "World"))

  putStrLn "\n2. 向量运算示例："
  putStrLn "   2.1 数字向量相加："
  let v1 = [Num 1.0, Num 2.0, Num 3.0]
      v2 = [Num 4.0, Num 5.0, Num 6.0]
      vSum = 向量加 v1 v2
  putStrLn $ "      [1,2,3] + [4,5,6] = " ++ show vSum
  
  putStrLn "\n   2.2 字符串向量相加："
  let sv1 = [Str "Hello", Str "World"]
      sv2 = [Str " ", Str "!"]
      svSum = 向量加 sv1 sv2
  putStrLn $ "      [\"Hello\",\"World\"] + [\" \",\"!\"] = " ++ show svSum
  
  putStrLn "\n   2.3 向量点积："
  putStrLn $ "      [1,2,3] · [4,5,6] = " ++ show (向量乘 v1 v2)
  
  putStrLn "\n   2.4 字符串向量相乘："
  let sv3 = [Str "x_1", Str "y_1", Str "z_1"]
      sv4 = [Str "x_2", Str "y_2", Str "z_2"]
  putStrLn $ "      [\"x_1\",\"y_1\",\"z_1\"] · [\"x_2\",\"y_2\",\"z_2\"] = " ++ show (向量乘 sv3 sv4)
