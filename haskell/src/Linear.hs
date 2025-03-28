module Linear
  ( Scalar (..),
    Vector,
    Matrix,
    标量加,
    标量乘,
    向量加,
    向量乘,
    标量乘向量,
    转置,
    矩阵乘,
    标量乘矩阵,
    矩阵加,
    向量乘矩阵,
  )
where

data Scalar
  = Str String
  | Num Double
  deriving (Show, Eq)

type Vector = [Scalar]

type Matrix = [Vector]

标量加 :: Scalar -> Scalar -> Scalar
标量加 (Num x) (Num y) = Num (x + y)
标量加 (Str "") (Str x) = Str x
标量加 (Str x) (Str "") = Str x
标量加 (Str x) (Str y) = Str (x ++ "+" ++ y)
标量加 _ _ = error "不支持的类型"

标量乘 :: Scalar -> Scalar -> Scalar
标量乘 (Num x) (Num y) = Num (x * y)
标量乘 (Str "") (Str _) = Str ""
标量乘 (Str _) (Str "") = Str ""
标量乘 (Str x) (Str y) = Str (x ++ y)
标量乘 _ _ = error "不支持的类型"

向量加 :: Vector -> Vector -> Vector
向量加 = zipWith 标量加

向量乘 :: Vector -> Vector -> Scalar
向量乘 x y = foldr1 标量加 $ zipWith 标量乘 x y

-- 标量乘向量
标量乘向量 :: Scalar -> Vector -> Vector
标量乘向量 s = map (标量乘 s)

-- 矩阵转置
转置 :: Matrix -> Matrix
转置 [] = []
转置 ([] : _) = []
转置 m = map head m : 转置 (map tail m)

-- 矩阵乘法
矩阵乘 :: Matrix -> Matrix -> Matrix
矩阵乘 a b = [[ 行 `向量乘`列 | 列 <- 转置 b] | 行 <- a]

-- 标量乘矩阵
标量乘矩阵 :: Scalar -> Matrix -> Matrix
标量乘矩阵 s = map (标量乘向量 s)

-- 矩阵加法
矩阵加 :: Matrix -> Matrix -> Matrix
矩阵加 = zipWith 向量加

-- 向量乘矩阵
向量乘矩阵 :: Vector -> Matrix -> Vector
向量乘矩阵 向量 m = [向量  `向量乘`  列 | 列 <- 转置 m]
